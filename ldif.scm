 (define comment-line
   (irregex '(seq "#" (* any))))

 (define white-line
   (irregex '(seq (* whitespace))))

 (define continue-line
   (irregex '(seq " " (=> value (+ any)))))

 (define (merge-continuation-lines shift! peek ln1)
   (let loop ((v ln1))
     (shift!)
     (let ((ln (peek)))
       (cond
	((and (string? ln) (irregex-match continue-line ln)) =>
	 (lambda (m) (loop (string-append v (irregex-match-substring m 'value)))))
	(else v)))))

 (define (make-input p)
   (let ((pending #f))
     (define (shift)
       (let ((ln (read-line p)))
	 (cond
	  ((eof-object? ln) ln)
	  ((irregex-match comment-line ln)
	   (merge-continuation-lines shift! peek ln)
	   (skip-empty-lines shift! peek))
	  (else ln))))
     (define shift!
       (lambda ()
	(if (eof-object? pending) pending
	    (begin
	      (set! pending #f)
	      #f))))
     (define (peek)
       (cond
	((not pending) (set! pending (shift)) pending)
	(else pending)))
     (values shift! peek)))

 (define (skip-empty-lines shift! peek)
   (let loop ((ln (peek)))
     (if (and (string? ln) (irregex-match white-line ln)) (loop (begin (shift!) (peek))) ln)))

 (define (source-empty? s i) (<= (string-length s) i))
 
 (define skip-white
   (let ((rx '(seq bol (* whitespace))))
     (lambda (s i)
       (irregex-match-end-index (irregex-search rx s i)))))

 (define key-line
   ;; FIXME: double check key and value for actually allowed values
   (irregex '(seq (=> key (+ (~ (";:"))))
		  (=> options (* (seq ";" (+ (or alphanumeric "-")))))
		  ":"
		  (=> kind (? (or "<" ":")))
		  (* whitespace)
		  (=> value (+ any)))))
 (define (getkv value-converter shift! peek)
   (let ((ln (peek)))
     (cond
      ((eof-object? ln) (values #f #f))
      ((string-null? ln) (values #f #f))
      ((irregex-match key-line ln) =>
       (lambda (m)
	 (let ((k (let ((k (irregex-match-substring m 'key))
			(o (string-split (irregex-match-substring m 'options) ";")))
		    ;;(identity #;string-downcase k)
		    (make-ldif-attdesc k o)))
	       (kind (let ((ks (irregex-match-substring m 'kind)))
		       (cond
			((fx= (string-length ks) 0) identity)
			((eqv? (string-ref ks 0) #\:) base64-decode)
			((eqv? (string-ref ks 0) #\<) uri-reference)
			(else identity)))))
	   ;; value-converter is a horrible HACK to allow parsing of
	   ;; samba's unprocessed ldif files.
	   (if value-converter
	       (set! kind (let ((kind kind)) (lambda (v) (let ((x (value-converter v 0))) (if x (car x) (kind v)))))))
	   ;; The actual parser:
	   (let loop ((v (irregex-match-substring m 'value)))
	     (shift!)
	     (let ((ln (peek)))
	       (cond
		((eof-object? ln) (values k (kind v)))
		((irregex-match continue-line ln) =>
		 (lambda (m) (loop (string-append v (irregex-match-substring m 'value)))))
		(else (values k (kind v)))))))))
      (else (error "illegal line" ln)))))

 (define rfc4514-read-=
   (let ((attval (irregex '(seq bos (* white) "=" (* white)))))
     (lambda (s i)
       (or (and-let* ((m (irregex-search attval s i))) (irregex-match-end-index m))
	   (error "rfc4514: expected '='" s i)))))
 (define rfc4514-read-att-value
   (let ((stringchar (irregex '(seq bos (+ (- any (",=+<>#;\\\""))))))
	 (pairstring (irregex '(seq bos "\\"
				    (or (seq xdigit xdigit)
					(",=+<>#;\\\"")))))
	 (hexstring (irregex '(seq bos "#" (+ (seq xdigit xdigit))))))
     (lambda (s i)
       (let loop ((l #f) (r '()) (i i))
	 (cond
	  ((irregex-search hexstring s i) =>
	   (lambda (m)
	     (cons
	      (let* ((ins (substring (irregex-match-substring m 0) 1))
		     (os (make-string (quotient (string-length ins) 2))))
		(do ((i 0 (+ i 2)))
		    ((= i (string-length os)) os)
		  (string-set! os i (integer->char (string->number (substring ins i (+ i 2)) 16)))))
	      (irregex-match-end-index m))))
	   ((irregex-search stringchar s i) =>
	    (lambda (m)
	      (let ((p (irregex-match-substring m 0)))
		(loop p (cons p r) (irregex-match-end-index m)))))
	   ((irregex-search pairstring s i) =>
	    (lambda (m)
	      (let ((elem (irregex-match-substring m 0)))
		(loop
		 #f
		 (cons
		  (if (= (string-length elem) 2)
		      (substring elem 1 2)
		      (string (integer->char (string->number (substring elem 1 3) 16))))
		  r)
		 (irregex-match-end-index m)))))
	   (else
	    (cons
	     (if (and l (eq? l (car r)))
		 (apply string-append (reverse (cons (string-trim-right (car r)) (cdr r))))
		 (apply string-append (reverse r)))
	     i)))))))
 (define rfc4514-read-att-type
   (let ((keym (irregex '(seq bos ($ (seq alphabetic (* (or alphanumeric "-")))))))
	 (oid-rx (irregex '(seq bos ($ (seq (+ num) (* (seq "." (+ num)))))))))
     (lambda (s i)
       (cond
	((irregex-search keym s i) =>
	 (lambda (m) (cons (irregex-match-substring m 1) (irregex-match-end-index m))))
	((irregex-search oid-rx s i) =>
	 (lambda (m) (cons (irregex-match-substring m 1) (irregex-match-end-index m))))
	;; TBD: eventually replace this end/err handling with simply returning #f
	;; ((or (source-empty? s i) (let ((c (string-ref s i))) (or (eqv? c #\,) (eqv? c #\+)))) #f)
	(else #f #; (error "rfc4514: illegal attribute spec" s i))))))
 (define (rfc4514-read-atttype-and-value s i)
   (and-let*
    ((k (rfc4514-read-att-type s (skip-white s i)))
     (assig (rfc4514-read-= s (cdr k)))
     (v (rfc4514-read-att-value s assig)))
    (cons
     (rfc4514-kv (car k) (car v))
     (cdr v))))
 (define rfc4514-read-plus
   (let ((rx (irregex '(seq bos (* white) "+" (* white)))))
     (lambda (s i)
       (and-let* ((m (irregex-search rx s i))) (irregex-match-end-index m)))))
 (define (rfc4514-read-name-component value-converter s i)
   (let ((x (rfc4514-read-atttype-and-value s i)))
     (if x
	 (let ((plus (rfc4514-read-plus s (cdr x))))
	   (if plus
	       (let ((y (rfc4514-read-name-component value-converter s plus)))
		 (cons (rfc4514-rdnsequence-cons (car x) (car y)) (cdr y)))
	       (cons
		(rfc4514-rdnsequence-cons
		 (car x) (rfc4514-rdnsequence))
		(cdr x))))
	 (if value-converter
	     (let ((x (value-converter s i)))
	       (if x x (cons (rfc4514-rdnsequence) i)))
	     (cons (rfc4514-rdnsequence) i)))))
 (define rfc4514-read-comma
   (let ((attval (irregex '(seq bos (* white) "," (* white)))))
     (lambda (s i)
       (and-let* ((m (irregex-search attval s i))) (irregex-match-end-index m)))))
 ;; `value-converter`, if given, must return a pair (result-ref next-index)
 (define (rfc4514-read s #!optional (i 0) #!key (value-converter #f))
   (let loop ((x (rfc4514-read-name-component value-converter s i)))
     (if x
	 (rfc4514-kv-cons
	  (car x)
	  (loop (let* ((i (cdr x)) (comma (rfc4514-read-comma s i)))
		  (cond
		   (comma (rfc4514-read-name-component value-converter s comma))
		   ((source-empty? s i) #f)
		   (else (error "rfc4514: unexpected char" s i))))))
	 (rfc4514-kv-empty))))

 (define (rfc4514-write-att-value s p)
   (do ((i 0 (add1 i)))
       ((eqv? i (string-length s)))
     (let ((c (string-ref s i)))
       (cond
	((string-index ",=+<>#;\\\"" c)
	 (display #\\ p) (display c p))
	((and (eqv? i 0) (eqv? c #\space))
	 (display "\\20" p))
	((and (eqv? i 0) (eqv? c #\#))
	 (display "\\23" p))
	((and (eqv? i (sub1 (string-length s))) (eqv? c #\space))
	 (display "\\20" p))
	((fx< (char->integer c) #x20)
	 (let ((cc (char->integer c)))
	   (display #\\ p)
	   (let ((c1 (quotient cc 16)))
	     (display (integer->char (fx+ (if (fx< c1 10) #x30 55) c1)) p))
	   (let ((c1 (remainder cc 16)))
	     (display (integer->char (fx+ (if (fx< c1 10) #x30 55) c1)) p))))
	(else (display c p))))))

 (define (rfc4514-write-a+v n p)
   (let ((p (if (vector? p) (vector-ref p 0) (begin (display #\+ p) p))))
     (display (rfc4514-kv-k n) p)
     (display "=" p)
     (rfc4514-write-att-value (rfc4514-kv-v n) p)
     p))

 (define (rfc4514-write-name-component n p)
   (let ((p (if (vector? p) (vector-ref p 0) (begin (display #\, p) p))))
     (rfc4514-rdnsequence-fold rfc4514-write-a+v (vector p) n)))

;; (((soso . gaga)))
 (define (rfc4514-write obj #!optional (port (current-output-port)))
   (rfc4514-rdnsequence-fold rfc4514-write-name-component (vector port) obj))

 (define (read-attributes value-converter shift! peek i)
   (receive
    (k v) (getkv value-converter shift! peek)
    (cond
     ((not k) i)
     (else (read-attributes value-converter shift! peek (ldif-attribute+ i k v))))))


 (define (read #!optional (port (current-input-port)) #!key (value-converter #f))
   (receive
    (shift! peek) (make-input port)
    (skip-empty-lines shift! peek)
    (receive
     (k v) (getkv #f shift! peek)
     (cond
      ((not k) (ldif-end))
      ((equal? k "dn")
       (let ((frame (read-attributes value-converter shift! peek (make-ldif-attribute-set))))
	 (make-ldif (rfc4514-read v 0 value-converter: value-converter) frame)))
      (else (error "ldif read: found" k v))))))


 (define safe-init-cs
   (char-set-delete char-set:ascii #\nul #\newline #\return #\space #\: #\<))

 (define safe-cs
   (char-set-delete char-set:ascii #\nul #\newline #\return))

 (define (safe-string? v)
   (or (fx= (string-length v) 0)
       (and (char-set-contains? safe-init-cs (string-ref v 0))
	    (string-every safe-cs v 1))))

 (define (ldif-write-a+v n v p)
   (let ((o (ldif-attdesc-options n)))
     (if o
	 (display (string-join (cons (ldif-attdesc-type n) o) ";") p)
	 (display n p)))
   (cond
    ((blob? v) (display ":: " p) (base64-encode v p))
    ((uri-reference? v) (display ":<" p) (display (uri->string v) p))
    ((safe-string? v) (display ": " p) (display v p))
    (else (display ":: " p) (base64-encode v p)))
   (newline p)
   p)

 (define (write ldif #!optional (port (current-output-port)))
   (assert (ldif? ldif))
   (display "dn: " port)
   (rfc4514-write (ldif-dn ldif))
   (newline port)
   (ldif-attributes-fold ldif-write-a+v port (ldif-attributes ldif))
   (newline port)
   ldif
   )
