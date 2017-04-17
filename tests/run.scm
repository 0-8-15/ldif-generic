(module
 rdn-sexp
 ((interface: ldif-rdn-constructor))
 (import scheme)
 (define (rdnsequence . args) args)
 (define (rdnsequence-cons a b) (cons a b))
 (define rdnsequence-empty? null?)
 (define (rdnsequence-fold kons nil s)
   (let loop ((i nil) (s s))
     (if (null? s) i (loop (kons (car s) i) (cdr s)))))
 (define (kv-empty) '())
 (define (kv-cons a b) (cons a b))
 (define kv? pair?)
 (define (kv k v) (cons k v))
 (define (kv-k x) (car x))
 (define (kv-v x) (cdr x))
 )

(use uri-common base64)

(module
 ldif-sexpr
 ((interface: ldif-constructor)
  )
 (import scheme matchable)
 (import uri-common)
 (import (prefix base64 b64:))
 (import chicken)

 (import extras)
 
 (define (base64-decode s) (string->blob (b64:base64-decode s)))
 (define (base64-encode b p) (b64:base64-encode (if (string? b) b (blob->string b)) p))

 (define (make-ldif name frame)
   `(LDIF ,name ,frame))

 (define (ldif? obj)
   (match obj (('LDIF name frame) #t) (_ #f)))

 (define (ldif-dn obj)
   (match obj (('LDIF name frame) name)))

 (define (ldif-attributes obj)
   (match obj (('LDIF name frame) frame)))

 (define (ldif-end) #!eof)

 (define (make-ldif-attdesc atttype options)
   (if (null? options) atttype (cons atttype options)))
 (define (ldif-attdesc? x)
   (or (string? x) (pair? x)))
 (define (ldif-attdesc-type x)
   (if (string? x) x (car x)))
 (define (ldif-attdesc-options x)
   (if (string? x) #f (cdr x)))

 (define (make-ldif-attribute-set) '())

 (define (ldif-attribute+ i k v)
   `((,k . ,v) . ,i))

 (define (ldif-attributes-fold kons nil atts)
   (let loop ((i nil) (s atts))
     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cdr a) i)) (cdr s)))))

 ) ;; end module ldif-model


;; Functor instanciation
(module ldif-parse2sexpr = (ldif-parser ldif-sexpr rdn-sexp))

;; Test
(import (prefix ldif-parse2sexpr ldif:))

(define tr #<<EOF
### Sowas
    aber auch

dn: soso=gaga
nm: Gi
 ck
url:<gacks://gick
nm;a2: Gacker



dn: DN=nm2
nm: Gacks
uss:: QUIKQ0Q=
nm: Blubber

EOF
  )

(display (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read)))))

(use ports)

(define dollarref
  (let ((rx (irregex '(: bos "${" ($ (+ alphanumeric)) "}"))))
    (lambda (s i)
      (and-let* ((m (irregex-search rx s i)))
		(cons `(VARREF ,(irregex-match-substring m 1)) (irregex-match-end-index m))))))


(display
 (with-input-from-file
    "/home/u/b1/qemu/tmp/sysroots/qemuarm/usr/share/samba/setup/provision.ldif"
  (lambda()
    (port-fold cons '() (lambda () (ldif:read (current-input-port) value-converter: dollarref)))
    #;(list (ldif:read) (ldif:read) (ldif:read)))))

(display (ldif:rfc4514-read "1.1.200.4 = a+2=X,b=3 , b=4" 0))
(display (ldif:rfc4514-write (ldif:rfc4514-read "1.1.200.4 = a\\0a+2=X,b=3 , b=4" 0)))
(newline)
(for-each ldif:write (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read)))))
(display "DONE\n")

(require-library llrb-tree)
(module
 ldif-tree
 ((interface: ldif-constructor)
  )
 (import scheme chicken)
 (import llrb-tree)
 (import uri-common base64)

 (define-record ldif dn attributes)

 (define (ldif-end) #!eof)

 (define (make-ldif-attdesc atttype options)
   (if (null? options) atttype (cons atttype options)))
 (define (ldif-attdesc? x)
   (or (string? x) (pair? x)))
 (define (ldif-attdesc-type x)
   (if (string? x) x (car x)))
 (define (ldif-attdesc-options x)
   (if (string? x) #f (cdr x)))

 (define make-ldif-attribute-set
   (let ((empty (empty-binding-set (make-llrb-treetype)#;(make-llrb-treetype #f string=? string>?))))
     (lambda () empty)))

 (define (ldif-attribute+ set k v)
   (binding-set-update set k (lambda (i) (cons v i)) (lambda () '())))

 (define (l2-fold k kons nil atts)
   (let loop ((i nil) (s atts))
     (if (null? s) i (loop (kons k (car s) i) (cdr s)))))
 (define (ldif-attributes-fold proc nil set)
   (binding-set-fold (lambda (k v i) (l2-fold k proc i v)) nil set))

 )

(module ldif-parse2tree = (ldif-parser ldif-tree rdn-sexp))

(import (prefix ldif-parse2tree ldif2:))

(display (with-input-from-string tr (lambda () (list (ldif2:read) (ldif2:read)))))

(use ports)
#;(display
 (with-input-from-file
    "/home/u/b1/qemu/tmp/sysroots/qemuarm/usr/share/samba/setup/provision.ldif"
  (lambda()
    (port-fold cons '() ldif2:read)
    #;(list (ldif2:read) (ldif2:read) (ldif2:read)))))
(display (ldif2:rfc4514-read "1.1.200.4 = a+2=X,b=3 , b=4" 0))
(display (ldif2:rfc4514-write (ldif2:rfc4514-read "1.1.200.4 = a\\0a+2=X,b=3 , b=4" 0)))
(newline)
(for-each ldif2:write (with-input-from-string tr (lambda () (list (ldif2:read) (ldif2:read)))))
(define-record-printe
