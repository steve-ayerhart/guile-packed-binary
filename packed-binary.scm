;; -*- geiser-scheme-implementation: guile -*-

(define-module (packed-binary)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs records syntactic)
  #:export (packed-binary-string?
            calculate-size
            pack
            pack-into
            unpack
            unpack-from
            write-packed
            read-packed))

;;; pack

(define (pack-char bv start char size signed? endianness)
  "Pack a character into the buffer at the specified position."
  (array-set! bv start (char->integer char)))

(define (pack-integer bv start int size signed? endianness)
  "Pack an integer into the buffer at the specified position."
  (if signed?
      (bytevector-sint-set! bv start int endianness size)
      (bytevector-uint-set! bv start int endianness size)))

(define (pack-real bv start real size signed? endianness)
  "Pack a floating-point number into the buffer at the specified position."
  (bytevector-ieee-double-set! bv start real endianness))

(define (pack-string bv start str size)
  "Pack a string into the buffer at the specified position."
  (let* ((bytes (string->utf8 str))
         (copy-size (min size (bytevector-length bytes))))
    (bytevector-copy! bv start bytes 0 copy-size)))

;;; unpack

(define (unpack-char bv start size signed? endianness)
  "Unpack a character from the specified position."
  (integer->char (array-ref bv start)))

(define (unpack-integer bv start size signed? endianness)
  "Unpack an integer from the specified position."
  (if signed?
      (bytevector-sint-ref bv start endianness size)
      (bytevector-uint-ref bv start endianness size)))

(define (unpack-real bv start size signed? endianness)
  "Unpack a floating point number from the specified position."
  (bytevector-ieee-double-ref bv start endianness))

(define (unpack-string bv start size)
  "Unpack a string from the specified position."
  (let* ((copy-size (min size (bytevector-length bv)))
         (new-bv (make-bytevector copy-size)))
    (bytevector-copy! bv start new-bv 0 copy-size)
    (utf8->string new-bv)))

(define-record-type packed-format
  (fields (mutable specifier)
          (mutable size)
          (mutable signed?)
          (mutable native-alignment)
          (mutable pack-handler)
          (mutable unpack-handler)))

;;Defines the pad byte packed binary format.

(define packed-format-x (make-packed-format #\x 1 #f 1 #f #f))
(define packed-format-c (make-packed-format #\c 1 #f 1 pack-char unpack-char))
(define packed-format-b (make-packed-format #\b 1 #t 1 pack-integer unpack-integer))
(define packed-format-B (make-packed-format #\B 1 #f 1 pack-integer unpack-integer))
(define packed-format-h (make-packed-format #\h 2 #t 1 pack-integer unpack-integer))
(define packed-format-H (make-packed-format #\H 2 #f 2 pack-integer unpack-integer))
(define packed-format-i (make-packed-format #\i 4 #t 4 pack-integer unpack-integer))
(define packed-format-I (make-packed-format #\I 4 #f 4 pack-integer unpack-integer))
(define packed-format-l (make-packed-format #\l 4 #t 4 pack-integer unpack-integer))
(define packed-format-L (make-packed-format #\L 4 #f 4 pack-integer unpack-integer))
(define packed-format-q (make-packed-format #\q 8 #t 8 pack-integer unpack-integer))
(define packed-format-Q (make-packed-format #\Q 8 #f 8 pack-integer unpack-integer))
(define packed-format-f (make-packed-format #\f 4 #t 4 pack-real unpack-real))
(define packed-format-d (make-packed-format #\d 8 #t 8 pack-real unpack-real))
(define packed-format-s (make-packed-format #\s 1 #f 1 pack-string unpack-string))

(define packed-format-specifiers '(#\x #\c #\b #\B #\h #\H #\i #\I #\l #\L #\q #\Q #\f #\d))

(define packed-format-alist
  `((#\x . ,packed-format-x)
    (#\c . ,packed-format-c)
    (#\b . ,packed-format-b)
    (#\B . ,packed-format-B)
    (#\h . ,packed-format-h)
    (#\H . ,packed-format-H)
    (#\i . ,packed-format-i)
    (#\I . ,packed-format-I)
    (#\l . ,packed-format-l)
    (#\L . ,packed-format-L)
    (#\q . ,packed-format-q)
    (#\Q . ,packed-format-Q)
    (#\f . ,packed-format-f)
    (#\d . ,packed-format-d)
    (#\s . ,packed-format-s)))

(define (char->packed-format char)
  "Return the packed format descriptor for the specified character or #f if
 the character is not a valid format specified."
  (let ((acell (assv char packed-format-alist)))
    (if acell (cdr acell) #f)))

;;; packed format string processing

(define packed-format-byte-order-regexp (make-regexp "^@|=|<|>|!"))
(define packed-format-regexp (make-regexp "\\s*([0-9]*)(x|c|b|B|h|H|i|I|l|L|q|Q|f|d|s)\\s*"))

(define (packed-format-for-each proc packed-format-string)
  "Applies a procedure to all of the packed format specifications in a
packed format string.  The procedure supplied takes four arguments:
byte-order-spec (char?), spec (string?), count (integer?), and format spec"
  (let* ((packed-format-string (string-trim-both packed-format-string))
         (byte-order-match (regexp-exec packed-format-byte-order-regexp packed-format-string))
         (byte-order-spec (if (regexp-match? byte-order-match)
                              (string-ref (match:string byte-order-match) 0)
                              #\@))
         (start (if (regexp-match? byte-order-match) 1 0)))
    (unless (= (fold-matches packed-format-regexp packed-format-string start
                             (lambda (m len)
                               (+ len (string-length (match:substring m)))))
               (string-length packed-format-string))
      (throw 'packed-format-invalid))
    (for-each
     (lambda (packed-format-spec)
       (let* ((count (if (> (string-length (match:substring packed-format-spec)) 1)
                         (string->number (substring (match:string packed-format-spec)
                                                    (car (array-ref packed-format-spec 2))
                                                    (cdr (array-ref packed-format-spec 2))))
                         1))
              (specifier (string-ref (match:string packed-format-spec) (car (array-ref packed-format-spec 3)))))
         (proc byte-order-spec packed-format-spec count specifier)))
     (list-matches packed-format-regexp packed-format-string))))

(define (pad-count offset alignment)
  "Returns the number of pad bytes to add to the offset for the specified
alignment."
  (modulo (- alignment (modulo offset alignment)) alignment))

(define (calculate-size packed-format-string)
  "Returns the number of bytes required to store packed binary data for the
specified format string."
  (let ((size 0))
    (packed-format-for-each
     (lambda (byte-order-spec packed-format-spec count specifier)
       (let ((packed-format (char->packed-format specifier)))
         ;; Align for native mode packing
         (when (eqv? byte-order-spec #\@)
           (set! size (+ size (pad-count size (packed-format-native-alignment packed-format)))))
         ;; Calculate size for this entry
         (set! size (+ size (* count (packed-format-size packed-format))))))
     packed-format-string)
    size))

(define* (pack-into packed-format-string buffer offset #:rest values)
  (packed-format-for-each
   (lambda (byte-order-spec packed-format-spec count specifier)
     (let* ((packed-format (char->packed-format specifier))
            (size (packed-format-size packed-format))
            (signed? (packed-format-signed? packed-format))
            (native-alignment (packed-format-native-alignment packed-format))
            (pack-handler (packed-format-pack-handler packed-format))
            (aligned? (if (eqv? byte-order-spec #\@) #t #f))
            (big-endian? (if (memv byte-order-spec '(#\@ #\=))
                             (system-big-endian?)
                             (if (eqv? byte-order-spec #\<) #f #t))))
       ;; Align for native mode packing
       (when aligned?
         (set! offset (+ offset (pad-count offset native-alignment))))
       ;; Pack values
       (cond ((eqv? specifier #\x)
              ;; Add count pad bytes
              (set! offset (+ offset count)))
             ((eqv? specifier #\s)
              ;; String, so count is the size
              (pack-handler buffer offset (car values) count)
              (set! values (cdr values))
              (set! offset (+ offset count)))
             (else
              (do ((i 1 (1+ i)))
                  ((> i count))
                (pack-handler buffer offset (car values) size signed? big-endian?)
                (set! values (cdr values))
                (set! offset (+ offset size)))))))
   packed-format-string)
  buffer)

(define* (pack packed-format-string #:rest values)
  (let* ((buffer-size (calculate-size packed-format-string))
         (buffer (make-bytevector buffer-size)))
    (apply pack-into packed-format-string buffer 0 values)))

(define (system-big-endian?)
  (eq? (native-endianness) 'big))

(define* (unpack-from packed-format-string buffer #:optional (offset 0))
  (let ((buffer-size (bytevector-length buffer))
        (values '()))
    (packed-format-for-each
     (lambda (byte-order-spec packed-format-spec count specifier)
       (let* ((packed-format (char->packed-format specifier))
              (size (packed-format-size packed-format))
              (signed? (packed-format-signed? packed-format))
              (native-alignment (packed-format-native-alignment packed-format))
              (unpack-handler (packed-format-unpack-handler packed-format))
              (aligned? (if (eqv? byte-order-spec #\@) #t #f))
              (big-endian? (if (memv byte-order-spec '(#\@ #\=))
                               (system-big-endian?)
                               (if (eqv? byte-order-spec #\<) #f #t))))
         ;; Align for native mode packing
         (when aligned?
           (set! offset (+ offset (pad-count offset native-alignment))))
         ;; unpack values
         (cond ((eqv? specifier #\x)
                ;; Add count pad bytes
                (set! offset (+ offset count)))
               ((eqv? specifier #\s)
                ;; String, so count is the size
                (set! values (append values (list (unpack-handler buffer offset count))))
                (set! offset (+ offset count)))
               (else
                (do ((i 1 (1+ i)))
                    ((> i count))
                  (set! values (append values (list (unpack-handler buffer offset size signed? big-endian?))))
                  (set! offset (+ offset size)))))))
     packed-format-string)
    values))

(define (unpack packed-format-string buffer)
  (unpack-from packed-format-string buffer 0))

(define* (write-packed packed-format-string port #:rest values)
  (let ((bytes (apply pack packed-format-string values)))
    (put-bytevector port bytes)))

(define (read-packed packed-format-string port)
  (let* ((buffer-size (calculate-size packed-format-string))
         (buffer (get-bytevector-n port buffer-size)))
    (unpack packed-format-string buffer)))

(define valid-packed-format-string-regexp
  (make-regexp "^(@|=|<|>|!)?(\\s*[0-9]*(x|c|b|B|h|H|i|I|l|L|q|Q|f|d|s)\\s*)*$" regexp/extended))

(define (packed-format-string? string)
  "A predicate function that returns #t if it's argument is a valid packed
format string, and #f otherwise."
  (and (string? string)
       (regexp-match? (regexp-exec valid-packed-format-string-regexp string))))
