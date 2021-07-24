#lang racket

;;;;;;;;;;;;;;;; Hex conversion ;;;;;;;;;;;;;;;;
(define (hex-char-to-bin-list c)
  (cond [(eq? #\0 c) (list 0 0 0 0)]
        [(eq? #\1 c) (list 0 0 0 1)]
        [(eq? #\2 c) (list 0 0 1 0)]
        [(eq? #\3 c) (list 0 0 1 1)]
        [(eq? #\4 c) (list 0 1 0 0)]
        [(eq? #\5 c) (list 0 1 0 1)]
        [(eq? #\6 c) (list 0 1 1 0)]
        [(eq? #\7 c) (list 0 1 1 1)]
        [(eq? #\8 c) (list 1 0 0 0)]
        [(eq? #\9 c) (list 1 0 0 1)]
        [(eq? #\A c) (list 1 0 1 0)]
        [(eq? #\B c) (list 1 0 1 1)]
        [(eq? #\C c) (list 1 1 0 0)]
        [(eq? #\D c) (list 1 1 0 1)]
        [(eq? #\E c) (list 1 1 1 0)]
        [(eq? #\F c) (list 1 1 1 1)]
        [#t (error "hex-char-to-bin-list: invalid hex char")]))

(define (hex-string-to-vector s)
  (list->vector (append* (map hex-char-to-bin-list (string->list s)))))

(define (vector-to-hex n)
  (cond [(equal? '#(0 0 0 0) n) "0"]
        [(equal? '#(0 0 0 1) n) "1"]
        [(equal? '#(0 0 1 0) n) "2"]
        [(equal? '#(0 0 1 1) n) "3"]
        [(equal? '#(0 1 0 0) n) "4"]
        [(equal? '#(0 1 0 1) n) "5"]
        [(equal? '#(0 1 1 0) n) "6"]
        [(equal? '#(0 1 1 1) n) "7"]
        [(equal? '#(1 0 0 0) n) "8"]
        [(equal? '#(1 0 0 1) n) "9"]
        [(equal? '#(1 0 1 0) n) "a"]
        [(equal? '#(1 0 1 1) n) "b"]
        [(equal? '#(1 1 0 0) n) "c"]
        [(equal? '#(1 1 0 1) n) "d"]
        [(equal? '#(1 1 1 0) n) "e"]
        [(equal? '#(1 1 1 1) n) "f"]
        [#t (error "vector-to-hex: un-reachable")]))
;;;;;;;;;;;;;;;; Binary operations ;;;;;;;;;;;;;;;;
(define (b-xor m n) (if (= m n) 0 1))
(define (b-and m n) (if (and (= m 1) (= n 1)) 1 0))
(define (b-or m n) (if (or (= m 1) (= n 1)) 1 0))
(define (b-not x) (if (= x 1) 0 1))
(define (vector-bitwise-op xs ys op)
  (letrec ([f (lambda (i)
                (if (= i 32)
                    null
                    (cons (op (vector-ref xs i) (vector-ref ys i)) (f (+ i 1)))))])
    (list->vector (f 0))))

(define (vector-xor xs ys)
  (vector-bitwise-op xs ys b-xor))

(define (vector-and xs ys)
  (vector-bitwise-op xs ys b-and))

(define (vector-or xs ys)
  (vector-bitwise-op xs ys b-or))

(define (vector-not xs)
  (letrec ([f (lambda (i)
                (if (= i 32)
                    null
                    (cons (b-not (vector-ref xs i)) (f (+ i 1)))))])
    (list->vector (f 0))))

(define (vector-reverse xs)
  (list->vector (reverse (vector->list xs))))

(define (vector-add-2 xs ys)
  (letrec ([f (lambda (xs ys i rem)
                (if (= i 32)
                    null
                    (let ([v (+ (vector-ref xs i) (vector-ref ys i) rem)])
                      (cond [(= v 0) (cons 0 (f xs ys (+ i 1) 0))]
                            [(= v 1) (cons 1 (f xs ys (+ i 1) 0))]
                            [(= v 2) (cons 0 (f xs ys (+ i 1) 1))]
                            [(= v 3) (cons 1 (f xs ys (+ i 1) 1))]
                            [#f (error "unreachable")]))))])
    (list->vector (reverse (f (vector-reverse xs) (vector-reverse ys) 0 0)))))

(define (foldl fn init xs)
  (letrec ([f (lambda (acc ys)
                (if (null? ys)
                    acc
                    (f (fn acc (car ys)) (cdr ys))))])
    (f init xs)))

(define vector-add (lambda xs
  (foldl vector-add-2 (make-vector 32 0) xs)))

(define (vector-left-rotate xs n)
  (let* ([size (vector-length xs)]
         [idx (remainder n size)])
    (let-values ([(a b) (vector-split-at xs idx)])
      (vector-append b a))))

;;;;;;;;;;;;;;;; SHA-1 specific ;;;;;;;;;;;;;;;;
(define (bytes-to-vector bs)
  (list->vector
   (append*
    (map
     (lambda (b)
       (map (lambda (c)
              (- (char->integer c) 48))
            (string->list (~r b #:base 2 #:min-width 8 #:pad-string "0"))))
     bs))))

(define (partition bs n)
  (letrec ([size (vector-length bs)]
           [f (lambda (xs)
              (cond [(= (vector-length xs) 0) null]
                    [#t (let-values ([(c n) (vector-split-at xs n)])
                          (cons c (f n)))]))])
    (f bs)))


(define (preprocess msg)
  (let* ([bs (bytes->list (string->bytes/utf-8 msg))]
         [size (* (string-utf-8-length msg) 8)]
         [zeros (make-vector (- (* (ceiling (/ (+ size 1) 512)) 512) (+ size 1) 64))]
         [padded (vector-append (bytes-to-vector bs) (make-vector 1 1) zeros)]
         [final (vector-append padded (bytes-to-vector (bytes->list (integer->integer-bytes size 8 #f #t))))])
    (partition final 512)))

(define (extend-w s e vec)
  (if (= s e)
      vec
      (let ([wi (vector-left-rotate
                 (vector-xor (vector-ref vec (- s 3))
                             (vector-xor (vector-ref vec (- s 8))
                                         (vector-xor (vector-ref vec (- s 14))
                                                     (vector-ref vec (- s 16)))))
                 1)])
        (extend-w (+ s 1) e (vector-append vec (make-vector 1 wi))))))

(define (process-chunk h0 h1 h2 h3 h4 chunk)
  (let ([w (extend-w 16 80 (list->vector (partition chunk 32)))])
    (letrec ([fk (lambda (a b c d e i)
                   (cond [(and (>= i 0) (<= i 19))
                          (values
                           ;f = (b and c) or ((not b) and d)
                           (vector-or (vector-and b c)
                                      (vector-and (vector-not b)
                                                  d))
                           (hex-string-to-vector "5A827999"))]
                         [(and (>= i 20) (<= i 39))
                          (values
                           ;f = b xor c xor d
                           (vector-xor b (vector-xor c d))
                           (hex-string-to-vector "6ED9EBA1"))]
                         [(and (>= i 40) (<= i 59))
                          (values
                           ;f = (b and c) or (b and d) or (c and d)
                           (vector-or (vector-and b c)
                                      (vector-or (vector-and b d)
                                                 (vector-and c d)))
                           (hex-string-to-vector "8F1BBCDC"))]
                         [(and (>= i 60) (<= i 79))
                          (values
                           ;f = b xor c xor d
                           (vector-xor b (vector-xor c d))
                           (hex-string-to-vector "CA62C1D6"))]
                         [#t (error "unreachable")]))]
             [loop (lambda (a b c d e i)
                     (if (= i 80)
                         (values a b c d e)
                         (let-values ([(f k) (fk a b c d e i)])
                           (let ([temp (vector-add (vector-left-rotate a 5) f e k (vector-ref w i))])
                             (loop temp a (vector-left-rotate b 30) c d (+ i 1))))))])
      (let-values ([(a b c d e) (loop h0 h1 h2 h3 h4 0)])
        (values (vector-add h0 a)
                (vector-add h1 b)
                (vector-add h2 c)
                (vector-add h3 d)
                (vector-add h4 e)))
)))

(define (sha1 msg)
  (let ([h0 (hex-string-to-vector "67452301")]
        [h1 (hex-string-to-vector "EFCDAB89")]
        [h2 (hex-string-to-vector "98BADCFE")]
        [h3 (hex-string-to-vector "10325476")]
        [h4 (hex-string-to-vector "C3D2E1F0")]
        [chunks (preprocess msg)])
    (letrec ([f (lambda (h0 h1 h2 h3 h4 cs)
                  (if (null? cs)
                      (values h0 h1 h2 h3 h4)
                      (let-values ([(h0_ h1_ h2_ h3_ h4_) (process-chunk h0 h1 h2 h3 h4 (car cs))])
                        (f h0_ h1_ h2_ h3_ h4_ (cdr cs)))))])
      (let-values ([(h0 h1 h2 h3 h4) (f h0 h1 h2 h3 h4 chunks)])
        (let ([bit-hash (vector-append h0 (vector-append h1 (vector-append h2 (vector-append h3 h4))))])
          (string-append* (map vector-to-hex (partition bit-hash 4))))))))

;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;
(require rackunit)
(require rackunit/text-ui)

(define tests
  (test-suite
   "Test SHA-1"
   (check-equal? (sha1 "") "da39a3ee5e6b4b0d3255bfef95601890afd80709")
   (check-equal? (sha1 "abc") "a9993e364706816aba3e25717850c26c9cd0d89d")
   (check-equal? (sha1 "The quick brown fox jumps over the lazy dog") "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
   (check-equal? (sha1 (make-string 1000 #\a)) "291e9a6c66994949b57ba5e650361e98fc36b1ba")
   (check-equal? (sha1 (make-string 10000 #\a)) "a080cbda64850abb7b7f67ee875ba068074ff6fe")))

(run-tests tests)