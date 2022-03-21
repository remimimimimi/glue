#lang racket/base

(require megaparsack megaparsack/text)
(require data/monad data/applicative)

;;w(define list/p)
(define end-of-line/p
  (lookahead/p (char/p #\newline)))

;; Whitespace parser
(define ws/p
  (many/p (hidden/p space/p)))

;; Use (many+/p spaces/p) because we need here not
;; hidden/p version of whitespace to read tokens
(define space-sep/p
  (many/p space/p))
(define space-sep+/p
  (many+/p space/p))

(define rest-of-line/p
  (or/p (do end-of-line/p
            (pure ""))
        (do [c <- any-char/p]
            [cs <- rest-of-line/p]
            (pure (string-append (string c) cs)))))

(define line-comment/p
  (do (try/p (string/p ";"))
      rest-of-line/p))

(define hex-digit/p
  (label/p
    "hex digit"
    (or/p (do (char/p    #\0) (pure  0))
          (do (char/p    #\1) (pure  1))
          (do (char/p    #\2) (pure  2))
          (do (char/p    #\3) (pure  3))
          (do (char/p    #\4) (pure  4))
          (do (char/p    #\5) (pure  5))
          (do (char/p    #\6) (pure  6))
          (do (char/p    #\7) (pure  7))
          (do (char/p    #\8) (pure  8))
          (do (char/p    #\9) (pure  9))
          (do (char-ci/p #\a) (pure 10))
          (do (char-ci/p #\b) (pure 11))
          (do (char-ci/p #\c) (pure 12))
          (do (char-ci/p #\d) (pure 13))
          (do (char-ci/p #\e) (pure 14))
          (do (char-ci/p #\f) (pure 15)))))

;; TODO: Add more number variants
(define number/p
  (label/p
    "number"
    (or/p (do (char/p #\-)
              [i <- integer/p]
              (pure (- i)))
          integer/p)))

(define string-char-or-escape/p
  (or/p (do (char/p #\\)
            (or/p (char/p #\")
                  (char/p #\\)
                  (char/p #\/)
                  (do (char/p #\b) (pure #\backspace))
                  (do (char/p #\f) (pure #\page))
                  (do (char/p #\n) (pure #\newline))
                  (do (char/p #\r) (pure #\return))
                  (do (char/p #\t) (pure #\tab))
                  (do (char/p #\u)
                      [ns <- (repeat/p 4 hex-digit/p)]
                      (define code (foldl (lambda (v l) (+ l (* 16 v))) 0 ns))
                      ;; (define code (foldl #{+ %2 (* 16 %1)} 0 ns))
                      (pure (integer->char code)))))
        (satisfy/p (lambda (c) (not (char=? #\" c))))))

(define string-literal/p
  (label/p
    "string literal"
    (do (char/p #\")
        [chars <- (many/p string-char-or-escape/p)]
        (char/p #\")
        (pure ('string (list->string chars))))))

(define symbol/p
  (label/p
    "symbol"
    (do [s <- (many/p
               (satisfy/p
                (lambda (c)
                  (not (or (char=? #\( c)
                           (char=? #\) c))))))]
        (pure (list->string s)))))

(define (atom/p)
  (label/p
   ("atom"
    (do [v <- (or/p number/p
                    symbol/p
                    string-literal/p
                    (atom-list/p)
                    (atom-list-with-syntax-op/p))]
        (pure v)))))

(define (atom-list/p)
  (label/p
    "list of atoms"
    (do (char/p #\()
        ws/p
        ;; Use (many+/p spaces/p) because we need here not
        ;; hidden/p version of whitespace to read tokens
        [l <- (many/p (atom/p) #:sep space-sep+/p)]
        ws/p
        (char/p #\))
        (pure l))))

(define syntax-op/p
  (or/p (char/p #\')       ;; Quote
        (char/p #\`)       ;; Quasi-quote
        (char/p #\,)       ;; Quasi-quote eval
        (string/p ",@")    ;; Quasi-quote splash eval
        (string/p "#'")    ;; Unquote
        (string/p "#`")    ;; Quasi-unquote
        (string/p "#,")    ;; Quasi-unquote
        (string/p "#,@"))) ;; Quasi-unquote splash

(define (atom-list-with-syntax-op/p)
  (do [op <- syntax-op/p]
      [atom-list <- (atom-list/p)]
      (pure (list op atom-list))))

(define program/p
  (do ws/p
      [v <- (many/p (atom-list/p) #:sep space-sep/p)]
      ws/p
      eof/p
      (pure v)))

(define (parse-program s)
  (parse-result! (parse-string program/p s)))

(provide parse-program)
