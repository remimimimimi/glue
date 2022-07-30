#lang nanopass

(define (name? n)
  (symbol? n))

(define (datum? d)
  #t)

(define (constant? c)
  (or (number? c)
      (char? c)
      (string? c)))


(define (field-modifier? name) (string-suffix? name "!"))

(define (primitive? p)
  (memq p
   '(+ - * / = < > <= >=
     fl+ fl- fl* fl/ fl= fl< fl> fl<= fl>=
     cons car cdr pair?
     vector make-vector vector-length vector-ref vector-set! vector?
     string make-string string-length string-ref string-set! string?
     void
     and or not eq?
     abs
     quotient remainder expt
     bitwise-ior bitwise-and bitwise-xor bitwise-not
     shift-left arithmetic-shift-right logical-shift-right
     char=? char<?
     current-input-port current-output-port
     open-input-file close-input-file
     open-output-file close-output-file
     read-char peek-char read-integer
     write-char newline write-string write-integer
     force-output
     deallocate null-pointer null-pointer?
     allocate-memory deallocate-memory
     unsigned-byte-ref unsigned-byte-set!
     word-ref word-set!
     flonum-ref flonum-set!
     address? null-address null-address?
     address+ address- address-difference
     address= address< address> address<= address>=
     integer->address address->integer
     copy-memory! memory-equal?
     char-pointer->string char-pointer->null-terminated-string
     read-block write-block
     error error-string)))


(define (primitive-type? pt)
  (memq pt '(integer float null unit boolean input-port output-port char address)))

(define-language PreScheme
  (entry Program)
  (terminals
    (name (n))
    (datum (d))
    (constant (c))
    (primitive (prim))
    (primitive-type (primtype))
    (field-modifier (field-modif)))
  (TypeSpecifier (type)
    primtype
    n
    (-> type* ... type)
    (^ type)
    (tuple type* ... type))
  (Constructor (constr)
    (constructor n* ... n))
  (FieldDefinition (field)
    (n type)
    (n0 type n1)
    (n type field-modif)
    (n0 type n1 field-modif))
  ;; `cond` and `do` should be implemented using macros
  (Expr (expr body)
    n
    c
    'd
    (define n expr)
    (define (n0 n* ...) body* ... body)
    (define-enumeration n n* ...)
    (define-record-type n
      constr
      field* ...)
    (external n type)
    (if expr0 expr1 expr2)
    (let ([n* expr*] ...) body* ... body)
    (let* ([n* expr*] ...) body* ... body)
    (prim0 expr* ...)
    (expr0 expr* ...))
  (Program (prog)
    (expr* ...)))

(define-parser parse-pre-scheme PreScheme)

;; TODO list of passes:
;; - lambda hoisting
