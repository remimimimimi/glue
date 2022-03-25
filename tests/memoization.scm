(define-module (tests memoization)
  #:use-module (srfi srfi-64)
  #:use-module (bozon))

(module-define! (resolve-module '(srfi srfi-64))
                'test-log-to-file #f)

(test-begin "test-suite")

(test-equal "Greetings"
  "Hi hackers!"
  (say-hi))

(test-end "test-suite")
