(hall-description
  (name "bozon")
  (prefix "")
  (version "0.1")
  (author "remimimimimi")
  (copyright (2022))
  (synopsis "")
  (description "")
  (home-page
    "https://github.com/remimimimimi/bozon")
  (license gpl3+)
  (dependencies `())
  (files (libraries
           ((scheme-file "bozon") (directory "bozon" ())))
         (tests ((directory "tests" ((scheme-file "memoization")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "bozon")))))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
