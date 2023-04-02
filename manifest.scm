(use-modules (guix inferior)
             (guix channels)
             (guix packages))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit
          "086f27cf8cb4198d15d7d65c8703d50b58ab3c03")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define inferior
  ;; An inferior representing the above revision.
  (inferior-for-channels channels))

(define (inferior-package package-specification)
  (car (lookup-inferior-packages inferior package-specification)))

(define (inferior-packages packages-specification)
  (map inferior-package packages-specification))

(define (inferior-packages->manifest packages)
  (packages->manifest (inferior-packages packages)))

;; Get packages from pinned packages
(inferior-packages->manifest '("sbcl" "cl-rove"))
