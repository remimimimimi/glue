;; TODO: Everything here
(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system cargo)
  (gnu packages)
  (gnu packages llvm)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "bozon")
  (version "0.1.0")
  (source ".")
  (build-system cargo-build-system)
  (arguments `())
  (native-inputs
    `(("llvm" ,llvm-13)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `())
  (synopsis "")
  (description "")
  (home-page
    "https://github.com/remimimimimi/bozon")
  (license license:gpl3+))
