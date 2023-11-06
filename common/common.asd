#-asdf3.1 (error "common requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "common"
  :description "Common utils."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("common/search")
  :in-order-to ((test-op (test-op "common-tests"))))


(register-system-packages "mito" '("MITO.DAO"))
