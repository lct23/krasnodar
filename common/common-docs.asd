(defsystem "common-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides documentation for common."
  :pathname "docs"
  :depends-on ("common"
               "common-docs/index"))
