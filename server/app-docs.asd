(defsystem "app-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides documentation for admin."
  :pathname "docs"
  :depends-on ("app"
               "app-docs/index"))
