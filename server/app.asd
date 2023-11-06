#-asdf3.1 (error "admin requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "app"
  :description "An application for HR adaptation flow."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("reblocks"
               "reblocks-ui2-tailwind"
               "clack-handler-hunchentoot"
               "app/server")
  :in-order-to ((test-op (test-op "app-tests"))))


(register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))
