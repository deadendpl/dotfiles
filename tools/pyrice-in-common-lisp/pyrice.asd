(defsystem "pyrice"
  :version "testing"
  :author "Oliwier Czerwiński <oliwier.czerwi@proton.me>"
  :license "GPL-3.0"
  :description "Personal pywal and wpgtk wrapper"
  :depends-on (:clingon :uiop)
  :components ((:file "main"))
  :build-operation "program-op"
  :build-pathname "pyrice"
  :entry-point "pyrice::main")
