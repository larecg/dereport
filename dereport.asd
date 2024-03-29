(defsystem "dereport"
  :build-operation program-op
  :build-pathname "dereport"
  :entry-point "dereport:main"
  :version "0.1.0"
  :author ""
  :description "Daily Report Utility"
  :license "LLGPL"
  :depends-on ("str"
               "log4cl"
               "cl-ppcre"
               "unix-opts"
               "arrow-macros")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "dereport")
                 (:file "cli")))))
