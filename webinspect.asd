;; cl-filed is a collection of packages which implement
;; a file database, with search, indexing, and syncing across
;; muliple, pluggable backends

(asdf:defsystem "webinspect"
  :description "a system for interacting with lisp objects through the web"
  :version "0.0.1"
  :author "Kyle Nusbaum <kjn@9project.net>"
  :license "MIT"
  :depends-on (:hunchentoot :cl-who)
  :components ((:file "package")
	       (:file "server" :depends-on ("package"))))
