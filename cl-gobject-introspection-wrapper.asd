(defsystem cl-gobject-introspection-wrapper
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "Wrap and call GObject Introspection FFI function in LISP style, based on cl-gobject-introspection."
  :homepage "https://github.com/BohongHuang/cl-gobject-introspection-wrapper"
  :bug-tracker "https://github.com/BohongHuang/cl-gobject-introspection-wrapper/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-gobject-introspection-wrapper.git")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "desc")
               (:file "macro"))
  :depends-on (#:alexandria #:cl-gobject-introspection #:cl-ppcre))

(uiop:register-image-dump-hook
 (lambda ()
   (setf (symbol-value (find-symbol "*NAMESPACE-CACHE*" :gir))
         (make-hash-table :test #'equal))))
