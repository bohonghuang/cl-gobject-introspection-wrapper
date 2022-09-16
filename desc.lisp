;;;; desc.lisp

;;;; Copyright (C) 2022 Bohong Huang
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:gir-wrapper)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *namespace* nil)
  (defparameter *class* nil)
  (defparameter *quoted-name-alist* nil))

(defun quoted-name-symbol (name)
  (assoc-value *quoted-name-alist* name :test #'equal))

(defun transform-method-desc (desc &optional (class *class*))
  (let* ((info (gir::info-of desc))
         (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
         (symbol (intern name))
         (args (mapcar (lambda (desc)
                         (let ((name (gir:name-of desc)))
                           (or (quoted-name-symbol name)
                               (underscores->lisp-symbol name))))
                       (gir::arguments-desc-of desc)))
         (arg-types (mapcar #'gir:type-desc-of (gir::arguments-desc-of desc)))
         (ret-type (gir:type-desc-of (car (gir::returns-desc-of desc))))
         (class-name (or (quoted-name-symbol class) (camel-case->lisp-symbol class))))
    (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
      `(defun ,name-symbol (instance ,@args)
         (gir:invoke (instance ',symbol) ,@args))
      (cond
        ((and (uiop:string-prefix-p "GET-" name) (= (length args) 0))
         `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-~A-P" "~A-~A") class-name (subseq name 4))) (instance)
            (gir:invoke (instance ',symbol))))
        ((and (uiop:string-prefix-p "SET-" name) (= (length args) 1))
         `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name (subseq name 4)))) (value instance)
            (gir:invoke (instance ',symbol) value)))
        ((and (uiop:string-prefix-p "IS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-~A-P" class-name (subseq name 3))) (instance)
            (gir:invoke (instance ',symbol))))
        ((and (uiop:string-prefix-p "HAS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-~A-P" class-name name)) (instance)
            (gir:invoke (instance ',symbol))))
        (t `(defun ,(intern (format nil "~A-~A" class-name name)) (instance ,@args)
              (gir:invoke (instance ',symbol) ,@args)))))))

(defun transform-constructor-desc (desc &optional (namespace *namespace*) (class *class*))
  (let* ((info (gir::info-of desc))
         (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
         (symbol (intern name))
         (args (mapcar (lambda (desc)
                         (let ((name (gir:name-of desc)))
                           (or (quoted-name-symbol name)
                               (underscores->lisp-symbol name))))
                       (gir::arguments-desc-of desc)))
         (class-name (or (quoted-name-symbol class)
                         (camel-case->lisp-symbol class))))
    `(defun ,@(if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
                (list name-symbol args)
                (cond
                  ((uiop:string-prefix-p "NEW-" name)
                   `(,(intern (format nil "MAKE-~A-~A" class-name (subseq name 4))) ,args))
                  ((string-equal "NEW" name)
                   `(,(intern (format nil "MAKE-~A" class-name)) (&key ,@args)))
                  ((uiop:string-prefix-p "CREATE-" name)
                   `(,(intern (format nil "MAKE-~A-~A" class-name (subseq name 6))) ,args))
                  ((string-equal "CREATE" name)
                   `(,(intern (format nil "MAKE-~A" class-name)) (&key ,@args)))
                  (t `(,(intern (format nil "~A-~A" class-name name)) ,args))))
         (gir:invoke (,namespace ,class ',symbol) ,@args))))

(defun transform-class-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (let* ((info (gir::info-of desc))
         (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
         (symbol (intern name))
         (args (mapcar (lambda (desc)
                         (let ((name (gir:name-of desc)))
                           (or (quoted-name-symbol name)
                               (underscores->lisp-symbol name))))
                       (gir::arguments-desc-of desc)))
         (arg-types (mapcar #'gir:type-desc-of (gir::arguments-desc-of desc)))
         (ret-type (gir:type-desc-of (car (gir::returns-desc-of desc))))
         (class-name (or (quoted-name-symbol class)
                         (camel-case->lisp-symbol class))))
    (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
      `(defun ,name-symbol ,args
         (gir:invoke (,namespace ,class ',symbol) ,@args))
      (cond
        ((and (uiop:string-prefix-p "GET-" name) (= (length args) 0))
         `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-~A-P" "~A-~A") class-name (subseq name 4))) ()
            (gir:invoke (,namespace ,class ',symbol))))
        ((and (uiop:string-prefix-p "SET-" name) (= (length args) 1))
         `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name (subseq name 4)))) (value)
            (gir:invoke (,namespace ,class ',symbol) value)))
        ((and (uiop:string-prefix-p "IS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-~A-P" class-name (subseq name 3))) ()
            (gir:invoke (,namespace ,class ',symbol))))
        ((and (uiop:string-prefix-p "HAS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-~A-P" class-name name)) ()
            (gir:invoke (,namespace ,class ',symbol))))
        (t `(defun ,(intern (format nil "~A-~A" class-name name)) ,args
              (gir:invoke (,namespace ,class ',symbol) ,@args)))))))

(defun transform-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore class))
  (let* ((info (gir::info-of desc))
         (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
         (symbol (intern name))
         (args (mapcar (lambda (desc)
                         (let ((name (gir:name-of desc)))
                           (or (quoted-name-symbol name)
                               (underscores->lisp-symbol name))))
                       (gir::arguments-desc-of desc)))
         (arg-types (mapcar #'gir:type-desc-of (gir::arguments-desc-of desc)))
         (ret-type (gir:type-desc-of (car (gir::returns-desc-of desc)))))
    (if-let ((name-symbol (quoted-name-symbol (gir:info-get-name info))))
      `(defun ,name-symbol ,args
         (gir:invoke (,namespace ',symbol) ,@args))
      (cond
        ((and (uiop:string-prefix-p "GET-" name) (= (length args) 0))
         `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-P" "~A") (subseq name 4))) ()
            (gir:invoke (,namespace ',symbol))))
        ((and (uiop:string-prefix-p "SET-" name) (= (length args) 1))
         `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-P" "~A") (subseq name 4)))) (value)
            (gir:invoke (,namespace ',symbol) value)))
        ((and (uiop:string-prefix-p "IS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-P" (subseq name 3))) ()
            (gir:invoke (,namespace ',symbol))))
        ((and (uiop:string-prefix-p "HAS-" name) (= (length args) 0))
         `(defun ,(intern (format nil "~A-P" name)) ()
            (gir:invoke (,namespace ',symbol))))
        (t `(defun ,(intern (format nil "~A" name)) ,args
              (gir:invoke (,namespace ',symbol) ,@args)))))))

(defun transform-enum-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore namespace))
  `(defconstant ,(or (quoted-name-symbol (cons class (car desc)))
                     (intern (format nil "+~A-~A+"
                                     (or (quoted-name-symbol class)
                                         (camel-case->lisp-symbol class))
                                     (underscores->lisp-symbol (car desc)))))
     ,(cdr desc)))
