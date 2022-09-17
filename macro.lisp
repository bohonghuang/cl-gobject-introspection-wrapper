;;;; macro.lisp

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

(defmacro define-gir-class (name &optional (namespace *namespace*))
  (let ((desc (gir:nget-desc (eval namespace) name))
        (*namespace* namespace)
        (*class* name))
    (let ((constructors (loop :for desc :in (gir:list-constructors-desc desc)
                              :for (form subst-arg-name) := (multiple-value-list (transform-constructor-desc desc))
                              :collect form :into forms
                              :collect subst-arg-name :into subst-arg-names
                              :collect desc :into descs
                              :finally (return (merge-constructor-forms forms descs subst-arg-names))))
          (methods (mapcar #'transform-method-desc (gir:list-methods-desc desc)))
          (class-functions (when (typep desc 'gir:object-class)
                             (mapcar #'transform-class-function-desc (gir:list-class-functions-desc desc)))))
      `(progn
         ,@constructors
         ,@methods
         ,@class-functions
         ,(when-let ((definitions (append constructors methods class-functions)))
            `(export '(,@(remove-if-not #'symbolp (mapcar #'second definitions)))))))))

(defmacro define-gir-constant (name &optional (namespace *namespace*))
  `(define-constant ,(or (quoted-name-symbol name)
                         (symbolicate '#:+ (underscores->lisp-symbol name) '#:+))
       (handler-case (gir:nget ,namespace ,name)
         (warning ()))
     :test #'equal))

(defmacro define-gir-enum (name &optional (namespace *namespace*))
  (let ((*namespace* namespace)
        (*class* name))
    (let ((members (mapcar #'transform-enum-desc (gir:values-of (gir:nget-desc (eval namespace) name)))))
      `(progn
         ,@members
         (export '(,@(remove-if-not #'symbolp (mapcar #'second members))))))))

(defmacro define-gir-function (name &optional (namespace *namespace*))
  (let ((*namespace* namespace)
        (*class* name))
    (let ((function (transform-function-desc (gir:nget-desc (eval namespace) name))))
      `(progn
         ,function
         (export ',(when (symbolp (second function)) (second function)))))))

(defmacro define-gir-namespace (name &optional version repository)
  (let ((*namespace* (gir:require-namespace name version))
        (namespace-symbol (intern "*NS*")))
    `(progn
       (eval-when (:execute :load-toplevel :compile-toplevel)
         (defparameter ,namespace-symbol (gir:require-namespace ,name ,version)))
       ,@(mapcar (lambda (info)
                   (let ((name (gir:info-get-name info))
                         (type (gir:info-get-type info)))
                     (switch (type)
                       (:object `(define-gir-class ,name ,namespace-symbol))
                       (:struct `(define-gir-class ,name ,namespace-symbol))
                       (:function `(define-gir-function ,name ,namespace-symbol))
                       (:constant `(define-gir-constant ,name ,namespace-symbol))
                       (:enum `(define-gir-enum ,name ,namespace-symbol))
                       (:flags `(define-gir-enum ,name ,namespace-symbol)))))
                 (gir:repository-get-infos repository name)))))
