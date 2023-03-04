;;;; util.lisp

;;;; Copyright (C) 2022-2023 Bohong Huang
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

(in-package #:gir)

(defmethod info-of ((desc callable-desc))
  (slot-value desc 'info))

(in-package #:gir-wrapper)

(defgeneric pointer-object (pointer type)
  (:documentation "Construct GObject from a CFFI pointer."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'object-pointer) (fdefinition 'gir::this-of)))

(defun subclassp (class-a class-b)
  (loop :for class := class-a :then (gir:parent-of class)
        :while class
        :thereis (gir:info-equal (gir::info-of class) (gir::info-of class-b))))

(defun class-instance-p (instance class)
  (subclassp (typecase instance
               (gir::object-instance (gir:gir-class-of instance))
               (gir::struct-instance (gir::struct-class-of instance))
               (t (return-from class-instance-p nil)))
             class))

(defun interface-instance-p (instance interface)
  (loop :with interface-info := (gir::info-of interface)
        :for info :in (gir::interface-infos-of (typecase instance
                                                 (gir::object-instance (gir:gir-class-of instance))
                                                 (gir::struct-instance (gir::struct-class-of instance))
                                                 (t (return-from interface-instance-p nil))))
        :thereis (gir:info-equal info interface-info)) )
