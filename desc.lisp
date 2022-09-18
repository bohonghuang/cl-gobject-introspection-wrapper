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

(defparameter *namespace* nil)

(defparameter *class* nil)

(defparameter *quoted-name-alist* nil)

(defun quoted-name-symbol (name)
  (multiple-value-bind (value exists-p) (assoc-value *quoted-name-alist* name :test #'equal)
    (when (and (not value) exists-p)
      (throw 'skip nil))
    value))

(defun camel-case->lisp-name (phrase)
  (string-right-trim "-" (nstring-upcase (cl-ppcre:regex-replace-all "((?<!\d)(\d+))|(([A-Z][a-z]+))|((?<![a-z])([a-z]+))|((?<![A-Z])(([A-Z](?![a-z]))+))" phrase "\\1\\3\\5\\7-"))))

(defun camel-case->lisp-symbol (phrase)
  (intern (string-upcase (camel-case->lisp-name phrase))))

(defun underscores->lisp-name (phrase)
  (substitute #\- #\_ phrase))

(defun underscores->lisp-symbol (phrase)
  (intern (string-upcase (underscores->lisp-name phrase))))

(defun transform-class-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore desc))
  (catch 'skip
    (let* ((class-symbol (or (quoted-name-symbol class) (camel-case->lisp-symbol class)))
           (pred-symbol (symbolicate class-symbol '#:-p)))
      `((defun ,pred-symbol (instance)
          (class-instance-p instance (gir:nget ,namespace ,class)))
        (deftype ,class-symbol ()
          '(satisfies ,pred-symbol))))))

(defun transform-interface-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore desc))
  (catch 'skip
    (let* ((interface-symbol (or (quoted-name-symbol class) (camel-case->lisp-symbol class)))
           (pred-symbol (symbolicate interface-symbol '#:-p)))
      `((defun ,pred-symbol (instance)
          (interface-instance-p instance (gir:nget ,namespace ,class)))
        (deftype ,interface-symbol ()
          '(satisfies ,pred-symbol))))))

(defun transform-method-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore namespace))
  (catch 'skip
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
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^GET-(.+)$" name)
                  `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-~A-P" "~A-~A") class-name name)) (instance)
                     (gir:invoke (instance ',symbol))))))
          ((and args (not (cdr args))
                (ppcre:register-groups-bind (name) ("^SET-(.+)$" name)
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name name))) (value instance)
                     (gir:invoke (instance ',symbol) value)))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^IS-(.+)$" name)
                  `(defun ,(intern (format nil "~A-~A-P" class-name name)) (instance)
                     (gir:invoke (instance ',symbol))))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^(HAS-.+)$" name)
                  `(defun ,(intern (format nil "~A-~A-P" class-name name)) (instance)
                     (gir:invoke (instance ',symbol))))))
          (t `(defun ,(intern (format nil "~A-~A" class-name name)) (instance ,@args)
                (gir:invoke (instance ',symbol) ,@args))))))))

(defun transform-constructor-desc (desc &optional (namespace *namespace*) (class *class*))
  (catch 'skip
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
      (let ((body `(gir:invoke (,namespace ,class ',symbol) ,@args)))
        (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
          (values `(defun ,name-symbol ,args ,body) nil)
          (if-let ((method (ppcre:register-groups-bind (verb prep method) ("^(NEW|CREATE)(-WITH|-FROM|$)(-.+|$)" name)
                             (declare (ignore verb))
                             (list prep method))))
            (values `(defun ,(intern (format nil "MAKE-~A" class-name)) (&key ,@args) ,body) (and method
                                                                                                  (every (compose #'plusp #'length) method)
                                                                                                  (mapcar (lambda (str) (subseq str 1)) method)))
            (values `(defun ,(intern (format nil "~A-~A" class-name name)) ,args ,body) nil)))))))

(defun merge-constructor-forms (forms descs subst-arg-names)
  (let ((grouped nil))
    (loop :for (defun-symbol name lambda-list body) :in forms
          :for desc :in descs
          :for subst-arg-name :in subst-arg-names
          :when (eq (car lambda-list) '&key)
            :do (pop lambda-list)
          :do (push (list desc subst-arg-name body)
                    (assoc-value (assoc-value grouped name) lambda-list :test #'equal)))
    (loop :with unmergeable-constructors
          :for (name . arg-groups) :in grouped
          :do (setf arg-groups
                    (mapcan (lambda (arg-group)
                              (destructuring-bind (args . bodies) arg-group
                                (if (> (length bodies) 1)
                                    (if (= (length args) 1)
                                        (loop :for (desc subst-arg-name body) :in bodies
                                              :if subst-arg-name
                                                :collect (let ((subst-symbol (intern (second subst-arg-name))))
                                                           `((,subst-symbol) (let ((,(first args) ,subst-symbol)) ,body)))
                                                  :into result
                                              :else
                                                :sum 1 :into no-subst-name-count
                                                :and :collect `(,args ,body) :into result
                                              :finally
                                                 (assert (<= no-subst-name-count 1))
                                                 (return result))
                                        (loop :for (desc subst-arg-name body) :in bodies
                                              :if subst-arg-name
                                                :do (let ((*quoted-name-alist*
                                                            (cons (let ((name (gir:info-get-name (gir::info-of desc))))
                                                                    (cons (cons *class* name)
                                                                          (intern (format nil "MAKE-~A-~A-~A"
                                                                                          (camel-case->lisp-symbol *class*)
                                                                                          (first subst-arg-name)
                                                                                          (second subst-arg-name)))))
                                                                  *quoted-name-alist*)))
                                                      (push (transform-constructor-desc desc) unmergeable-constructors))
                                              :else
                                                :collect `(,args ,(third bodies))))
                                    (mapcar (compose (lambda (body) `(,args ,body)) #'third) bodies))))
                            (sort arg-groups #'> :key (compose #'length #'first))))
          :collect `(defun ,name (&key ,@(mapcar (lambda (arg) `(,arg :unspecified))
                                                 (remove-duplicates (loop :for (args body) :in arg-groups
                                                                          :append args))))
                      (cond
                        ,@(mapcar (lambda (arg-group)
                                    (destructuring-bind (args body) arg-group
                                      `((not (or ,@(mapcar (lambda (arg) `(eql ,arg :unspecified)) args))) ,body)))
                                  arg-groups)
                        (t (error "Invalid arguments for constructor ~A" ',name))))
            :into merged-constructors
          :finally (return (nconc merged-constructors unmergeable-constructors)))))

(defun transform-class-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (catch 'skip
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
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^GET-(.+)$" name)
                  `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-~A-P" "~A-~A") class-name name)) ()
                     (gir:invoke (,namespace ,class ',symbol))))))
          ((and args (not (cdr args))
                (ppcre:register-groups-bind (name) ("^SET-(.+)$" name)
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name name))) (value)
                     (gir:invoke (,namespace ,class ',symbol) value)))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^IS-(.+)$" name)
                  `(defun ,(intern (format nil "~A-~A-P" class-name name)) ()
                     (gir:invoke (,namespace ,class ',symbol))))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^(HAS-.+)$" name)
                  `(defun ,(intern (format nil "~A-~A-P" class-name name)) ()
                     (gir:invoke (,namespace ,class ',symbol))))))
          (t `(defun ,(intern (format nil "~A-~A" class-name name)) ,args
                (gir:invoke (,namespace ,class ',symbol) ,@args))))))))

(defun transform-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore class))
  (catch 'skip
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
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^GET-(.+)$" name)
                  `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-P" "~A") name)) ()
                     (gir:invoke (,namespace ',symbol))))))
          ((and args (not (cdr args))
                (ppcre:register-groups-bind (name) ("^SET-(.+)$" name)
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-P" "~A") name))) (value)
                     (gir:invoke (,namespace ',symbol) value)))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^IS-(.+)$" name)
                  `(defun ,(intern (format nil "~A-P" name)) ()
                     (gir:invoke (,namespace ',symbol))))))
          ((and (not args)
                (ppcre:register-groups-bind (name) ("^(HAS-.+)$" name)
                  `(defun ,(intern (format nil "~A-P" name)) ()
                     (gir:invoke (,namespace ',symbol))))))
          (t `(defun ,(intern (format nil "~A" name)) ,args
                (gir:invoke (,namespace ',symbol) ,@args))))))))

(defun transform-enum-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore namespace))
  (catch 'skip
    `(defconstant ,(or (quoted-name-symbol (cons class (car desc)))
                       (intern (format nil "+~A-~A+"
                                       (or (quoted-name-symbol class)
                                           (camel-case->lisp-symbol class))
                                       (underscores->lisp-symbol (car desc)))))
       ,(cdr desc))))

(defun transform-constant-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore class))
  (catch 'skip
    `(define-constant ,(or (quoted-name-symbol desc)
                           (symbolicate '#:+ (underscores->lisp-symbol desc) '#:+))
         (handler-case (gir:nget ,namespace ,desc)
           (warning ()))
       :test #'equal)))
