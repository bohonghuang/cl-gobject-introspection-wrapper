;;;; desc.lisp

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

(defun underscores->lisp-symbol (phrase &optional case-sensitive-p)
  (intern (funcall (if case-sensitive-p #'identity #'string-upcase) (underscores->lisp-name phrase))))

(defun callable-desc-argument-names (desc)
  (let ((argument-name-case-sensitive-p nil))
    (flet ((desc-args ()
             (mapcar
              (lambda (desc)
                (let ((name (gir:name-of desc)))
                  (or (quoted-name-symbol name) (underscores->lisp-symbol name argument-name-case-sensitive-p))))
              (gir:arguments-desc-of desc))))
      (let ((args (desc-args)))
        (unless (= (length (remove-duplicates args)) (length args))
          (setf argument-name-case-sensitive-p t
                args (desc-args)))
        (values args)))))

(defun transform-class-desc (desc &optional (namespace *namespace*) (class *class*))
  (catch 'skip
    (let* ((class-symbol (or (quoted-name-symbol class) (camel-case->lisp-symbol class)))
           (pred-symbol (symbolicate class-symbol (if (find #\- (symbol-name class-symbol)) '#:-p '#:p))))
      `((defun ,pred-symbol (instance)
          (class-instance-p instance (gir:nget ,namespace ,class)))
        (deftype ,class-symbol ()
          '(satisfies ,pred-symbol))
        (defmethod gir-wrapper:pointer-object (pointer (type (eql ',class-symbol)))
          (declare (ignore type))
          (make-instance ',(etypecase desc
                             (gir::object-class 'gir::object-instance)
                             (gir::struct-class 'gir::struct-instance))
                         :class (gir:nget ,namespace ,class)
                         :this pointer))))))

(defun transform-interface-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore desc))
  (catch 'skip
    (let* ((interface-symbol (or (quoted-name-symbol class) (camel-case->lisp-symbol class)))
           (pred-symbol (symbolicate interface-symbol (if (find #\- (symbol-name interface-symbol)) '#:-p '#:p))))
      `((defun ,pred-symbol (instance)
          (interface-instance-p instance (gir:nget ,namespace ,class)))
        (deftype ,interface-symbol ()
          '(satisfies ,pred-symbol))))))

(defparameter +getter-pattern-1-base+ "(?:(.+?(?=\\bIS\\b))?(?:IS-)?(.+))") ; xxx_is_xxx, xxx

(defparameter +getter-pattern-1+ (format nil "(?:(.*?(?=\\b(?:GET|IS)\\b))(?:GET-|IS-)~A)" +getter-pattern-1-base+)) ; get_xxx_is_xxx, get_xxx, is_xxx, get_is_xxx

(defparameter +getter-pattern-2+ "(.*?(?=\\b(?:HAS|SHOULD|CAN)\\b)(?:HAS|SHOULD|CAN)-.+)") ; xxx_should_xxx, xxx_has_xxx, has_xxx, should_xxx

(defparameter +getter-pattern+ (format nil "(?:~A|~A)" +getter-pattern-1+ +getter-pattern-2+))

(defparameter +setter-pattern+ (format nil "(?:SET-(?:~A|~A))" +getter-pattern-1-base+ +getter-pattern-2+))

(defparameter +constructor-pattern+ "^(NEW|CREATE)(-WITH|-FROM|-FOR|$)?(-.+|$)")

(defun scan-to-string (regex target-string)
  (multiple-value-bind (match-string groups) (ppcre:scan-to-strings regex target-string)
    (when (and match-string (= (length match-string) (length target-string)))
      (loop :with string := (make-string (loop :for group :across groups :summing (length group)))
            :for i := 0 :then (+ i (length group))
            :for group :across groups
            :if group
              :do (loop :for char :across group
                        :for j :from 0
                        :do (setf (aref string (+ i j)) char))
            :finally (return string)))))

(defun transform-method-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore namespace))
  (catch 'skip
    (let* ((info (gir::info-of desc))
           (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
           (symbol (intern name))
           (args (callable-desc-argument-names desc))
           (arg-types (mapcar #'gir:type-desc-of (gir:arguments-desc-of desc)))
           (ret-types (mapcar #'gir:type-desc-of (gir:returns-desc-of desc)))
           (class-name (or (quoted-name-symbol class) (camel-case->lisp-symbol class)))
           (proc-arg-fn (loop :for (arg-text arg-len) :on args ; (const char* text, int len, ...) -> (text ... &aux (len (length text)))
                              :for (arg-text-type arg-len-type) :on arg-types
                              :for i :from 0
                              :when (and (eql arg-len-type 'integer)
                                         (eql arg-text-type 'string)
                                         (member (symbol-name arg-len) '("LENGTH" "LEN") :test #'string-equal))
                                :return (lambda (args)
                                          (loop :for arg :in args
                                                :for j :from 0
                                                :when (/= (1+ i) j)
                                                  :collect arg :into result-args
                                                :finally (return `(,@result-args &aux (,arg-len (length ,arg-text))))))
                              :finally (return #'identity)))
           (proc-ret-fn (if (and (eq (car ret-types) :void) (cdr ret-types))
                            (lambda (body)
                              (let ((syms (loop :for tpe :in ret-types :collect (gensym))))
                                `(multiple-value-bind ,syms ,body
                                   (declare (ignore ,(car syms)))
                                   (values . ,(cdr syms)))))
                            #'identity)))
      (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
        `(defun ,name-symbol (instance ,@args)
           ,(funcall proc-ret-fn `(gir:invoke (instance ',symbol) ,@args)))
        (cond
          ((and (not args)
                (when-let ((name (scan-to-string +getter-pattern+ name)))
                  `(defun ,(intern (format nil (if (equal ret-types '(boolean)) "~A-~A-P" "~A-~A") class-name name)) (instance)
                     ,(funcall proc-ret-fn `(gir:invoke (instance ',symbol)))))))
          ((and args
                (when-let ((name (scan-to-string +setter-pattern+ name)))
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name name))) (value instance)
                     (,@(if (cdr args) `(destructuring-bind ,args value) `(symbol-macrolet ((,(car args) value))))
                      ,(funcall proc-ret-fn `(gir:invoke (instance ',symbol) ,@args)))))))
          (t `(defun ,(intern (format nil "~A-~A" class-name name)) (instance ,@(funcall proc-arg-fn args))
                ,(funcall proc-ret-fn `(gir:invoke (instance ',symbol) ,@args)))))))))

(defun transform-constructor-desc (desc &optional (namespace *namespace*) (class *class*))
  (catch 'skip
    (let* ((info (gir::info-of desc))
           (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
           (symbol (intern name))
           (args (callable-desc-argument-names desc))
           (class-name (or (quoted-name-symbol class)
                           (camel-case->lisp-symbol class))))
      (let ((body `(gir:invoke (,namespace ,class ',symbol) ,@args)))
        (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
          (values `(defun ,name-symbol ,args ,body) nil)
          (if-let ((method (ppcre:register-groups-bind (verb prep method) (+constructor-pattern+ name)
                             (declare (ignore verb))
                             (if prep (list prep method) (list method)))))
            (values `(defun ,(intern (format nil "MAKE~A-~A"
                                             (ecase (length method)
                                               (1 (first method))
                                               (2 ""))
                                             class-name))
                         (&key ,@args) ,body)
                    (and method
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
                                                :collect (let ((subst-symbol (intern (lastcar subst-arg-name))))
                                                           `((,subst-symbol) (let ((,(first args) ,subst-symbol)) ,body)))
                                                  :into result
                                              :else
                                                :count t :into no-subst-name-count
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
          :finally (return (values merged-constructors unmergeable-constructors)))))

(defun transform-class-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (catch 'skip
    (let* ((info (gir::info-of desc))
           (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
           (symbol (intern name))
           (args (callable-desc-argument-names desc))
           (arg-types (mapcar #'gir:type-desc-of (gir:arguments-desc-of desc)))
           (ret-types (mapcar #'gir:type-desc-of (gir:returns-desc-of desc)))
           (class-name (or (quoted-name-symbol class)
                           (camel-case->lisp-symbol class)))
           (proc-arg-fn (loop :for (arg-text arg-len) :on args ; (const char* text, int len, ...) -> (text ... &aux (len (length text)))
                              :for (arg-text-type arg-len-type) :on arg-types
                              :for i :from 0
                              :when (and (eql arg-len-type 'integer)
                                         (eql arg-text-type 'string)
                                         (member (symbol-name arg-len) '("LENGTH" "LEN") :test #'string-equal))
                                :return (lambda (args)
                                          (loop :for arg :in args
                                                :for j :from 0
                                                :when (/= (1+ i) j)
                                                  :collect arg :into result-args
                                                :finally (return `(,@result-args &aux (,arg-len (length ,arg-text))))))
                              :finally (return #'identity)))
           (proc-ret-fn (if (and (eq (car ret-types) :void) (cdr ret-types))
                            (lambda (body)
                              (let ((syms (loop :for tpe :in ret-types :collect (gensym))))
                                `(multiple-value-bind ,syms ,body
                                   (declare (ignore ,(car syms)))
                                   (values . ,(cdr syms)))))
                            #'identity)))
      (if-let ((name-symbol (quoted-name-symbol (cons class (gir:info-get-name info)))))
        `(defun ,name-symbol ,args
           (gir:invoke (,namespace ,class ',symbol) ,@args))
        (cond
          ((and (not args)
                (when-let ((name (scan-to-string +getter-pattern+ name)))
                  `(defun ,(intern (format nil (if (equal ret-types '(boolean)) "~A-~A-P" "~A-~A") class-name name)) ()
                     ,(funcall proc-ret-fn `(gir:invoke (,namespace ,class ',symbol)))))))
          ((and args
                (when-let ((name (scan-to-string +setter-pattern+ name)))
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-~A-P" "~A-~A") class-name name))) (value)
                     (,@(if (cdr args) `(destructuring-bind ,args value) `(symbol-macrolet ((,(car args) value))))
                      ,(funcall proc-ret-fn `(gir:invoke (,namespace ,class ',symbol) ,@args)))))))
          (t `(defun ,(intern (format nil "~A-~A" class-name name)) ,(funcall proc-arg-fn args)
                ,(funcall proc-ret-fn `(gir:invoke (,namespace ,class ',symbol) ,@args)))))))))

(defun transform-function-desc (desc &optional (namespace *namespace*) (class *class*))
  (declare (ignore class))
  (catch 'skip
    (let* ((info (gir::info-of desc))
           (name (nstring-upcase (underscores->lisp-name (gir:info-get-name info))))
           (symbol (intern name))
           (args (callable-desc-argument-names desc))
           (arg-types (mapcar #'gir:type-desc-of (gir:arguments-desc-of desc)))
           (ret-type (gir:type-desc-of (car (gir:returns-desc-of desc)))))
      (if-let ((name-symbol (quoted-name-symbol (gir:info-get-name info))))
        `(defun ,name-symbol ,args
           (gir:invoke (,namespace ',symbol) ,@args))
        (cond
          ((and (not args)
                (when-let ((name (scan-to-string +getter-pattern+ name)))
                  `(defun ,(intern (format nil (if (eql ret-type 'boolean) "~A-P" "~A") name)) ()
                     (gir:invoke (,namespace ',symbol))))))
          ((and args
                (when-let ((name (scan-to-string +setter-pattern+ name)))
                  `(defun (setf ,(intern (format nil (if (eql (car arg-types) 'boolean) "~A-P" "~A") name))) (value)
                     (,@(if (cdr args) `(destructuring-bind ,args value) `(symbol-macrolet ((,(car args) value))))
                      (gir:invoke (,namespace ',symbol) ,@args))))))
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
