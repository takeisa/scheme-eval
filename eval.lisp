;;; SICP metacircular evaluation

(ql:quickload :lisp-unit)

(defpackage :sicp
  (:use :common-lisp :lisp-unit))

(in-package :sicp)

;; utils

(defun tagged-list-p (lst tag)
  (equal (car lst) tag))

;; self-eval

(defun self-eval-p (exp)
  (or (numberp exp)
      (stringp exp)))

(defun variable-p (exp)
  (symbolp exp))

;; quote

(defun quoted-p (exp)
  (tagged-list-p exp 'quote))

(defun quoted-object (exp)
  (assert (quoted-p exp))
  (cdr exp))

;; application

(defun application-p (exp)
  (consp exp))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

(defun primitive-proc-p (exp)
  (tagged-list-p exp :primitive-proc))

(defun primitive-proc-names (lst)
  (mapcar #'car lst))

(defun primitive-proc-funcs (lst)
  (mapcar #'(lambda (proc-bind) (list :primitive-proc (cdr proc-bind))) lst))

(defun extend-primitive-proc-env (proc-assoc env)
  (extend-env
   (primitive-proc-names proc-assoc)
   (primitive-proc-funcs proc-assoc) env))

(defun primitive-proc-body (exp)
  (cadr exp))

;; frame

(defun make-frame (vars vals)
  (unless (= (length vars) (length vals))
    (error "variables and values are not same length."))
  (mapcar #'cons vars vals))

(defun first-frame (env)
  (car env))

(defun parent-env (env)
  (cdr env))

;; env

(defconstant +empty-env+ nil)

(defun lookup-variable-value (var env)
  (when (null env)
    (error "The ~a variable is unbound." var))
  (let ((frame (first-frame env)))
    (let ((var-val (assoc var frame)))
      (if var-val
          (cdr var-val)
          (lookup-variable-value var (parent-env env))))))

(defun extend-env (vars vals env)
  (cons (make-frame vars vals) env))

;; apply

(defun m-apply (proc args)
  (if (primitive-proc-p proc)
      (apply (primitive-proc-body proc) args)
      (error "not implemented")))

;; eval

(defun eval-each (lst env)
  (mapcar #'(lambda (item) (m-eval item env)) lst))

(defun m-eval (exp env)
  (cond
    ((self-eval-p exp) exp)
    ((variable-p exp)
     (lookup-variable-value exp env))
    ((quoted-p exp)
     (quoted-object exp))

    ((application-p exp)
     (m-apply (m-eval (operator exp) env)
              (eval-each (operands exp) env)))
    ))

;; init env

(defparameter *primitive-proc-assoc*
  (list
   (cons '+ #'+)
   (cons '- #'-)
   (cons '* #'*)
   (cons '/ #'/)))

(defun make-init-env ()
  (extend-primitive-proc-env *primitive-proc-assoc* +empty-env+))

;;--------------------
;; test

(define-test self-eval-p
  (assert-eq t (self-eval-p 10))
  (assert-eq t (self-eval-p "abc"))
  (assert-eq nil (self-eval-p (list 1 2))))

(define-test variable-p
  (assert-eq t (variable-p 'foo)))

(define-test application-p
  (assert-eq t (application-p '(+ 1 2)))
  (assert-eq nil (application-p 'a)))

(define-test make-frame
  (assert-equal nil (make-frame '() '()))
  (assert-equal
   '((one . 1)) (make-frame '(one) '(1)))
  (assert-equal
   '((one . 1) (two . 2)) (make-frame '(one two) '(1 2)))
  (assert-error 'simple-error (make-frame '(one two) '(1))))

(define-test extend-env
  (assert-equal '(((e2a . "2a") (e2b . "2b")) ((e1a . "1a")))
                (extend-env '(e2a e2b) '("2a" "2b")
                            (extend-env '(e1a) '("1a") +empty-env+))))

(define-test lookup-variable-value
  (let ((env (extend-env '(e2-1 e2-2) '("v2-1" "v2-2")
                         (extend-env '(e1-1 e1-2) '("v1-1" "v1-2") +empty-env+))))
    (assert-equal "v2-2" (lookup-variable-value 'e2-2 env))
    (assert-equal "v1-1" (lookup-variable-value 'e1-1 env))
    (assert-error 'simple-error (lookup-variable-value 'unbound-var env))))

(define-test eval-each
  (let ((env (extend-env '(e2-1 e2-2) '("v2-1" "v2-2")
                         (extend-env '(e1-1 e1-2) '("v1-1" "v1-2") +empty-env+))))
    (assert-equal '() (eval-each '() env))
    (assert-equal '(1 "one") (eval-each '(1 "one") env))
    (assert-equal '("v1-1" "v2-2") (eval-each '(e1-1 e2-2) env))))

(define-test m-eval
  (let ((env (extend-env '(one two) '(1 2)
                         (make-init-env))))
    (assert-equal 100 (m-eval 100 env))
    (assert-equal "abc" (m-eval "abc" env))
    (assert-equal 2 (m-eval 'two env))
    (assert-equal 6 (m-eval '(+ 1 2 3) env))
    (assert-equal 9 (m-eval '(* (+ 1 2 ) (- 5 2)) env))
    (assert-equal '(123 abc "str") (m-eval '(quote 123 abc "str") env))))

(defun test-all ()
  (setf *print-failures* t)
  (let ((result (run-tests :all)))
    (print-errors result)))
