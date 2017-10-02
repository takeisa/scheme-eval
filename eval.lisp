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

;; define

(defun define-p (exp)
  (tagged-list-p exp 'define))

(defun define-var (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun make-lambda (args body)
  (list 'lambda args body))

;; ex: (define (foo arg1 arg2) body)
(defun define-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (caddr exp))))

(defun eval-define (exp env)
  (let ((frame (first-frame env))
        (var (define-var exp))
        (val (define-value exp)))
    (let ((var-val (assoc var frame)))
      (when var-val
        (error "already ~a variable defined." var))
      (add-binding-to-frame var (m-eval val env) frame)
      'ok)))

;; set!

(defun set-p (exp)
  (tagged-list-p exp 'set!))

(defun set-var (exp)
  (cadr exp))

(defun set-value (exp)
  (caddr exp))

(defun eval-set (exp env)
  (let ((frame (first-frame env))
        (var (set-var exp))
        (val (set-value exp)))
    (let ((var-val (assoc var frame)))
      (unless var-val
        (error "~a variable undefined" var))
      (set-binding-to-frame var val frame)
      'ok)))

;; if

(defun if-p (exp)
  (tagged-list-p exp 'if))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequence (exp)
  (caddr exp))

(defun if-alternative (exp)
  (if (cadddr exp)
      (cadddr exp)
      'false))

(defun eval-if (exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequence exp) env)
      (m-eval (if-alternative exp) env)))

;; lambda

(defun lambda-p (exp)
  (tagged-list-p exp 'lambda))

(defun lambda-params (exp)
  (cadr exp))

(defun lambda-body (exp)
  (caddr exp))

(defun make-closure (exp env)
  (list :closure
        (lambda-params exp)
        (lambda-body exp)
        env))

(defun closure-p (exp)
  (tagged-list-p exp :closure))

(defun closure-params (exp)
  (cadr exp))

(defun closure-body (exp)
  (caddr exp))

(defun closure-env (exp)
  (cadddr exp))

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

(defun add-binding-to-frame (var val frame)
  (setf (cdr frame) (cons (car frame) (cdr frame)))
  (setf (car frame) (cons var val)))

(defun set-binding-to-frame (var val frame)
  (let ((var-val (assoc var frame)))
    (rplacd var-val val)))

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

(defun extend-env-with-assoc (var-val-assoc env)
  (extend-env
   (mapcar #'car var-val-assoc)
   (mapcar #'cdr var-val-assoc) env))

;; apply

(defun m-apply (proc args)
  (cond
    ((primitive-proc-p proc)
     (apply (primitive-proc-body proc) args))
    ((closure-p proc)
     (let ((new-env (extend-env
                     (closure-params proc)
                     args
                     (closure-env proc))))
       (m-eval (closure-body proc) new-env)))
    (t (error "not implemented"))))

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
    ((define-p exp)
     (eval-define exp env))
    ((set-p exp)
     (eval-set exp env))
    ((if-p exp)
     (eval-if exp env))
    ((lambda-p exp)
     (make-closure exp env))
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

(defparameter *var-val-assoc*
  (list
   (cons 'true t)
   (cons 'false nil)))

(defun make-init-env ()
  (extend-env-with-assoc *var-val-assoc*
                         (extend-primitive-proc-env *primitive-proc-assoc* +empty-env+)))

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
    (assert-equal '(123 abc "str") (m-eval '(quote 123 abc "str") env))
    (assert-equal 5 (m-eval '((lambda (x y) (+ x y)) 2 3) env))
    ;; if
    (assert-equal "true" (m-eval '(if true "true" "false") env))
    (assert-equal "false" (m-eval '(if false "true" "false") env))
    (assert-equal nil (m-eval '(if false "true") env))
    ;; define
    (m-eval '(define hello "abcd") env)
    (assert-equal "abcd" (m-eval 'hello env))
    (m-eval '(define (add a b) (+ a b)) env)
    (assert-equal 3 (m-eval '(+ 1 2) env))
    ;; set!
    (m-eval '(define foo "foo") env)
    (assert-equal "foo" (m-eval 'foo env))
    (m-eval '(set! foo "foo2") env)
    (assert-equal "foo2" (m-eval 'foo env))
    ))

(defun test-all ()
  (setf *print-failures* t)
  (let ((result (run-tests :all)))
    (print-errors result)))
