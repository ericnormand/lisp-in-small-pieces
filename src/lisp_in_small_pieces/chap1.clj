(ns lisp-in-small-pieces.chap1
  (:refer-clojure :exclude [cons extend list])
  (:require
   [clojure.string :as string]))

(def empty-list [:empty-list])

(def the-false-value [:false])

(defn wrong [& args]
  (throw (ex-info (string/join " " args) {})))

(declare eprogn evlis lookup update! cons car cdr pair? make-function null? invoke length set-car! set-cdr! list)

(defn evaluate [e env]
  (cond
   ;; variables
   (symbol? e)
   (lookup e env)

   ;; self-evaluating
   (or (number? e)
       (string? e)
       (char?   e)
       (true?   e)
       (false?  e)
       (vector? e))
   e

   ;; any other non-seqs are errors
   (not (pair? e))
   (wrong "Cannot evaluate" e)

   :else
   (condp = (car e)
     'quote (car (cdr e))
     'if (if (not (identical? the-false-value (evaluate (car (cdr e)) env)))
             (evaluate (car (cdr (cdr e))) env)
             (evaluate (car (cdr (cdr (cdr e)))) env))
     'begin (eprogn (cdr e) env)
     'set! (update! )
     'lambda (make-function (car (cdr e))
                            (cdr (cdr e))
                            env)
     (invoke (evaluate (car e) env)
             (evlis    (cdr e) env)))))

(defn eprogn [exps env]
  (if (pair? exps)
    (do
      (evaluate (car exps) env)
      (recur    (cdr exps) env))
    empty-list))

(defn evlis [exps env]
  (if (pair? exps)
    (cons (evaluate (car exps) env)
          (evlis    (cdr exps) env))
    empty-list))

(defn lookup [id env]
  (if (pair? env)
    (if (= (car (car env)) id)
      (cdr (car env))
      (recur id (cdr env)))
    (wrong "No such binding" id)))

(defn update! [id env value]
  (if (pair? env)
    (if (= (car (car env)) id)
      (do
        (set-cdr! (car env) value)
        value)
      (recur id (cdr env) value))
    (wrong "No such binding" id)))

(def env-init empty-list)

(defn extend [env variables values]
  (cond
   (pair? variables)
   (if (pair? values)
     (cons (cons (car variables) (car values))
           (extend env (cdr variables) (cdr values)))
     (wrong "Too few values"))
   (null? variables)
   (if (null? values)
     env
     (wrong "Too many values"))
   (symbol? variables)
   (cons (cons variables values) env)))

(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defn make-function [variables body env]
  (fn [values]
    (eprogn body (extend env variables values))))

(def env-global env-init)

(defmacro definitial
  ([name]
     `(do
        (def env-global (cons (cons '~name 'void) env-global))
        '~name))
  ([name value]
     `(do
        (def env-global (cons (cons '~name ~value) env-global))
        '~name)))

(defmacro defprimitive [name value arity]
  `(definitial ~name
     (fn [values#]
       (if (= ~arity (length values#))
         (apply ~value values#)
         (wrong "Incorrect arity" [~name values#])))))

(defmacro defpredicate [name value arity]
  `(defprimitive ~name
     (fn [& values#]
       (or (apply ~value values#)
           the-false-value))
     ~arity))

(definitial t true)
(definitial f the-false-value)
(definitial null empty-list)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defpredicate = = 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defpredicate > > 2)
(defpredicate < < 2)
(defprimitive * * 2)
(defpredicate <= <= 2)
(defpredicate >= >= 2)
(defprimitive remainder mod 2)
(defprimitive display prn 1)

;; Exercise 1.1

;; Modify the function `evaluate` so that it becomes a tracer. All
;; function calls should display their arguments and their
;; results. You can well imagine extending such a reimentary tracer to
;; make a step-by-step debugger that could modify the execution path
;; of the program under its control.

(defn evaluate [e env]
  (cond
   ;; variables
   (symbol? e)
   (lookup e env)

   ;; self-evaluating
   (or (number? e)
       (string? e)
       (char?   e)
       (true?   e)
       (false?  e)
       (vector? e))
   e

   ;; any other non-seqs are errors
   (not (pair? e))
   (wrong "Cannot evaluate" e)

   :else
   (condp = (car e)
     'quote (car (cdr e))
     'if (if (not (identical? the-false-value (evaluate (car (cdr e)) env)))
           (evaluate (car (cdr (cdr e))) env)
           (evaluate (car (cdr (cdr (cdr e)))) env))
     'begin (eprogn (cdr e) env)
     'set! (update! )
     'lambda (make-function (car (cdr e))
                            (cdr (cdr e))
                            env)
     (let [args (evlis (cdr e) env)]
       (println (seq args))
       (let [res (invoke (evaluate (car e) env)
                         args)]
         (println res)
         res)))))

;; Exercise 1.2

;; When `evlis` evaluates a list containing only one expression, it
;; has to carry out a useless recursion. Find a way to eliminate that
;; recursion.

(defn evlis [exps env]
  (if (pair? exps)
    (cons (evaluate (car exps) env)
          (if (null? (cdr exps))
            empty-list
            (evlis  (cdr exps) env)))
    empty-list))

;; My note: this is a pointless optimization.

;; Exercise 1.3

;; Suppose we now define the function extend like this:

(defn extend [env names values]
  (cons (cons names values) env))

;; Define the associated functions, `lookup` and `update!`. Compare
;; them with their earlier definitions.

(declare lookup)

(defn lookup* [id vars vals renv]
  (cond
   (= id vars)
   vals
   (pair? vars)
   (if (pair? vals)
     (if (= id (car vars))
       (car vals)
       (recur id (cdr vars) (cdr vals) renv))
     (wrong "Too many vars"))
   (and (null? vars) (pair? vals))
   (wrong "Too many vals")
   :else
   (lookup id renv)))

(defn lookup [id env]
  (if (pair? env)
    (lookup* id (car (car r)) (cdr (car r)) (cdr env))
    (wrong "No such binding" id)))

(defn update!* [id r value renv]
  (cond
   (= id (car r))
   (do
     (set-cdr! r value)
     value)
   (pair? (car r))
   (if (pair? (cdr r))
     (if (= id (car (car r)))
       (do
         (set-car! (cdr r) value)
         value)
       (recur id (cons (cdr (car r)) (cdr (cdr r))) value renv))
     (wrong "Toom many vars"))
   (and (null? vars) (pair? vals))
   (wrong "Too many vals")
   :else
   (update! id renv)))

(defn update! [id env value]
  (if (pair? env)
    (update!* id (car r) value (cdr env))
    (wrong "No such binding" id)))

;; It reminds me of the quote that one should strive for smart data
;; structures and stupid code. It looks like this is stupid data
;; structures and smart code.

;; The symptoms are many: It is longer and more convoluted than the
;; original. It requires mutual recursion, which in Scheme would be
;; compiled away but in Clojure is dangerous. `update!*` conses as it
;; recurses. It is better the original way.

;; Exercise 1.4

;; Another way of implementing shallow binding was suggested by the
;; idea of a `rack` in [SS80]. Instead of each symbol being associated
;; with a field to contain the value of the variable of the same name,
;; there is a stack for that purpose. At any given time, the value of
;; the variable is the value found on top of that stack of associated
;; values. Rewrite the functions `s.make-function`, `s.lookup`, and
;; `s.update!` to take advantage of this new representation.

;; [SS80]:

(defn s_make-function [vars body env]
  )

(defn s_lookup [id env]
  (if (pair? env)
    (if (= id (car (car env)))
      (car (cdr (car env)))
      (recur id (cdr env)))
    (wrong "No such binding" id)))

(defn s_update! [id env value]
  (if (pair? env)
    (if (= id (car (car env)))
      (do
        (set-cdr! (car env) (cons value (cdr (car env))))
        value)
      (recur id (cdr env) value))
    (wrong "No such binding" id)))

;; Exercise 1.7



(definitial list identity)

;; exercise 1.8

(definitial apply
  (fn [values]
    (cond
     (> (length values) 2)
     (let [flat (fn [values]
                  (if (null? (cdr args))
                    (car args)
                    (cons (car args) (flat (cdr args)))))]
       (invoke (car values) (flat (cdr values))))
     (= (length values) 2)
     (invoke (car values) (car (cdr values)))
     :else
     (wrong "Incorrect arity" 'apply))))




;; yes, I want mutable cons cells
(defn cons [a d]
  (doto (object-array 2)
    (aset 0 a)
    (aset 1 d)))

(defn car [c]
  (aget c 0))

(defn cdr [c]
  (aget c 1))

(defn set-car! [c v]
  (aset c 0 v))

(defn set-cdr! [c v]
  (aset c 1 v))

(defn length [c]
  (if (pair? c)
    (+ 1 (length (cdr c)))
    0))

(def array-of-objects-type (Class/forName "[Ljava.lang.Object;"))

(defn pair? [c]
  (= array-of-objects-type (type c)))

(defn null? [x]
  (identical? empty-list x))

(defn clj->cons [x]
  (if (seq? x)
    (if (seq x)
      (cons (clj->cons (first x)) (clj->cons (rest x)))
      empty-list)
    (if (nil? x)
      empty-list
      x)))
