(require
  hyrule.collections [assoc]
  hyrule.control [unless])
(import
  functools [partial]
  itertools [islice]
  collections [OrderedDict]
  hy.reserved
  hyrule.collections [coll? prewalk walk]
  hyrule.macrotools [macroexpand-all])


(defn call? [form]
  "Checks whether form is a non-empty hy.models.Expression"
  (and (isinstance form hy.models.Expression)
       form))

(defn by2s [x]
  #[[Returns the given iterable in pairs.
  (list (by2s (range 6))) => [(, 0 1) (, 2 3) (, 4 5)] #]]
  (setv x (iter x))
  (while True
    (try
      (yield (, (next x) (next x)))
      (except [StopIteration]
        (break)))))



(setv _mangled-core-macros (frozenset
  (map hy.mangle (hy.reserved.macros))))


(defn lambda-list [form]
  "splits a fn argument list into sections based on &-headers.

  returns an OrderedDict mapping headers to sublists.
  Arguments without a header are under None.
  "
  (setv headers ['unpack-iterable '* 'unpack-mapping]
        sections (OrderedDict [(, None [])])
        vararg-types {'unpack-iterable (hy.models.Symbol "#*")
                      'unpack-mapping (hy.models.Symbol "#**")}
        header None)
  (for [arg form]
    (cond
      [(in arg headers)
       (do (setv header arg)
           (assoc sections header [])
           ;; Don't use a header more than once. It's the compiler's problem.
           (.remove headers header))]

      [(and (isinstance arg hy.models.Expression) (in (get arg 0) headers))
       (do (setv header (get arg 0))
           (assoc sections header [])
           ;; Don't use a header more than once. It's the compiler's problem.
           (.remove headers header)
           (.append (get sections header) arg))]

      [True (.append (get sections header) arg)]))
  sections)


(defn symbolexpand [form expander
                    [protected (frozenset)]
                    [quote-level 0]]
  (.expand (SymbolExpander form expander protected quote-level)))

(defclass SymbolExpander[]

  (defn __init__ [self form expander protected quote-level]
    (setv self.form form
          self.expander expander
          self.protected protected
          self.quote-level quote-level))

  (defn expand-symbols [self form [protected None] [quote-level None]]
    (if (is protected None)
        (setv protected self.protected))
    (if (is quote-level None)
        (setv quote-level self.quote-level))
    (symbolexpand form self.expander protected quote-level))

  (defn traverse [self form [protected None] [quote-level None]]
    (if (is protected None)
        (setv protected self.protected))
    (if (is quote-level None)
        (setv quote-level self.quote-level))
    (walk (partial symbolexpand
                   :expander self.expander
                   :protected protected
                   :quote-level quote-level)
          (fn [x] x)
          form))

  ;; manages quote levels
  (defn +quote [self [x 1]]
    `(~(self.head) ~@(self.traverse (self.tail)
                                    :quote-level (+ self.quote-level x))))

  (defn handle-dot [self]
    `(. ~(self.expand-symbols (get (self.tail) 0))
        ~@(walk (fn [form]
                  (if (isinstance form hy.models.Symbol)
                      form  ; don't expand attrs
                      (self.expand-symbols form)))
                (fn [x] x)
                (cut (self.tail) 1 None))))

  (defn head [self]
    (get self.form 0))

  (defn tail [self]
    (cut self.form 1 None))

  (defn handle-except [self]
    (setv tail (self.tail))
    ;; protect the "as" name binding the exception
    `(~(self.head) ~@(self.traverse tail (| self.protected
                                            (if (and tail (= (len (get tail 0)) 2))
                                                #{(get tail 0 0)}
                                                #{})))))

  (defn handle-match [self]
    ;; protect name bindings from match patterns
    (setv [expr #* cases] (self.tail)
          new-expr (self.expand-symbols expr)
          new-cases [])
    (defn traverse-clauses [args]
      (unless args
        (return))
      (setv a (next (islice args 1 None) None)
            index (cond
                    [(and (= a :as) (= (get args 3) :if)) 6]
                    [(in a (, :as :if)) 4]
                    [True 2])
            [clause more] [(cut args None index) (cut args index None)]
            protected #{}
            [pattern #* body] clause)

      (when (= (get body 0) :as)
        (protected.add (get body 1)))

      (defn handle-match-symbol [form]
        (if (in "." form)
            (self.expand-symbols form)
            (do
              (.add protected form)
              form)))

      (defn handle-match-call [form]
        (setv head (get form 0))
        (setv tail (cut form 1 None))
        (cond
          [(= head '.) (self.expand-symbols form)]
          [True `(~head ~@(traverse-pattern tail))]))

      (defn handle-pattern-form [form]
        (cond
          [(and (isinstance form hy.models.Symbol) (!= form '_))
            (handle-match-symbol form)]
          [(call? form)
            (handle-match-call form)]
          [(coll? form)
            (traverse-pattern form)]
          [True
            form]))

      (defn traverse-pattern [pattern]
        (walk handle-pattern-form (fn [x] x) pattern))

      (setv new-pattern (handle-pattern-form pattern))
      (setv new-body (self.expand-symbols body (| protected self.protected)))
      (.extend new-cases [new-pattern #* new-body])
      (traverse-clauses more))
    (traverse-clauses cases)
    `(~(self.head) ~new-expr ~@new-cases))

  (defn handle-args-list [self]
    (setv protected #{}
          argslist [])
    (for [[header section] (.items (lambda-list (get (.tail self) (if (= (self.head) (hy.models.Symbol "defn")) 1 0))))]
      (unless (in header [None 'unpack-iterable 'unpack-mapping])
          (.append argslist header))
      (cond [(in header [None '*])
             (for [pair section]
               (cond [(coll? pair)
                      (.add protected (get pair 0))
                      (.append argslist
                               `[~(get pair 0)
                                 ~(self.expand-symbols (get pair 1))])]
                     [True
                      (.add protected pair)
                      (.append argslist pair)]))]
            [(in header ['unpack-iterable 'unpack-mapping])
             (.update protected (gfor  [_ b #* _] section  b))
             (.extend argslist section)]))
    (, protected argslist))

  (defn handle-fn [self]
    (setv [protected argslist] (.handle-args-list self))
    (setv defn? (= (.head self) 'defn))
    `(; The operator
      ~(.head self)
      ; The name of the function, in the case of `defn`
      ~@(if defn?
        [(get (.tail self) 0)]
        [])
      ; The lambda list
      ~argslist
      ; The function body
      ~@(self.traverse
        (cut (.tail self) (if defn? 2 1) None)
        (| protected self.protected))))

  ;; don't expand symbols in quotations
  (defn handle-quoted [self]
    (if (call? self.form)
        (cond [(in (self.head) '[unquote unquote-splice]) (self.+quote -1)]
              [(= (self.head) 'quasiquote) (self.+quote)]
              [True (self.handle-coll)])
        (if (coll? self.form)
            (self.handle-coll)
            (self.handle-base))))

  ;; convert dotted names to the standard special form
  (defn convert-dotted-symbol [self]
    (self.expand-symbols `(. ~@(map hy.models.Symbol (.split self.form '.)))))

  (defn expand-symbol [self]
    (if (not-in self.form self.protected)
        (self.expander self.form)
        (self.handle-base)))

  (defn handle-symbol [self]
    (if (and self.form
             (not (.startswith self.form '.))
             (in '. self.form))
        (self.convert-dotted-symbol)
        (self.expand-symbol)))

  (defn handle-global [self]
    (.update self.protected (set (self.tail)))
    (self.handle-base))

  (defn handle-defclass [self]
    ;; don't expand the name of the class
    `(~(self.head) ~(get (self.tail) 0)
      ~@(self.traverse (cut (self.tail) 1 None))))

  (defn handle-special-form [self]
    ;; don't expand other special form symbols in head position
    `(~(self.head) ~@(self.traverse (self.tail))))

  (defn handle-base [self]
    self.form)

  (defn handle-coll [self]
    ;; recursion
    (self.traverse self.form))

  ;; We have to treat special forms differently.
  ;; Quotation should suppress symbol expansion,
  ;; and local bindings should shadow those made by let.
  (defn handle-call [self]
    (setv head (get self.form 0))
    (cond
      [(in head '[fn defn]) (self.handle-fn)]
      [(in head '[import
                  require
                  quote
                  eval-and-compile
                  eval-when-compile]) (self.handle-base)]
      [(= head 'except) (self.handle-except)]
      [(= head '.) (self.handle-dot)]
      [(= head 'defclass) (self.handle-defclass)]
      [(= head 'match) (self.handle-match)]
      [(= head 'quasiquote) (self.+quote)]
        ;; must be checked last!
      [(in (hy.mangle head) _mangled-core-macros)
        (self.handle-special-form)]
        ;; Not a special form. Traverse it like a coll
      [True (self.handle-coll)]))

  (defn expand [self]
    "the main entry point. Call this to do  the expansion"
    (setv form self.form)
    (cond
      [self.quote-level (self.handle-quoted)]
      [(isinstance form hy.models.Symbol) (self.handle-symbol)]
      [(call? form) (self.handle-call)]
      [(coll? form) (self.handle-coll)]
        ;; recursive base case--it's an atom. Put it back.
      [True (self.handle-base)])))

(defmacro smacrolet [bindings #* body]
  "symbol macro let.

  Replaces symbols in body, but only where it would be a valid let binding.
  The bindings pairs the target symbol and the expansion form for that symbol.
  "
  (if (% (len bindings) 2)
      (raise (ValueError "bindings must be paired")))
  (for [k (cut bindings None None 2)]
    (if (not (isinstance k hy.models.Symbol))
      (raise (TypeError "bind targets must be symbols")))
    (if (in '. k)
      (raise (ValueError "binding target may not contain a dot"))))
  (setv bindings (dict (by2s bindings))
        body (macroexpand-all body &compiler))
  (symbolexpand `(do ~@body)
                (fn [symbol]
                  (.get bindings symbol symbol))))

(defmacro let [bindings #* body]
  "sets up lexical bindings in its body

  ``let`` creates lexically-scoped names for local variables.
  A let-bound name ceases to refer to that local outside the ``let`` form.
  Arguments in nested functions and bindings in nested ``let`` forms can shadow these names.

  Examples:
    ::

       => (let [x 5]  ; creates a new local bound to name 'x
       ...  (print x)
       ...  (let [x 6]  ; new local and name binding that shadows 'x
       ...    (print x))
       ...  (print x))  ; 'x refers to the first local again
       5
       6
       5

    Basic assignments (e.g. ``setv``, ``+=``) will update the local variable named by a let binding,
    when they assign to a let-bound name.

    But assignments via ``import`` are always hoisted to normal Python scope, and
    likewise, ``defclass`` will assign the class to the Python scope,
    even if it shares the name of a let binding.

    Use ``importlib.import_module`` and ``type`` (or whatever metaclass) instead,
    if you must avoid this hoisting.

    The ``let`` macro takes two parameters: a list defining *variables*
    and the *body* which gets executed. *variables* is a vector of
    variable and value pairs. ``let`` can also define variables using
    Python's `extended iterable unpacking`_ syntax to destructure iterables::

       => (let [[head #* tail] (, 0 1 2)]
       ...   [head tail])
       [0 [1 2]]

    Do note, however, that let can not destructure into a mutable data type,
    such as ``dicts`` or ``classes``. As such, the following will both raise
    macro expansion errors:

    Unpack into dictionary::

       => (let [x (dict)
       ...      (, a (get x \"y\")) [1 2]]
       ...  [a x])

    Unpack into a class::

       => (let [x (SimpleNamespace)
       ...      [a x.y] [1 2]]
       ...  [a x])

    Like the ``let*`` of many other Lisps, ``let`` executes the variable
    assignments one-by-one, in the order written::

       => (let [x 5
       ...       y (+ x 1)]
       ...   (print x y))
       5 6

    Unlike them, however, each ``(let …)`` form uses only one
    namespace for all its assignments. Thus, ``(let [x 1  x (fn [] x)]
    (x))`` returns a function object, not 1 as you might expect.

  It is an error to use a let-bound name in a ``global`` or ``nonlocal`` form.

  .. _extended iterable unpacking: https://www.python.org/dev/peps/pep-3132/#specification
  "
  (if (% (len bindings) 2)
      (raise (ValueError "let bindings must be paired")))
  (setv g!let (hy.gensym 'let)
        replacements (OrderedDict)
        unpacked-syms (OrderedDict)
        keys []
        values [])
  (defn expander [symbol]
    (.get replacements symbol symbol))

  (defn destructuring-expander [symbol]
    (cond
      [(not (isinstance symbol hy.models.Symbol)) (raise (TypeError "bind targets must be symbol or destructing assignment"))]
      [(in '. symbol) (raise (ValueError "binding target may not contain a dot"))])
    (setv replaced (hy.gensym symbol))
    (assoc unpacked-syms symbol replaced)
    replaced)

  (defn destructuring? [x]
    (or (isinstance x hy.models.List)
        (and (isinstance x hy.models.Expression)
             (= (get x 0) ',))))

  (for [[k v] (by2s bindings)]
    (cond
      [(and (isinstance k hy.models.Symbol) (in '. k))
       (raise (ValueError "binding target may not contain a dot"))]

      [(not (or (isinstance k hy.models.Symbol) (destructuring? k)))
       (raise (TypeError "bind targets must be symbol or iterable unpacking assignment"))])

    (if (destructuring? k)
        (do
          ;; append the setv unpacking form
          (.append keys (symbolexpand (macroexpand-all k &compiler) destructuring-expander))
          (.append values (symbolexpand (macroexpand-all v &compiler) expander))

          ;; add the keys we replaced in the unpacking form into the let
          ;; dict
          (prewalk (fn [x]
                     (cond
                       [(and (isinstance x hy.models.Symbol) (in '. x))
                        (raise (ValueError "bind target may not contain a dot"))]

                       [(and (isinstance x hy.models.Expression)
                             (not-in (get x 0) #{', 'unpack-iterable}))
                        (raise (ValueError "cannot destructure non-iterable unpacking expression"))]

                       [(and (isinstance x hy.models.Symbol) (in x unpacked-syms))
                        (do (.append keys `(get ~g!let ~(hy.unmangle x)))
                            (.append values (.get unpacked-syms x x))
                            (assoc replacements x (get keys -1)))]

                       [True x]))
                   k))

        (do (.append values (symbolexpand (macroexpand-all v &compiler) expander))
            (.append keys `(get ~g!let ~(hy.unmangle k)))
            (assoc replacements k (get keys -1)))))

  `(do
     (setv ~g!let {}
           ~@(sum (zip keys values) (,)))
     ~@(symbolexpand (macroexpand-all body &compiler)
                     expander)))

;; (defmacro macrolet [])
