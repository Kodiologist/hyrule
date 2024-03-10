(import
  hy.compiler [HyASTCompiler calling-module]
  hyrule.iterables [coll? distinct flatten rest]
  hyrule.collections [walk])


(defmacro defmacro/g! [name args #* body]
  "Like `defmacro`, but symbols prefixed with 'g!' are gensymed.

  ``defmacro/g!`` is a special version of ``defmacro`` that is used to
  automatically generate :hy:func:`gensyms <hy.gensym>` for
  any symbol that starts with
  ``g!``.

  For example, ``g!a`` would become ``(hy.gensym \"a\")``."
  (setv syms (list
              (distinct
               (filter (fn [x]
                         (and (hasattr x "startswith")
                              (.startswith x "g!")))
                       (flatten body))))
        gensyms [])
  (for [sym syms]
    (.extend gensyms [sym `(hy.gensym ~(cut sym 2 None))]))

  (setv [docstring body] (if (and (isinstance (get body 0) str)
                                  (> (len body) 1))
                             #((get body 0) (tuple (rest body)))
                             #(None body)))

  `(defmacro ~name [~@args]
     ~docstring
     (setv ~@gensyms)
     ~@body))


(defmacro defmacro! [name args #* body]
  "Like `defmacro/g!`, with automatic once-only evaluation for 'o!' params.

  Such 'o!' params are available within `body` as the equivalent 'g!' symbol.

  Examples:
    ::

       => (defn expensive-get-number [] (print \"spam\") 14)
       => (defmacro triple-1 [n] `(+ ~n ~n ~n))
       => (triple-1 (expensive-get-number))  ; evals n three times
       spam
       spam
       spam
       42

    ::

       => (defmacro/g! triple-2 [n] `(do (setv ~g!n ~n) (+ ~g!n ~g!n ~g!n)))
       => (triple-2 (expensive-get-number))  ; avoid repeats with a gensym
       spam
       42

    ::

       => (defmacro! triple-3 [o!n] `(+ ~g!n ~g!n ~g!n))
       => (triple-3 (expensive-get-number))  ; easier with defmacro!
       spam
       42
  "
  (defn extract-o!-sym [arg]
    (cond (and (isinstance arg hy.models.Symbol) (.startswith arg "o!"))
            arg
          (and (isinstance args hy.models.List) (.startswith (get arg 0) "o!"))
            (get arg 0)))
  (setv os (lfor  x (map extract-o!-sym args)  :if x  x)
        gs (lfor s os (hy.models.Symbol (+ "g!" (cut s 2 None)))))

  (setv [docstring body] (if (and (isinstance (get body 0) str)
                                  (> (len body) 1))
                             #((get body 0) (tuple (rest body)))
                             #(None body)))
  (setv dg (hy.gensym))

  `(do
     (require hyrule.macrotools [defmacro/g! :as ~dg])
     (~dg ~name ~args
       ~docstring
       `(do (setv ~@(sum (zip ~gs ~os) #()))
            ~@~body))))


(defn macroexpand-all [form [ast-compiler None]]
  "Recursively performs all possible macroexpansions in form, using the ``require`` context of ``module-name``.
  `macroexpand-all` assumes the calling module's context if unspecified.
  "
  (setv quote-level 0
        ast-compiler (or ast-compiler (HyASTCompiler (calling-module))))
  (defn traverse [form]
    (walk expand (fn [x] x) form))
  (defn expand [form]
    (nonlocal quote-level)
    ;; manages quote levels
    (defn +quote [[x 1]]
      (nonlocal quote-level)
      (setv head (get form 0))
      (+= quote-level x)
      (when (< quote-level 0)
        (raise (TypeError "unquote outside of quasiquote")))
      (setv res (traverse (cut form 1 None)))
      (-= quote-level x)
      `(~head ~@res))
    (if (and (isinstance form hy.models.Expression) form)
        (cond quote-level
               (cond (in (get form 0) '[unquote unquote-splice])
                       (+quote -1)
                     (= (get form 0) 'quasiquote) (+quote)
                     True (traverse form))
              (= (get form 0) 'quote) form
              (= (get form 0) 'quasiquote) (+quote)
              (= (get form 0) (hy.models.Symbol "require")) (do
               (ast-compiler.compile form)
               (return))
              (in (get form 0) '[except unpack-mapping])
               (hy.models.Expression [(get form 0) #* (traverse (cut form 1 None))])
              True (traverse (hy.macros.macroexpand form ast-compiler.module ast-compiler :result-ok False)))
        (if (coll? form)
            (traverse form)
            form)))
  (expand form))


(defn map-model [x f]
  #[[Recursively apply a callback to some code. The unary function ``f`` is called on the object ``x``, converting it to a :ref:`model <hy:models>` first if it isn't one already. If the return value isn't ``None``, it's converted to a model and used as the result. But if the return value is ``None``, and ``x`` isn't a :ref:`sequential model <hy:hysequence>`, then ``x`` is used as the result instead. ::

     (defn f [x]
       (when (= x 'b)
         'B))
     (map-model 'a f)  ; => 'a
     (map-model 'b f)  ; => 'B

  Recursive descent occurs when ``f`` returns ``None`` and ``x`` is sequential. Then ``map-model`` is called on all the elements of ``x`` and the results are bound up in the same model type as ``x``. ::

    (map-model '[a [b c] d] f)  ; => '[a [B c] d]

  The typical use of ``map-model`` is to write a macro that replaces models of a selected kind, however deeply they're nested in a tree of models. ::

    (defmacro lowercase-syms [#* body]
      "Evaluate `body` with all symbols downcased."
      (hy.I.hyrule.map-model `(do ~@body) (fn [x]
        (when (isinstance x hy.models.Symbol)
          (hy.models.Symbol (.lower (str x)))))))
    (lowercase-syms
      (SETV FOO 15)
      (+= FOO (ABS -5)))
    (print foo)  ; => 20

  That's why the parameters of ``map-model`` are backwards compared to ``map``: in user code, ``x`` is typically a symbol or other simple form whereas ``f`` is a multi-line anonymous function.]]

  (when (not (isinstance x hy.models.Object))
    (setv x (hy.as-model x)))
  (cond
    (is-not (setx value (f x)) None)
      (hy.as-model value)
    (isinstance x hy.models.Sequence)
      ((type x)
        (gfor  elem x  (map-model elem f))
        #** (cond
          (isinstance x hy.models.FString)
            {"brackets" x.brackets}
          (isinstance x hy.models.FComponent)
            {"conversion" x.conversion}
          True
            {}))
    True
      x))


(defmacro with-gensyms [args #* body]
  "Execute `body` with `args` as bracket of names to gensym for use in macros.

  ``with-gensym`` is used to generate a set of :hy:func:`gensyms <hy.gensym>`
  for use in a macro. The following code:

  Examples:
    ::

       => (with-gensyms [a b c]
       ...   ...)

    expands to::

       => (do
       ...   (setv a (hy.gensym)
       ...         b (hy.gensym)
       ...         c (hy.gensym))
       ...   ...)"
  (setv syms [])
  (for [arg args]
    (.extend syms [arg `(hy.gensym '~arg)]))
  `(do
    (setv ~@syms)
    ~@body))


(defreader /
  #[[Sugar for :hy:class:`hy.I`, to access modules without needing to explicitly import them first.
  Unlike ``hy.I``, ``#/`` cannot be used if the module name is only known at runtime.

  Examples:

    Access modules and their elements directly by name:

    ::

      => (type #/ re)
      <class 'module'>
      => #/ os.curdir
      "."
      => (#/ re.search r"[a-z]+" "HAYneedleSTACK")
      <re.Match object; :span #(3 9) :match "needle">

    Like ``hy.I``, separate submodule names with ``/``:

    ::

      => (#/ os/path.basename "path/to/file")
      "file"]]
  (.slurp-space &reader)
  (setv [mod #* ident] (.split (.read-ident &reader) ".")
        imp `(hy.I ~(hy.mangle (.replace mod "/" "."))))
  (if ident
    `(. ~imp ~@(map hy.models.Symbol ident))
    imp))
