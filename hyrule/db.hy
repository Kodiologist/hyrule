(import
  contextlib [contextmanager]
  sqlite3
  hyrule.collections [by2s])


(defclass AttributeRow [sqlite3.Row]

  #[[A subclass of :class:`sqlite3.Row` that also lets you fetch values of the tuple with attribute names. ::

    (setv db (hy.I.sqlite3.connect ":memory:"))
    (setv db.row-factory AttributeRow)
    (.execute db "create table T(foo text)")
    (.execute db "insert into T values ('hello')")
    (setv [row] (.execute db "select * from T"))
    (print row.foo)  ; => hello

  Real preexisting attributes, like ``keys``, have priority over this syntactic sugar, so you'll still need ``(get row "keys")`` or ``(:keys row)`` to get an element named "keys".]]

  (defn __getattr__ [self name]
    (get self name)))

(defn sqlite-db [
    [database ":memory:"]
    [isolation-level None]
    [row-factory AttributeRow]
    [foreign-keys True]
    #** kwargs]

  #[[Create a connection to a SQLite database, via :func:`sqlite3.connect`, that automatically closes when used as a context manager. ::

       (with [db (sqlite-db "mydatabase.sqlite")]
         (.execute db "create table T(n integer primary key) strict")
         (.execute db "insert into T values (1)"))

     By contrast, ``(with [db (sqlite3.connect …)] …)`` opens and closes a transaction, but leaves the database connection itself open.

     Hyrule's ``sqlite-db`` also has different default behavior from ``sqlite3.connect`` in a few ways, for the sake of convenience. The behavior is set by the following parameters. Any remaining keyword arguments are passed through to ``sqlite3.connect``.

     - ``database``: As ``sqlite3.connect``, but there's now a default argument, which opens an in-memory database.
     - ``isolation-level``: As ``sqlite3.connect``, but the default is ``None``, obtaining the autocommit behavior that's also the default of the ``sqlite3`` command-line program and various other standard interfaces but that Python's ``sqlite3`` library departs from.
     - ``row-factory``: This is assigned to the ``row_factory`` attribute of the connection. The default is :hy:class:`AttributeRow`. Use ``None`` to get the behavior of an unset ``row_factory``.
     - ``foreign-keys``: If true, ``(.execute db "pragma foreign_keys = true")`` is called after creating the connection, enabling `SQLite's foreign-key support <https://sqlite.org/foreignkeys.html>`_.]]

  (_sqlite-db database isolation-level row-factory foreign-keys kwargs))

(defn [contextmanager] _sqlite-db [database isolation-level row-factory foreign-keys kwargs]

  (setv db None)

  (try

    (setv db (hy.I.sqlite3.connect
      :database database
      :isolation-level isolation-level
      #** kwargs))
    (setv db.row-factory row-factory)
    (when foreign-keys
      (.execute db "pragma foreign_keys = true"))
    (yield db)

    (finally
      (when (is-not db None)
        (.close db)))))


(defn sqlq [x]
  "Quote ``x`` as a SQL identifier. ``x`` is stringified and then
  surrounded by double quotes, with any internal double quotes
  duplicated to escape them."
  (+ "\"" (.replace (str x) "\"" "\"\"") "\""))


(defn sql-interpolate [fstring [rest None]]
  (assert (isinstance fstring hy.models.FString))
  (setv params [])
  (setv sql (.join "" (gfor
    part fstring
    (cond
      (isinstance part hy.models.String)
        part
      ; Otherwise, `part` should be an `FComponent`.
      (is-not part.conversion None)
        (raise (ValueError "conversion specifier not allowed"))
      (= (len part) 1) (do
        (.append params (get part 0))
        "?")
      (and (= (len part) 2) (= (get part 1) '"=")) (do
        (.append params (get part 0))
        (+ (sqlq (get part 0)) " = ?"))
      (and (= (len part) 2) (= (get part 1) '"values")) (do
        (assert (= (get part 0) '...))
        (setv [keys values] (zip #* (by2s rest)))
        (.extend params values)
        (.format "({}) values ({})"
          (.join ", " (gfor  k keys  (sqlq k.name)))
          (.join ", " (* ["?"] (len values)))))
      True
         (raise ValueError)))))
  #(sql (tuple params)))

(defmacro sqlexec [db arg1 #* rest]
  (if (= arg1 ':many)
    (setv [f fstring #* rest] ['executemany #* rest])
    (setv [f fstring] ['execute arg1]))
  `((. ~db ~f) ~@(sql-interpolate fstring rest)))
