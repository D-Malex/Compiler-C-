; LOADS
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "expression.clj")
(load-file "functions.clj")
(load-file "miscellany.clj")
(load-file "programa.clj")
(load-file "proposition.clj")


; DECLARES
(declare tipo?)
(declare proposicion?)
(declare numero?)  
(declare identificador?)
(declare cadena?) 
(declare asignacion?)                            
(declare booleano?) 
(declare palabra-reservada?)


; FUNCTIONS
(defn booleano? [x]
  (contains? #{"true" "false"} (str x))
)

(defn numero? [x]
  (number? x)
)

(defn identificador? [x]
  (let [res (re-matches #"\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*" (str x))] 
    (and (some? res) (not (palabra-reservada? res)))
  )
)

(defn cadena? [x]
  (string? x)
)

(defn tipo? [x]
  (contains? (hash-set 'int 'double 'bool 'string) x)
)

(defn proposicion? [x]
  (or (identificador? x) (tipo? x) (contains? (hash-set 'return 'getline 'cout 'if 'while) x))
)

(defn palabra-reservada? [x]
  (contains? palabras-reservadas x)
)

(defn asignacion? [x]
  (contains? asignaciones (str x))
)