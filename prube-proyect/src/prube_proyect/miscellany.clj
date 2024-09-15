; LOADS
(load-file "predicates.clj")
(load-file "ambient.clj")
(laod-file "programa.clj")
(load-file "errors.clj")


; DECLARES
(declare escanear-arch)
(declare escanear)
(declare listar)
(declare parsear)
(declare spy)


; FUNCTIONS
; Escanea un archivo
(defn escanear-arch [nom]
    (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux) (instance? Boolean aux)) aux (symbol %)))
        (remove empty? (with-open [rdr (clojure.java.io/reader nom)]
        (flatten (doall (map #(re-seq #"\#include|\<\<|\<\=|\>\=|\=\=|\!\=|\+\=|\-\=|\*\=|\/\=|\%\=|\&\&|\|\||\<|\>|\=|\(|\)|\,|\;|\+|\-|\*|\/|\{|\}|\%|\&|\!|\:|\"[^\"]*\"|\d+\.\d+E[+-]?\d+|\d+\.E[+-]?\d+|\.\d+E[+-]?\d+|\d+E[+-]?\d+|\d+\.\d+|\d+\.|\.\d+|\.|\d+|\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*|\.|\'|\"|\||\#|\$|\@|\[|\]|\?|\^|\\|\~" %) (line-seq rdr)))))))
)

; Enlista
(defn listar
    ([prog] (listar prog 0))
    ([prog tab]
        (if (empty? prog)
            (prn)
            (let [s1 (first prog), s2 (second prog)]
            (do (cond
                (contains? #{(symbol "using")(symbol "#include")} s2) (prn s1)
                (= s1 (symbol "{")) (do (prn) (print (apply str (repeat tab " "))) (prn s1) (print (apply str (repeat (+ tab 2) " "))))
                (= s2 (symbol "}")) (do (prn s1) (print (apply str (repeat (- tab 2) " "))))
                (and (= s1 (symbol "}")) (not= s2 (symbol ";"))) (do (prn s1) (print (apply str (repeat tab " "))))
                (and (= s1 (symbol ";")) (not= s2 (symbol "{"))) (do (prn s1) (print (apply str (repeat tab " "))))
                :else (do (pr s1) (print " ")))
            
            (recur (rest prog) (cond
                (= s1 (symbol "{")) (+ tab 2)
                (= s2 (symbol "}")) (- tab 2)
                :else tab))))
        )
    )
)

; ESCANEA un simbolo mas
(defn escanear [amb] 
  (if (= (estado amb) :sin-errores)
      [(let [simb (first (simb-no-parseados-aun amb))]
            (if (nil? simb) 'EOF simb)) (rest (simb-no-parseados-aun amb)) (conj (simb-ya-parseados amb) (simb-actual amb)) (estado amb) (contexto amb) (prox-var amb) (bytecode amb) (mapa-regs-de-act amb)]
      amb)
)

; Da inicio al parseo delegando el ambiente al PROGRAMA
(defn parsear [tokens]
    (let [simbolo-inicial (first tokens)]
        (if (nil? simbolo-inicial)
            (dar-error ['EOF '() [] :sin-errores] 3)
            (programa [simbolo-inicial (rest tokens) [] :sin-errores [] 0 [] []])))
            ; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode mapa-regs-de-act]
)

; Evalua si el simbolo es terminal o no
(defn procesar-terminal [amb x cod-err]
  (if (= (estado amb) :sin-errores)
      (if (or (and (symbol? x) (= (simb-actual amb) x)) (x (simb-actual amb)))
          (escanear amb)
          (dar-error amb cod-err))
      amb)
)

; Permite conocer el valor de una variable
(defn spy[x] (do (pr x) (print " ") x))

