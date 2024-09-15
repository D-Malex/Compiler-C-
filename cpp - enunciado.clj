(declare agrupar)
(declare aplicar-operador-diadico)
(declare aplicar-operador-monadico)
(declare aplicar-operador-triadico)
(declare asignacion)
(declare asignacion?)
(declare asignar-aritmetico)
(declare asignar-aritmetico-ref)
(declare booleano?)
(declare buscar-coincidencias)
(declare buscar-mensaje)
(declare bytecode)
(declare cadena?)
(declare cargar-const-en-tabla)
(declare cargar-en-reg-dest)
(declare cargar-en-ult-reg)
(declare cargar-fn-en-tabla)
(declare cargar-lib-en-tabla)
(declare cargar-ns-en-tabla)
(declare cargar-params-en-tabla)
(declare cargar-var-en-tabla)
(declare compatibles?)
(declare confirmar-no-retorno)
(declare confirmar-retorno)
(declare contexto)
(declare controlar-duplicado)
(declare dar-error)
(declare declaracion-const)
(declare declaracion-fn)
(declare declaracion-include)
(declare declaracion-using)
(declare declarar-mas-fn)
(declare declarar-opcional-param)
(declare dividir)
(declare driver-loop)
(declare dump)
(declare escanear) 
(declare escanear-arch)
(declare estado)
(declare estructura-fn-actual)
(declare expresion)
(declare expresion-aditiva)
(declare expresion-and)
(declare expresion-atomica)
(declare expresion-igualdad)
(declare expresion-multiplicativa)
(declare expresion-relacional)
(declare expresion-unaria)
(declare fixup)
(declare fn-predefinida)
(declare generar)
(declare generar-con-valor)
(declare generar-factor-const-o-var)
(declare hacer-fixup-si-es-main)
(declare identificador?)
(declare inicializar-contexto-global)
(declare inicializar-contexto-local)
(declare interpretar)
(declare listar)
(declare mapa-regs-de-act)
(declare numero?)
(declare boolean?)
(declare palabra-reservada?)
(declare params-args)
(declare parsear)
(declare pasar-a-float)
(declare pasar-a-int)
(declare preparar-mapa-regs-de-act)
(declare procesar-asignacion-llamada)
(declare procesar-declaraciones-fn)
(declare procesar-expresion-endl)
(declare procesar-firma-fn)
(declare procesar-fn-comun)
(declare procesar-fn-con-retorno)
(declare procesar-fn-sin-retorno)
(declare procesar-llamada)
(declare procesar-mas-expresion-aditiva)
(declare procesar-mas-expresion-and)
(declare procesar-mas-expresiones-a-imprimir)
(declare procesar-mas-expresion-igualdad)
(declare procesar-mas-expresion-multiplicativa)
(declare procesar-mas-expresion-relacional)
(declare procesar-mas-expresion-unaria)
(declare procesar-mas-opcional-expresion)
(declare procesar-mas-param)
(declare procesar-opcional-asignacion)
(declare procesar-opcional-declaraciones-const)
(declare procesar-opcional-declaraciones-include) 
(declare procesar-opcional-declaraciones-using)
(declare procesar-opcional-expresiones)
(declare procesar-opcional-llamada-punto)
(declare procesar-opcional-por-ref)
(declare procesar-opcional-proposiciones)
(declare procesar-opciones-include)
(declare procesar-terminal)
(declare procesar-tipo-const)
(declare procesar-tipo-param)
(declare programa)
(declare proposicion)
(declare proposicion?)
(declare proposicion-compuesta)
(declare prox-var)
(declare restaurar-contexto-anterior)
(declare simb-actual)
(declare simb-no-parseados-aun)
(declare simb-ya-parseados)
(declare tipo?)
(declare var-ref?)
(declare verificar-que-sea)
(declare verificar-que-sea-const-o-var)
(declare verificar-que-sea-fn)
(declare verificar-que-sea-string)
(declare verificar-que-sea-var)
(declare ya-declarado-localmente?)
(declare evaluar-ya-declarado-localmente)

(defn spy
  ([x] (do (prn x) (prn) x))
  ([x y] (do (print x) (print ": ") (prn y) (prn) y))
) 

(defn boolean? [valor] (or (= valor true) (= valor false)))

(def asignaciones #{"=" "+=" "-=" "*=" "/=" "%="})

(def palabras-reservadas
  #{"alignas" "alignof" "and" "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept"
    "auto" "bitand" "bitor" "bool" "break" "case" "catch" "char" "char8_t" "char16_t" "char32_t"
    "class" "compl" "concept" "const" "consteval" "constexpr" "constinit" "const_cast" "continue"
    "co_await" "co_return" "co_yield" "decltype" "default" "delete" "do" "double" "dynamic_cast"
    "else" "enum" "explicit" "export" "extern" "false" "float" "for" "friend" "goto" "if"
    "inline" "int" "long" "mutable" "namespace" "new" "noexcept" "not" "not_eq" "nullptr"
    "operator" "or" "or_eq" "private" "protected" "public" "register" "reinterpret_cast" "requires"
    "return" "short" "signed" "sizeof" "static" "static_assert" "static_cast" "struct" "switch"
    "synchronized" "template" "this" "thread_local" "throw" "true" "try" "typedef" "typeid"
    "typename" "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while" "xor" "xor_eq"}
)

(defn driver-loop
   ([]
      (prn)
      (println "Interprete de C++ en Clojure")
      (println "Trabajo Practico de Programacion III - 2024")
      (println)
      (println "Inspirado en: g++ 14.1.0 (Copyright 2024 Free Software Foundation, Inc.)")
      (prn)
      (println "Lista de comandos posibles:")
      (println "AYUDA: volver a este menu")
      (println "SALIR: Finalizar la ejecucion de este interprete")
      (println "ESCAN <archivo>: mostrar los tokens de un programa escrito en C++")
      (println "VIRTU <archivo>: mostrar la RI de un programa escrito en C++")
      (println "INTER <archivo>: interpretar la RI de un programa escrito en C++")
      (prn)
      (driver-loop :iniciado))
   ([status]
      (print "C++> ") (flush)
      (try (let [linea (clojure.string/split (clojure.string/trim (read-line)) #" "),
                 cabeza (clojure.string/upper-case (first linea))]
                (cond
                  (= cabeza "SALIR") 'CHAU
                  (= cabeza "AYUDA") (driver-loop)
                  (= cabeza "ESCAN") (let [nom (second linea)]
                                          (if (not (.exists (clojure.java.io/file nom)))
                                              (do (print "ERROR: ") (println (buscar-mensaje 2) (str (symbol "(") nom (symbol ")"))) (flush) (driver-loop status))
                                              (do (listar (escanear-arch nom)) (driver-loop status))))
                  (= cabeza "VIRTU") (let [nom (second linea)]
                                          (if (not (.exists (clojure.java.io/file nom)))
                                              (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                                              (let [res (parsear (escanear-arch nom))]
                                                   (do (if (= (estado res) :sin-errores)
                                                           (dump (bytecode res)))
                                                       (driver-loop status)))))
                  (= cabeza "INTER") (let [nom (second linea)]
                                          (if (not (.exists (clojure.java.io/file nom)))
                                              (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                                              (let [res (parsear (escanear-arch nom))]
                                                   (do (if (= (estado res) :sin-errores)
                                                           (interpretar (bytecode res) [] 0 [] (mapa-regs-de-act res)))
                                                       (driver-loop status)))))
                  (= cabeza "") (driver-loop status)
                  :else (do (print "ERROR: ") (println (buscar-mensaje 1) (str (symbol "(") (first linea) (symbol ")"))) (flush) (driver-loop status))))
           (catch Exception e (println "ERROR ->" (clojure.string/trim (clojure.string/upper-case (let [msg-err (get (Throwable->map e) :cause)] (if (nil? msg-err) "desconocido" msg-err))))) (driver-loop status))))
)

(defn escanear-arch [nom]
  (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux) (instance? Boolean aux)) aux (symbol %)))
        (remove empty? (with-open [rdr (clojure.java.io/reader nom)]
                                  (flatten (doall (map #(re-seq #"\#include|\<\<|\<\=|\>\=|\=\=|\!\=|\+\=|\-\=|\*\=|\/\=|\%\=|\&\&|\|\||\<|\>|\=|\(|\)|\,|\;|\+|\-|\*|\/|\{|\}|\%|\&|\!|\:|\"[^\"]*\"|\d+\.\d+E[+-]?\d+|\d+\.E[+-]?\d+|\.\d+E[+-]?\d+|\d+E[+-]?\d+|\d+\.\d+|\d+\.|\.\d+|\.|\d+|\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*|\.|\'|\"|\||\#|\$|\@|\[|\]|\?|\^|\\|\~" %) (line-seq rdr)))))))
)

(defn listar
  ([prog] (listar prog 0))
  ([prog tab]
    (if (empty? prog)
        (prn)
        (let [s1 (first prog),
              s2 (second prog)]
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
                                    :else tab))))))
)

(defn buscar-mensaje [cod]
  (case cod
     1 "COMANDO DESCONOCIDO"
     2 "ARCHIVO NO ENCONTRADO"
     3 "SE ENCONTRO PREMATURAMENTE EL FIN DEL ARCHIVO:  EOF"
     4 "SE ESPERABA ABRIR UN CORCHETE ANGULAR:  <"
     5 "SE ESPERABA CERRAR UN CORCHETE ANGULAR:  >"
     6 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS iostream O cmath"
     7 "SE ESPERABA UN PUNTO Y COMA:  ;"
     8 "SE ESPERABA LA PALABRA RESERVADA namespace"
     9 "SE ESPERABA LA PALABRA RESERVADA std"
    10 "SE ESPERABA UN TIPO DE CONSTANTE:  int o DOUBLE"
    11 "SE ESPERABA UN IDENTIFICADOR"
    12 "SE ESPERABA UN IGUAL:  ="
    13 "SE ESPERABA UN NUMERO"
    14 "SE ESPERABA UN TIPO DE RETORNO:  int, double, bool, string O void"
    15 "SE ESPERABA ABRIR UN PARENTESIS:  ("
    16 "SE ESPERABA CERRAR UN PARENTESIS:  )"
    17 "SE ESPERABA UNA COMA O CERRAR UN PARENTESIS:  , O )"
    18 "SE ESPERABA UN TIPO DE PARAMETRO:  int, double, bool O string"
    19 "SE ESPERABA ABRIR UNA LLAVE:  {"
    20 "SE ESPERABA UN PUNTO Y COMA O CERRAR UNA LLAVE:  ; O }"
    21 "SE ESPERABA EL OPERADOR <<"
    22 "SE ESPERABA UN INICIO DE EXPRESION"
    23 "SE ESPERABA UNA COMA:  ,"
    24 "SE ESPERABA LA PALABRA RESERVADA cin"
    25 "SE ESPERABA UN OPERADOR DE ASIGNACION (=, +=, ETC.) O ABRIR UN PARENTESIS:  ("
    26 "SE ESPERABA LA PALABRA RESERVADA substr"
    27 "DECLARACION DUPLICADA DE IDENTIFICADOR DE BIBLIOTECA, NAMESPACE, CONSTANTE O FUNCION"
    28 "IDENTIFICADOR NO DECLARADO"
    29 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE"
    30 "SE ESPERABA UN IDENTIFICADOR DE FUNCION"
    31 "SE ESPERABA UN IDENTIFICADOR DE CONSTANTE O VARIABLE"
    32 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE DE TIPO string"
    33 "SE ESPERABA QUE LA FUNCION RETORNE UN VALOR"
    34 "SE ESPERABA QUE LA FUNCION NO RETORNE NINGUN VALOR"
    35 "TIPOS INCOMPATIBLES"
    36 "FALLO EN UNA OPERACION MONADICA"
    37 "FALLO EN UNA OPERACION DIADICA"
    38 "FALLO EN UNA OPERACION DE ARIDAD 3"
   cod)
)

(defn escanear [amb] 
  (if (= (estado amb) :sin-errores)
      [(let [simb (first (simb-no-parseados-aun amb))]
            (if (nil? simb) 'EOF simb)) (rest (simb-no-parseados-aun amb)) (conj (simb-ya-parseados amb) (simb-actual amb)) (estado amb) (contexto amb) (prox-var amb) (bytecode amb) (mapa-regs-de-act amb)]
      amb)
)

(defn dar-error [amb cod]
  (if (= (estado amb) :sin-errores)
      (do (prn)
          (println "ERROR AL INTERPRETAR EL PROGRAMA!")
          (println "*********************************")
          (prn)
          (listar (simb-ya-parseados amb))
          (prn) (println ">") (println ">>" (buscar-mensaje cod)) (println ">") (prn)
          (pr (simb-actual amb)) (print " ")
          (listar (simb-no-parseados-aun amb)) (prn)
          (flush)
          [(simb-actual amb) '() (simb-ya-parseados amb) cod])
      amb)
)

(defn parsear [tokens]
  (let [simbolo-inicial (first tokens)]
       (if (nil? simbolo-inicial)
           (dar-error ['EOF '() [] :sin-errores] 3)
           (programa [simbolo-inicial (rest tokens) [] :sin-errores [] 0 [] []])))
           ; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode mapa-regs-de-act]
)

(defn simb-actual [amb]
  (amb 0)
)

(defn simb-no-parseados-aun [amb]
  (amb 1)
)

(defn simb-ya-parseados [amb]
  (amb 2)
)

(defn estado [amb]
  (amb 3)
)

(defn contexto [amb]
  (amb 4)
)

(defn prox-var [amb]
  (amb 5)
)

(defn bytecode [amb]
  (amb 6)
)

(defn mapa-regs-de-act [amb]
  (amb 7)
)

(defn booleano? [x]
  (contains? #{"true" "false"} (str x))
)

(defn numero? [x]
  (number? x)
)

(defn cadena? [x]
  (string? x)
)

(defn procesar-terminal [amb x cod-err]
  (if (= (estado amb) :sin-errores)
      (if (or (and (symbol? x) (= (simb-actual amb) x)) (x (simb-actual amb)))
          (escanear amb)
          (dar-error amb cod-err))
      amb)
)

(defn generar 
  ([amb instr]
    (if (= (estado amb) :sin-errores)
        (assoc amb 6 (conj (bytecode amb) instr))
        amb))
  ([amb instr val]
    (if (= (estado amb) :sin-errores)
        (assoc amb 6 (conj (bytecode amb) [instr val]))
        amb))
)

(defn controlar-duplicado [amb]
  (if (= (estado amb) :sin-errores)
      (if (ya-declarado-localmente? (last (simb-ya-parseados amb)) (contexto amb))
          (let [coincidencias (buscar-coincidencias amb),
                tipo (second (last coincidencias))]
               (if (not= 'var tipo)
                   (dar-error amb 27)
                   amb))
          amb)
      amb)
)

(defn buscar-coincidencias
  ([amb] (filter #(= (first %) (last (simb-ya-parseados amb))) (second (contexto amb))))
  ([amb elem] (filter #(= (first %) elem) (second (contexto amb))))
)

(defn verificar-que-sea [amb fn-control]
  (if (= (estado amb) :sin-errores)
      (let [coincidencias (buscar-coincidencias amb)]
           (if (empty? coincidencias)
               (dar-error amb 28)
               (fn-control amb coincidencias)))
      amb)
)

(defn verificar-que-sea-var [amb]
  (verificar-que-sea amb #(if (not (contains? (hash-set 'var 'var-ref) (first (second (last %2)))))
                           (dar-error %1 29)
                           %1))
)

(defn var-ref? [amb]
  (verificar-que-sea amb #(= 'var-ref (first (second (last %2)))))
)

(defn verificar-que-sea-fn [amb]
  (verificar-que-sea amb #(if (not= 'fn (first (second (last %2))))
                           (dar-error %1 30)
                           %1))
)

(defn verificar-que-sea-const-o-var [amb]
  (verificar-que-sea amb #(if (not (contains? (hash-set 'const 'var 'var-ref) (first (second (last %2)))))
                           (dar-error %1 31)
                           %1))
)

(defn verificar-que-sea-string [amb]
  (verificar-que-sea amb #(if (not= 'string (second (second (last %2))))
                           (dar-error %1 32)
                           %1))
)

(defn cargar-fn-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (let [simbolos-ya (simb-ya-parseados amb),
            sin-params (rest (drop-while #(not= % (symbol "(")) (reverse simbolos-ya))),
            nombre (first sin-params),
            tipo-dato [(params-args simbolos-ya) (second sin-params)],
            valor (count (bytecode amb))]
           (assoc amb 4 [((contexto amb) 0)
                         (conj ((contexto amb) 1) [nombre 
                                                   ['fn tipo-dato]
                                                   valor])]))
      amb)
)

(defn cargar-var-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (let [simbolos-ya (simb-ya-parseados amb),
            nombre (last simbolos-ya),
            tipo-dato (last (butlast simbolos-ya)),
            valor (prox-var amb),
            nuevo (assoc (assoc amb 4 [((contexto amb) 0)
                                (conj ((contexto amb) 1) [nombre 
                                                          ['var tipo-dato]
                                                          valor])]) 5 (inc valor)),
            vars (mapa-regs-de-act amb)]
            (assoc nuevo 7 (conj (vec (butlast vars)) (conj (last vars) [tipo-dato nil]))))
      amb)
)

(defn inicializar-contexto-global [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [[0] []])           ; [fronteras  tabla]
      amb)
)

(defn preparar-mapa-regs-de-act [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 7 (zipmap (map first (mapa-regs-de-act amb)) (map vec (map rest (mapa-regs-de-act amb))))) 
      amb)
)

(defn programa [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (inicializar-contexto-global)
          (generar ,,, 'CAL 0)
          (generar ,,, 'HLT)
          (procesar-opcional-declaraciones-include)  
          (procesar-opcional-declaraciones-using)  
          (procesar-opcional-declaraciones-const)
          (procesar-declaraciones-fn)
          (preparar-mapa-regs-de-act))
      amb)
)

(defn procesar-opcional-declaraciones-include [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "#include"))
          (-> amb
              (escanear)
              (declaracion-include)
              (recur))
          amb)
      amb)
)

(defn declaracion-include [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-terminal ,,, '< 4)
          (procesar-opciones-include)
          (controlar-duplicado)
          (cargar-lib-en-tabla)
          (procesar-terminal ,,, '> 5))
      amb)
)

(defn cargar-lib-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [((contexto amb) 0)
                    (conj ((contexto amb) 1) [(last (simb-ya-parseados amb))
                                              ['lib]
                                              0])])
      amb)
)

(defn procesar-opciones-include [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
            iostream (-> amb
                         (escanear))
            cmath    (-> amb
                         (escanear))
            (dar-error amb 6))
      amb)
)

(defn procesar-opcional-declaraciones-using [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'using)
          (-> amb
              (escanear)
              (declaracion-using)
              (procesar-terminal ,,, (symbol ";") 7)
              (recur))
          amb)
      amb)
)

(defn declaracion-using [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-terminal ,,, 'namespace 8)
          (procesar-terminal ,,, 'std 9)
          (controlar-duplicado)
          (cargar-ns-en-tabla))
      amb)
)

(defn cargar-ns-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
      (assoc amb 4 [((contexto amb) 0)
                    (conj ((contexto amb) 1) [(last (simb-ya-parseados amb))
                                              ['ns]
                                              0])])
      amb)
)

(defn procesar-opcional-declaraciones-const [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'const)
          (-> amb
              (escanear)
              (declaracion-const)
              (procesar-terminal ,,, (symbol ";") 7)
              (recur))
          amb)
      amb)
)

(defn declaracion-const [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-tipo-const)
          (procesar-terminal ,,, identificador? 11)
          (controlar-duplicado)
          (procesar-terminal ,,, (symbol "=") 12)
          (procesar-terminal ,,, numero? 13)
          (cargar-const-en-tabla))
      amb)
)

(defn procesar-tipo-const [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
        int (-> amb
                (escanear))
        double (-> amb
                (escanear))
       (dar-error amb 10))
      amb)
)

(defn procesar-declaraciones-fn [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (declaracion-fn)
          (declarar-mas-fn))
      amb)
)

(defn declarar-mas-fn [amb]
  (if (= (estado amb) :sin-errores)
      (if (not= (simb-actual amb) 'EOF)
          (-> amb
              (declaracion-fn)
              (recur))
          amb)
      amb)
)

(defn declaracion-fn [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
            (int double bool string)
                 (-> amb
                     (escanear)
                     (procesar-firma-fn)
                     (procesar-fn-con-retorno))
            void (-> amb
                     (escanear)
                     (procesar-firma-fn)
                     (procesar-fn-sin-retorno))
          (dar-error amb 14))
      amb)
)

(defn procesar-firma-fn [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (assoc ,,, 5 0)
          (procesar-terminal ,,, identificador? 11)
          (controlar-duplicado)
          (procesar-terminal ,,, (symbol "(") 15)
          (declarar-opcional-param)
          (procesar-terminal ,,, (symbol ")") 17))
      amb)
)

(defn declarar-opcional-param [amb]
  (if (= (estado amb) :sin-errores)
      (if (tipo? (simb-actual amb))
          (-> amb
              (procesar-tipo-param)
              (procesar-opcional-por-ref)
              (procesar-terminal ,,, identificador? 11)
              (procesar-mas-param))
          amb)
      amb)
)

(defn procesar-opcional-por-ref [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "&"))
          (-> amb
              (escanear))
          amb)
      amb)
)

(defn procesar-fn-comun [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (cargar-fn-en-tabla)
          (hacer-fixup-si-es-main)
          (inicializar-contexto-local)
          (cargar-params-en-tabla)
          (proposicion-compuesta))
      amb)
)

(defn procesar-fn-con-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-fn-comun)
          (confirmar-retorno)
          (restaurar-contexto-anterior))
      amb)
)

(defn procesar-fn-sin-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-fn-comun)
          (confirmar-no-retorno)
          (restaurar-contexto-anterior)
          (generar ,,, 'RETN))
      amb)
)

(defn procesar-mas-param [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (procesar-tipo-param)
              (procesar-opcional-por-ref)
              (procesar-terminal ,,, identificador? 11)
              (recur))
          amb)
      amb)
)

(defn procesar-tipo-param [amb]
  (if (= (estado amb) :sin-errores)
      (if (tipo? (simb-actual amb))
          (escanear amb)
          (dar-error amb 18))
      amb)
)

(defn cargar-params-en-tabla
  ([amb]
    (if (= (estado amb) :sin-errores)
        (let [simbolos-ya (simb-ya-parseados amb),
              params (params-args simbolos-ya),
              proto-amb (vec (conj (map #(if (empty? %) % [(first %) nil]) params) (count (bytecode amb)))),
              args (map #(conj ['POPARG] %) (range (dec (count params)) -1 -1))]
             (cargar-params-en-tabla (assoc (assoc amb 7 (conj (mapa-regs-de-act amb)
                                                        (if (and (= (count proto-amb) 2) (= (second proto-amb) []))
                                                            [(first proto-amb)]
                                                            proto-amb))) 6 (into (bytecode amb) args))
                                     params))
        amb))
  ([amb params]
    (if (empty? params)
        amb
        (let [param (first params),
              tam (count param)]
             (case tam
               0 (cargar-params-en-tabla amb (rest params))
               2 (cargar-params-en-tabla (let [nombre (second param),
                                               tipo-dato (first param),
                                               valor (prox-var amb)]
                      (assoc (assoc amb 4 [((contexto amb) 0)
                                           (conj ((contexto amb) 1) [nombre 
                                                                     ['var tipo-dato]
                                                                     valor])]) 5 (inc valor))) (rest params))
               3 (cargar-params-en-tabla (let [nombre (last param),
                                               tipo-dato (first param),
                                               valor (prox-var amb)]
                      (assoc (assoc amb 4 [((contexto amb) 0)
                                           (conj ((contexto amb) 1) [nombre 
                                                                     ['var-ref tipo-dato]
                                                                     valor])]) 5 (inc valor))) (rest params))))))
)

(defn hacer-fixup-si-es-main [amb]
  (if (= (estado amb) :sin-errores)
      (let [simbolos-ya (simb-ya-parseados amb),
            firma (take-last 3 simbolos-ya)]
           (if (= firma (list 'main (symbol "(" ) (symbol ")" )))
               (let [bc (bytecode amb)]
                    (assoc amb 6 (assoc bc 0 ['CAL (count bc)])))
               amb))
      amb)
)

(defn confirmar-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (last (bytecode amb)) 'RET)
          amb
          (dar-error amb 33))
      amb)
)

(defn confirmar-no-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (if (not= (last (bytecode amb)) 'RET)
          amb
          (dar-error amb 34))
      amb)
)

(defn tipo? [x]
    (contains? (hash-set 'int 'double 'bool 'string) x)
)

(defn proposicion? [x]
  (or (identificador? x) (tipo? x) (contains? (hash-set 'return 'getline 'cout 'if 'while) x))
)

(defn proposicion-compuesta [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-terminal ,,, (symbol "{") 19)
          (procesar-opcional-proposiciones)
          (procesar-terminal ,,, (symbol "}") 20))
      amb)
)

(defn procesar-opcional-proposiciones [amb]
  (if (= (estado amb) :sin-errores)
      (if (or (proposicion? (simb-actual amb)) (= (simb-actual amb) (symbol "{")))
          (-> amb
              (proposicion)
              (recur))
          amb)
      amb)
)

(defn proposicion [amb]
  (if (= (estado amb) :sin-errores)
      (if (identificador? (simb-actual amb))
          (-> amb
              (escanear)
              (procesar-asignacion-llamada)
              (procesar-terminal ,,, (symbol ";") 7))
          (case (simb-actual amb)
                (int double bool string)
                  (-> amb
                      (escanear)
                      (procesar-terminal ,,, identificador? 11)
                      (cargar-var-en-tabla)
                      (procesar-opcional-asignacion)
                      (procesar-terminal ,,, (symbol ";") 7))
           return (-> amb
                      (escanear)
                      (expresion)
                      (generar ,,, 'RET)
                      (procesar-terminal ,,, (symbol ";") 7))
          getline (-> amb
                      (escanear)
                      (procesar-terminal ,,, (symbol "(") 15)
                      (procesar-terminal ,,, 'cin 24)
                      (procesar-terminal ,,, (symbol ",") 23)
                      (procesar-terminal ,,, identificador? 11)
                      (verificar-que-sea-var)
                      (verificar-que-sea-string)
                      (generar-con-valor ,,, 'IN)
                      (procesar-terminal ,,, (symbol ")") 16)
                      (procesar-terminal ,,, (symbol ";") 7))
             cout (-> amb
                      (escanear)
                      (procesar-terminal ,,, (symbol "<<") 21)
                      (procesar-expresion-endl)
                      (procesar-mas-expresiones-a-imprimir)
                      (procesar-terminal ,,, (symbol ";") 7))
               if (let [primera-fase (-> amb
                                         (escanear)
                                         (expresion))]
                       (if (= (estado primera-fase) :sin-errores)
                           (let [segunda-fase (-> primera-fase
                                         (generar ,,, 'JC (+ 2 (count (bytecode primera-fase))))
                                         (generar ,,, 'JMP '?))]
                                (if (= (estado segunda-fase) :sin-errores)
                                    (let [tercera-fase (-> segunda-fase
                                                           (inicializar-contexto-local)
                                                           (proposicion)
                                                           (restaurar-contexto-anterior))]
                                         (if (= (estado tercera-fase) :sin-errores)
                                             (if (= (simb-actual tercera-fase) 'else)
                                                 (-> tercera-fase
                                                     (escanear)
                                                     (generar ,,, 'JMP '?)
                                                     (inicializar-contexto-local)
                                                     (fixup ,,, (inc (count (bytecode primera-fase))))
                                                     (proposicion)
                                                     (fixup ,,, (count (bytecode tercera-fase)))
                                                     (restaurar-contexto-anterior))
                                                 (fixup tercera-fase (inc (count (bytecode primera-fase)))))
                                             tercera-fase))
                                    segunda-fase))
                           primera-fase))
            while (let [primera-fase (escanear amb),
                        segunda-fase (expresion primera-fase)]
                       (if (= (estado segunda-fase) :sin-errores)
                           (-> segunda-fase
                               (generar ,,, 'JC (+ 2 (count (bytecode segunda-fase))))
                               (generar ,,, 'JMP '?)
                               (inicializar-contexto-local)
                               (proposicion)
                               (generar ,,, 'JMP (count (bytecode primera-fase)))
                               (fixup ,,, (inc (count (bytecode segunda-fase))))
                               (restaurar-contexto-anterior))
                           segunda-fase))
            (proposicion-compuesta amb)))
      amb)
)

(defn asignacion [amb ident puntero? no-puntero puntero]
  (if (not puntero?)
      (-> amb
          (verificar-que-sea-var)
          (escanear)
          (expresion)
          (generar-con-valor ,,, no-puntero ident))
      (-> amb
          (escanear)
          (expresion)
          (generar-con-valor ,,, puntero ident)))
)

(defn procesar-opcional-asignacion [amb]
  (if (= (estado amb) :sin-errores)
      (let [ident (last (simb-ya-parseados amb)),
            puntero? (var-ref? amb)]
           (cond
             (= (simb-actual amb) (symbol "=")) (asignacion amb ident puntero? 'POP 'POPREF)
             (= (simb-actual amb) (symbol "+=")) (asignacion amb ident puntero? 'POPADD 'POPADDREF)
             (= (simb-actual amb) (symbol "-=")) (asignacion amb ident puntero? 'POPSUB 'POPSUBREF)
             (= (simb-actual amb) (symbol "*=")) (asignacion amb ident puntero? 'POPMUL 'POPMULREF)
             (= (simb-actual amb) (symbol "/=")) (asignacion amb ident puntero? 'POPDIV 'POPDIVREF)
             (= (simb-actual amb) (symbol "%=")) (asignacion amb ident puntero? 'POPMOD 'POPMODREF)
             :else amb))
      amb)
)

(defn asignacion? [x]
  (contains? (hash-set (symbol "=") (symbol "+=") (symbol "-=") (symbol "*=") (symbol "/=") (symbol "%=")) x)
)

(defn procesar-asignacion-llamada [amb]
  (if (= (estado amb) :sin-errores)
      (cond
         (asignacion? (simb-actual amb))
           (procesar-opcional-asignacion amb)
         (= (simb-actual amb) (symbol "("))
           (procesar-llamada amb)
         :else (dar-error amb 25))
      amb)
)

(defn procesar-expresion-endl [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'endl)
          (-> amb
              (escanear)
              (generar ,,, 'NL))
          (-> amb
              (expresion)
              (generar ,,, 'OUT)))
      amb)
)

(defn procesar-mas-expresiones-a-imprimir [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "<<"))
          (-> amb
              (escanear)
              (procesar-expresion-endl)
              (recur))
          amb)
      amb)
)

(defn procesar-mas-opcional-expresion [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (procesar-opcional-expresiones)
              (recur))
          amb)
      amb)
)

(defn procesar-opcional-expresiones [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ")"))
          amb
          (-> amb
              (expresion)
              (procesar-mas-opcional-expresion)))
      amb)
)

(defn agrupar [cad]
  (map vec
    (map reverse
      (reverse
        (reduce
          #(if (neg? (first %1))
               (reduced (conj (vec (butlast (second %1))) (vec (butlast (last (second %1))))))
               (cond
                 (= %2 (symbol ")")) [(+ (first %1) 1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]
                 (= %2 (symbol "(")) [(- (first %1) 1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]
                 (and (= %2 (symbol ",")) (zero? (first %1))) [(first %1) (conj (second %1) [])]
                 :else [(first %1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]))
          [0 [[]]] (rest (reverse cad))))))
)

(defn params-args [cad]
  (let [res (agrupar cad)]
       (if (= res '([])) () res))
)

(defn estructura-fn-actual [ca]
  (let [cad (if (= (last ca) (symbol ")")) (vec ca) (conj (vec ca) (symbol ")")))
        res (agrupar cad)]
       (if (= res '([])) () (list (first (take-last (+ 2 (count res) (count (flatten res))) cad)) res)))
)

(defn generar-factor-const-o-var [amb]
  (if (= (estado amb) :sin-errores)
      (let [coincidencia (last (buscar-coincidencias amb)),
            ident (first coincidencia),
            tipo (first (second coincidencia)),
            valor (nth coincidencia 2),
            puntero? (var-ref? amb),

            simbolos-ya (simb-ya-parseados amb),
            anterior (last (butlast simbolos-ya)),
            posterior (simb-actual amb),
            param? (and (contains? #{(symbol "("), (symbol ",")} anterior)
                        (contains? #{(symbol ")"), (symbol ",")} posterior)),

            estruc-fn-actual (if param? (estructura-fn-actual simbolos-ya) ()),
            estruc-fn-declarada (buscar-coincidencias amb (first estruc-fn-actual)),

            params-fn-actual (second estruc-fn-actual),
            params-fn-declarada (first (second (second (first estruc-fn-declarada)))),
            param-ref? (= 3 (count (nth params-fn-declarada (dec (count params-fn-actual)))))]
           (if (= tipo 'const)
               (generar amb 'PUSHFI valor)
               (if puntero?
                   (generar amb 'PUSHREF valor)
                   (if param-ref?
                       (generar amb 'PUSHADDR valor) 
                       (generar amb 'PUSHFM valor)))))      
      amb)
)

(defn procesar-llamada [amb]
  (if (= (estado amb) :sin-errores)
  (let [ident (last (simb-ya-parseados amb))]
      (-> amb
          (verificar-que-sea-fn)
          (escanear)
          (procesar-opcional-expresiones)
          (procesar-terminal ,,, (symbol ")") 17)
          (generar-con-valor ,,, 'CAL ident)
         ))
      amb)
)

(defn procesar-opcional-llamada-punto [amb]
  (if (= (estado amb) :sin-errores)
      (cond
             (= (simb-actual amb) (symbol "("))
               (procesar-llamada amb)
             (= (simb-actual amb) (symbol "."))
               (-> amb
                   (verificar-que-sea-string)
                   (generar-factor-const-o-var)
                   (escanear)
                   (procesar-terminal ,,, 'substr 26)
                   (procesar-terminal ,,, (symbol "(") 15)
                   (expresion)
                   (procesar-terminal ,,, (symbol ",") 23)
                   (expresion)
                   (procesar-terminal ,,, (symbol ")") 17)
                   (generar ,,, 'SUBSTR)
                   )
             :else (-> amb
                       (verificar-que-sea-const-o-var)
                       (generar-factor-const-o-var)
                       ))
      amb)
)

(defn fn-predefinida [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (escanear)
          (procesar-terminal ,,, (symbol "(") 15)
          (expresion)
          (procesar-terminal ,,, (symbol ")") 16))
      amb)
)

(defn expresion-atomica [amb]
  (if (= (estado amb) :sin-errores)
      (cond
        (or (numero? (simb-actual amb)) (cadena? (simb-actual amb)) (booleano? (simb-actual amb)))
          (-> amb
              (generar ,,, 'PUSHFI (simb-actual amb))
              (escanear))
        (identificador? (simb-actual amb))
          (-> amb
              (escanear)
              (procesar-opcional-llamada-punto))
          :else
    (case (simb-actual amb) 
     sqrt (-> amb
              (fn-predefinida)
              (generar ,,, 'SQRT))
      sin (-> amb
              (fn-predefinida)
              (generar ,,, 'SIN))
     atan (-> amb
              (fn-predefinida)
              (generar ,,, 'ATAN))
      abs (-> amb
              (fn-predefinida)
              (generar ,,, 'ABS))
     stoi (-> amb
              (fn-predefinida)
              (generar ,,, 'STOI))
     stof (-> amb
              (fn-predefinida)
              (generar ,,, 'STOF))
        (if (= (simb-actual amb) (symbol"("))
          (-> amb
              (escanear)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
          (dar-error amb 22))))
      amb)
)

(defn expresion-unaria  [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         + (-> amb
               (escanear)
               (expresion-atomica))
         - (-> amb
               (escanear)
               (expresion-atomica)
               (generar ,,, 'NEG))
         ! (-> amb
               (escanear)
               (expresion-atomica)
               (generar ,,, 'NOT))
         (expresion-atomica amb))
      amb)
)

(defn procesar-mas-expresion-unaria [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         * (-> amb
               (escanear)
               (expresion-unaria)
               (generar ,,, 'MUL)
               (recur))
         / (-> amb
               (escanear)
               (expresion-unaria)
               (generar ,,, 'DIV)
               (recur))
         % (-> amb
               (escanear)
               (expresion-unaria)
               (generar ,,, 'MOD)
               (recur))
         amb)
      amb)
)

(defn expresion-multiplicativa [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-unaria)
          (procesar-mas-expresion-unaria))
      amb)
)

(defn procesar-mas-expresion-multiplicativa [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         + (-> amb
               (escanear)
               (expresion-multiplicativa)
               (generar ,,, 'ADD)
               (recur))
         - (-> amb
               (escanear)
               (expresion-multiplicativa)
               (generar ,,, 'SUB)
               (recur))
         amb)
      amb)
)

(defn expresion-aditiva [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-multiplicativa)
          (procesar-mas-expresion-multiplicativa))
      amb)
)

(defn procesar-mas-expresion-aditiva [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         <= (-> amb
                (escanear)
                (expresion-aditiva)
                (generar ,,, 'LTE)
                (recur))
          < (-> amb
                (escanear)
                (expresion-aditiva)
                (generar ,,, 'LT)
                (recur))
         >= (-> amb
                (escanear)
                (expresion-aditiva)
                (generar ,,, 'GTE)
                (recur))
          > (-> amb
                (escanear)
                (expresion-aditiva)
                (generar ,,, 'GT)
                (recur))
         amb)
      amb)
)

(defn expresion-relacional [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-aditiva)
          (procesar-mas-expresion-aditiva))
      amb)
)

(defn procesar-mas-expresion-relacional [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         != (-> amb
                (escanear)
                (expresion-relacional)
                (generar ,,, 'NEQ)
                (recur))
         == (-> amb
                (escanear)
                (expresion-relacional)
                (generar ,,, 'EQ)
                (recur))
         amb)
      amb)
)

(defn expresion-igualdad [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-relacional)
          (procesar-mas-expresion-relacional))
      amb)
)

(defn procesar-mas-expresion-igualdad [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "&&"))
          (-> amb
              (escanear)
              (expresion-igualdad)
              (generar ,,, 'AND)
              (recur))
          amb)
      amb)
)

(defn expresion-and [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-igualdad)
          (procesar-mas-expresion-igualdad))
      amb)
)

(defn procesar-mas-expresion-and [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "||"))
          (-> amb
              (escanear)
              (expresion-and)
              (generar ,,, 'OR)
              (recur))
          amb)
      amb)
)

(defn expresion [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (expresion-and)
          (procesar-mas-expresion-and))
      amb)
)

(defn generar-con-valor
  ([amb instr]
    (if (= (estado amb) :sin-errores)
        (let [coincidencias (buscar-coincidencias amb),
              valor (nth (last coincidencias) 2)]
            (generar amb instr valor))
        amb))
  ([amb instr ident]
    (if (= (estado amb) :sin-errores)
        (let [coincidencias (buscar-coincidencias amb ident),
              valor (nth (last coincidencias) 2)]
            (generar amb instr valor))
        amb))
)

(defn aplicar-operador-monadico [op pila]
  (try (vec (conj (vec (butlast pila)) (op (last pila))))
       (catch Exception e (print "ERROR: ") (println (buscar-mensaje 36)) nil))
)

(defn aplicar-operador-diadico [op pila]
  (try (vec (conj (vec (drop-last 2 pila)) (op (last (butlast pila)) (last pila))))
       (catch Exception e (print "ERROR: ") (println (buscar-mensaje 37)) nil))
)

(defn aplicar-operador-triadico [op pila]
  (try (vec (conj (vec (drop-last 3 pila)) (op (last (butlast (butlast pila))) (last (butlast pila)) (last pila))))
       (catch Exception e (print "ERROR: ") (println (buscar-mensaje 38)) nil))
)

(defn asignar-aritmetico [regs-de-act pila reg-actual fetched op]
   (let [direc (second fetched),
         tipo-en-reg (first (reg-actual direc)),
         dato-en-reg (second (reg-actual direc)),
         dato-en-pila (last pila)] 
         (if (compatibles? tipo-en-reg dato-en-pila) 
             (cargar-en-ult-reg regs-de-act direc tipo-en-reg (op dato-en-reg dato-en-pila))
             (do (print "ERROR: ") (println (buscar-mensaje 35)) nil)))
)

(defn asignar-aritmetico-ref [regs-de-act pila reg-actual fetched op]
   (let [direc (second fetched),
         destino (second (reg-actual direc)),
         dato-en-pila (last pila),
         dato-en-dest (second ((regs-de-act (first destino)) (second destino)))
         tipo-en-dest (first ((regs-de-act (first destino)) (second destino)))] 
         (if (compatibles? tipo-en-dest dato-en-pila) 
             (cargar-en-reg-dest regs-de-act destino tipo-en-dest (op dato-en-dest dato-en-pila))
             (do (print "ERROR: ") (println (buscar-mensaje 35)) nil)))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LAS FUNCIONES QUE SIGUEN DEBERAN SER IMPLEMENTADAS PARA QUE ANDE EL INTERPRETE DE C++ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PALABRA-RESERVADA?: Recibe un elemento y devuelve true si es una palabra reservada de C++; si no, false.
; Por ejemplo:
; user=> (palabra-reservada? 'while)  ✅
; true
; user=> (palabra-reservada? 'until)  ✅
; false
; user=> (palabra-reservada? 13)      ✅
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn palabra-reservada? [x]
  (contains? palabras-reservadas x)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDENTIFICADOR?: Recibe un elemento y devuelve true si es un identificador valido en C++; si no, false.
; Por ejemplo:
; user=> (identificador? 'boolean)  ✅
; true
; user=> (identificador? 'bool)     ✅
; false
; user=> (identificador? 'e120)     ✅
; true
; user=> (identificador? '12e0)     ✅
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn identificador? [x]
  (let [res (re-matches #"\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*" (str x))] 
    (and (some? res) (not (palabra-reservada? res)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DUMP: Recibe un vector con instrucciones de la RI y las imprime numeradas a partir de 0. Siempre devuelve nil.
; Por ejemplo:
; user=> (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])   ✅
; 0 [POPREF 2]
; 1 [PUSHFI 2]
; 2 MUL
; 3 [PUSHFI 1]
; 4 ADD
; 5 NEG
; nil
; user=> (dump '[HLT])                                            ✅
; 0 HLT
; nil
; user=> (dump nil)                                               ✅
; 0 nil
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dump [vector]
    (if (nil? vector)
        (println 0 nil)
        (doseq [i (range (count vector))]
            (println (str i " " (nth vector i)))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DIVIDIR: Recibe dos numeros y devuelve su cociente, manteniendo su tipo.
; Por ejemplo:
; user=> (dividir 12 3)     ✅
; 4
; user=> (dividir 12.0 3)   ✅
; 4.0
; user=> (dividir 12 3.0)   ✅
; 4.0
; user=> (dividir 12.0 3.0) ✅
; 4.0
; user=> (dividir 1 2)      ✅
; 0
; user=> (dividir 1 2.0)    ✅
; 0.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dividir [n1 n2]
    (cond
        (or (float? n1) (float? n2)) (float (/ n1 n2))
        (and (integer? n1) (integer? n2)) (int (/ n1 n2))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMPATIBLES?: Recibe dos elementos. Si el primero es un tipo de dato de C++ y el segundo es un valor de Clojure
; de un tipo de dato compatible con el mismo o un vector, devuelve true. Si no, false.
; Por ejemplo:
; user=> (compatibles? 'int 5)          ✅
; true
; user=> (compatibles? 'int 5.0)        ✅
; false
; user=> (compatibles? 'int [5.0])      ✅
; true
; user=> (compatibles? 'double 5.0)     ✅
; true
; user=> (compatibles? 'string "Hola")  ✅
; true
; user=> (compatibles? 'bool true)      ✅
; true
; user=> (compatibles? 'bool 1)         ✅
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tipo [nan]
    (cond
        (integer? nan) 'int
        (float? nan) 'double
        (ratio? nan) 'racional
        (decimal? nan) 'decimal
        (string? nan) 'string
        (symbol? nan) 'character
        (instance? java.lang.Boolean nan) 'bool
        (instance? java.math.BigDecimal nan) 'bigdecimal
        (instance? clojure.lang.PersistentVector nan) 'vector
        (instance? clojure.lang.PersistentList nan) 'list
        (instance? clojure.lang.PersistentArrayMap nan) 'map
        :else 'undefined
    )
)

(defn compatibles? [typ valor]
    (= (tipo valor) typ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PASAR-A-INT: Recibe un elemento. Si puede devolverlo expresado como un entero, lo hace. Si no, lo devuelve intacto.
; Por ejemplo:
; user=> (pasar-a-int "10")   ✅
; 10
; user=> (pasar-a-int 10.0)   ✅
; 10
; user=> (pasar-a-int 10)     ✅
; 10
; user=> (pasar-a-int 'a)     ✅
; a
; user=> (pasar-a-int [10.0]) ✅
; [10.0]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parseInt [nan] (Integer/parseInt nan))

(defn pasar-a-int [nan]
    (cond 
        (= (tipo nan) 'string) (parseInt nan)
        (= (tipo nan) 'double) (int nan)
        :else nan
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PASAR-A-FLOAT: Recibe un elemento. Si puede devolverlo expresado como un numero de punto flotante, lo hace. Si no,
; lo devuelve intacto.
; Por ejemplo:
; user=> (pasar-a-float "10") ✅
; 10.0
; user=> (pasar-a-float 10)   ✅
; 10.0
; user=> (pasar-a-float 10.0) ✅
; 10.0
; user=> (pasar-a-float 'a)   ✅
; a
; user=> (pasar-a-float [10]) ✅
; [10]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parseFloat [nan] (Float/parseFloat nan))

(defn pasar-a-float [nan]
    (cond 
        (= (tipo nan) 'string) (parseFloat nan)
        (= (tipo nan) 'int) (float nan)
        :else nan
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; YA-DECLARADO-LOCALMENTE?: Recibe un identificador y un contexto (un vector formado por dos subvectores: el primero
; con las sucesivas posiciones de inicio de los distintos ambitos/scopes y el segundo con ternas
; [identificador, tipo, valor] resultantes de las declaraciones efectuadas, y devuelve true si el identificador esta
; declarado en el segundo subvector a partir de la ultima posicion guardada en el primer subvector (o sea, en el
; ambito/scope local); si no, devuelve false.
; Por ejemplo:
; user=> (ya-declarado-localmente? 'TRES '[[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3]]])                        ✅
; true
; user=> (ya-declarado-localmente? 'DOS '[[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3]]])                         ✅
; false
; user=> (ya-declarado-localmente? 'TRES '[[0 3] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3] [DOS [const int] 2]]])  ✅
; true
; user=> (ya-declarado-localmente? 'TRES '[[0 4] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3] [DOS [const int] 2]]])  ✅
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ya-declarado-localmente? [ident context]
  (evaluar-ya-declarado-localmente ident (subvec (second context) (last (first context))))
)

(defn evaluar-ya-declarado-localmente [ident context] 
  (if (= ident (first (first context)))
    true
    (if (empty? context)
      false
      (evaluar-ya-declarado-localmente ident (rest context))
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-CONST-EN-TABLA: Recibe un ambiente 
; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode mapa-regs-de-act]
; Si su estado no es :sin-errores, lo devuelve intacto. 
; De lo contrario, lo devuelve modificado con la constante
; declarada como terna [identificador, tipo, valor] en el segundo subvector del vector contexto.
; Por ejemplo:
; user=> (cargar-const-en-tabla [(symbol ";") (list 'int 'main (symbol "(") (symbol ")") (symbol "{") 'return 0 (symbol ";") (symbol "}")) [(symbol "#include") '< 'iostream '> (symbol "#include") '< 'cmath '> 'using 'namespace 'std (symbol ";") 'const 'int 'TRES '= 3] :error '[[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0]]] 0 '[[CAL 0] HLT] []])
; [; (int main ( ) { return 0 ; }) [#include < iostream > #include < cmath > using namespace std ; const int TRES = 3] 8 [[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0]]] 0 [[CAL 0] HLT] []]                                    ✅
;                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
; (cargar-const-en-tabla [(symbol ";") (list 'int 'main (symbol "(") (symbol ")") (symbol "{") 'return 0 (symbol ";") (symbol "}")) [(symbol "#include") '< 'iostream '> (symbol "#include") '< 'cmath '> 'using 'namespace 'std (symbol ";") 'const 'int 'TRES '= 3] :sin-errores '[[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0]]] 0 '[[CAL 0] HLT] []])
; [; (int main ( ) { return 0 ; }) [#include < iostream > #include < cmath > using namespace std ; const int TRES = 3] :sin-errores [[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3]]] 0 [[CAL 0] HLT] []]    ✅
;                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
; [[0] [[iostream [lib] 0] [cmath [lib] 0] [std [ns] 0] [TRES [const int] 3]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cargar-const-en-tabla [amb]
  (if (not= (estado amb) :sin-errores)
    amb
    (assoc amb 4 (conj [(first (contexto amb))] (conj
      (second (contexto amb))
      [
        (nth (simb-ya-parseados amb) (+ (.indexOf (simb-ya-parseados amb) 'const) 2))
        ['const (nth (simb-ya-parseados amb) (inc (.indexOf (simb-ya-parseados amb) 'const)))]
        (nth (simb-ya-parseados amb) (inc (.indexOf (simb-ya-parseados amb) '=)))
      ]
    )))
))


;(let [
;      const-idx (.indexOf (simb-ya-parseados amb) 'const)
;      eq-idx (.indexOf (simb-ya-parseados amb) '=)]
;
;    (when (and (not= const-idx -1) (not= eq-idx -1))
;      (let [
;        tipo (nth (simb-ya-parseados amb) (inc const-idx))
;        identificador (nth (simb-ya-parseados amb) (+ const-idx 2))
;        valor (nth (simb-ya-parseados amb) (inc eq-idx))]
;
;        (assoc amb 4 (conj (second (contexto amb)) [identificador ['const tipo] valor]))))) ;return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INICIALIZAR-CONTEXTO-LOCAL: Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto.
; De lo contrario, lo devuelve modificado con el tamano del segundo subvector del vector contexto agregado al final
; del primer subvector del vector contexto.
; Por ejemplo:
; user=> (inicializar-contexto-local [(symbol "{") (list 'int 'x (symbol "=") 10 (symbol ";")) ['int 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (int x = 10 ;) [int main ( )] 8 [[0] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []]              ✅

; user=> (inicializar-contexto-local [(symbol "{") (list 'int 'x (symbol "=") 10 (symbol ";")) ['int 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (int x = 10 ;) [int main ( )] :sin-errores [[0 1] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []] ✅
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inicializar-contexto-local [amb]
  (if (not= (estado amb) :sin-errores)
    amb
    (assoc amb 4 (conj [(conj (first (contexto amb)) (count (second (contexto amb))))] (second (contexto amb))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESTAURAR-CONTEXTO-ANTERIOR: Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto.
; De lo contrario, lo devuelve modificado, quitando la ultima frontera en el primer subvector del vector contexto,
; y sacando del final del segundo subvector del vector contexto los elementos ubicados a partir de la frontera quitada.
; Por ejemplo:
; user=> (restaurar-contexto-anterior ['EOF () ['int 'main (symbol "(") (symbol ")") (symbol "{") 'int 'x (symbol "=") 10 (symbol ";") 'int 'y (symbol "=") 20 (symbol ";") 'return 'x '+ 'y (symbol ";") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var 'int] 0] ['y ['var 'int] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['int nil] ['int nil]]]])
; [EOF () [int main ( ) { int x = 10 ; int y = 20 ; return x + y ; }] 8 [[0 1] [[main [fn [() ()]] 2] [x [var int] 0] [y [var int] 1]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [int nil] [int nil]]]]      ✅
;
; user=> (restaurar-contexto-anterior ['EOF () ['int 'main (symbol "(") (symbol ")") (symbol "{") 'int 'x (symbol "=") 10 (symbol ";") 'int 'y (symbol "=") 20 (symbol ";") 'return 'x '+ 'y (symbol ";") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var 'int] 0] ['y ['var 'int] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['int nil] ['int nil]]]])
; [EOF () [int main ( ) { int x = 10 ; int y = 20 ; return x + y ; }] :sin-errores [[0] [[main [fn [() ()]] 2]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [int nil] [int nil]]]]                             ✅
;                                                                     ^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn restaurar-contexto-anterior [amb]
  (if (not= (estado amb) :sin-errores)
    amb
    (assoc amb 4 (conj 
      [(vec (remove #(= % (last (first (contexto amb)))) (first (contexto amb))))]
      (subvec (second (contexto amb)) 0 (last (first (contexto amb)))))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FIXUP: Recibe un ambiente y la ubicacion de un JMP ? a corregir en el vector de bytecode. Si el estado no es
; :sin-errores, devuelve el ambiente intacto. De lo contrario, lo devuelve con el JMP corregido con el tamano del
; vector de bytecode.
; Por ejemplo:
; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'cout (symbol "<<") 'x (symbol ";") (symbol "}")) ['int 'main (symbol "(") (symbol ")") (symbol "{") 'int 'x (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var 'int] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['int nil]]]] 4)
; [{ (x = 20 ; } ; cout << x ; }) [int main ( ) { int x ; if false { x = 10 ; } else] 8 [[0 1 2] [[main [fn [() ()]] 2] [x [var int] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP ?] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [int nil]]]]
; [{ (x = 20 ; } ; cout << x ; }) [int main ( ) { int x ; if false { x = 10 ; } else] 8 [[0 1 2] [[main [fn [() ()]] 2] [x [var int] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP ?] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [int nil]]]]              ✅
; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'cout (symbol "<<") 'x (symbol ";") (symbol "}")) ['int 'main (symbol "(") (symbol ")") (symbol "{") 'int 'x (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var 'int] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['int nil]]]] 4)
;                                                                                                                                                                                                                                                                              ^^^^^^^^^^^^                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^: tamano 8          ^ ubicacion de JMP ? en contexto
; [{ (x = 20 ; } ; cout << x ; }) [int main ( ) { int x ; if false { x = 10 ; } else] :sin-errores [[0 1 2] [[main [fn [() ()]] 2] [x [var int] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP 8] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [int nil]]]]   ✅
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fixup [amb jmp-a-modificar]
  (if (not= (estado amb) :sin-errores)
    amb
    (assoc amb 6 (assoc (bytecode amb) 
      jmp-a-modificar ['JMP (count (bytecode amb))])
    )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-EN-ULT-REG: Recibe un vector de registros de activacion, una direccion, un tipo y un valor. Devuelve el
; vector de registros de activacion con el ultimo registro actualizado, en la direccion indicada, con el nuevo tipo
; y el nuevo valor.
; Por ejemplo:
; user=> (cargar-en-ult-reg [[['string "2"] ['int 6] ['int 2] ['int 3] ['int 0]] [['int nil] ['int nil]]] 1 'int 0)
; [[[string "2"] [int 6] [int 2] [int 3] [int 0]] [[int nil] [int 0]]]    ✅
;                                                             ^^^ ^
; user=> (cargar-en-ult-reg [[['string "2"] ['int 6] ['int 2] ['int 3] ['int 0]] [['int nil] ['int 0]]] 0 'double 3)
; [[[string "2"] [int 6] [int 2] [int 3] [int 0]] [[double 3] [int 0]]]   ✅
;                                                   ^^^^^^ ^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cargar-en-ult-reg [vec-regs-de-act direccion tipo valor]
  (assoc vec-regs-de-act (- (count vec-regs-de-act) 1) (assoc (last vec-regs-de-act) direccion [tipo valor]))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-EN-REG-DEST: Recibe un vector de registros de activacion, coordenadas, un tipo y un valor. Devuelve el
; vector de registros de activacion con el registro indicado por las coordenadas actualizado, con el nuevo tipo
; y el nuevo valor.
; 
; Por ejemplo:
; user=> (cargar-en-reg-dest [[['string "2"] ['int 6] ['int 2] ['int 2] ['int 2]] [['int 6] ['int 2] ['int [0 3]] ['int [0 4]] ['int 2] ['int 2]]] [0 4] 'int 0)
; [[[string "2"] [int 6] [int 2] [int 2] [int 0]] [[int 6] [int 2] [int [0 3]] [int [0 4]] [int 2] [int 2]]]      ✅
;                                         ^^^ ^
; user=> (cargar-en-reg-dest [[['string "2"] ['int 6] ['int 2] ['int 2] ['int 0]] [['int 6] ['int 2] ['int [0 3]] ['int [0 4]] ['int 2] ['int 2]]] [0 3] 'double 3)
; [[[string "2"] [int 6] [int 2] [double 3] [int 0]] [[int 6] [int 2] [int [0 3]] [int [0 4]] [int 2] [int 2]]]   ✅
;                                 ^^^^^^ ^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cargar-en-reg-dest [vec-regs-de-act coordenadas tipo valor]
  (assoc vec-regs-de-act (first coordenadas) 
    (assoc (vec-regs-de-act (first coordenadas)) (second coordenadas) [tipo valor]))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LA SIGUIENTE FUNCION DEBERA SER COMPLETADA PARA QUE ANDE EL INTERPRETE DE C++ 
; FALTAN IMPLEMENTAR (todas como llamados recursivos a la funcion interpretar, con recur y argumentos actualizados):
;
; ✅ PUSHFI: PUSH FROM INSTRUCTION. Direccionamiento inmediato. Incrementa cont-prg en 1 y agrega al final de pila el valor del argumento.
; ✅ PUSHFM: PUSH FROM MEMORY. Direccionamiento directo. Incrementa cont-prg en 1 y agrega al final de pila el elemento ubicado en la posicion de reg-actual indicada por el valor del argumento.
; ✅ JMP: Salto incondicional. Cambia cont-prg por el valor del argumento.
; ✅ JC: Salto condicional. Quita el ultimo valor de la pila. Si este es true, cambia cont-prg por el valor del argumento. Si no, incrementa cont-prg en 1.
; ✅ CAL: Llamada a una funcion. Agrega al final de regs-de-act el reg-de-act (proveniente de mapa-regs) indicado por el argumento, cambia cont-prg por el valor del argumento y coloca al final de la pila la direccion de retorno (el valor del argumento incrementado en 1).
; ✅ RETN: Indica el retorno de la llamada a un procedimiento (no funcion). Llama recursivamente a interpretar con valores actualizados de regs-de-act (se elimina el ultimo de ellos), cont-prg (pasa a ser el ultimo valor en la pila) y pila (se quita de ella el nuevo cont-prg).
; ✅ NL: New line. Imprime un salto de linea e incrementa cont-prg en 1.
; ✅ FLUSH: Purga la salida e incrementa cont-prg en 1.
; ✅ POPSUB: Como POPADD, pero resta. 
; ✅ POPMUL: Como POPADD, pero multiplica.
; ✅ POPDIV: Como POPADD, pero divide.
; ✅ POPMOD: Como POPADD, pero calcula el resto de la division.
; ✅ POPSUBREF: Como POPADDREF, pero resta. 
; ✅ POPMULREF: Como POPADDREF, pero multiplica.
; ✅ POPDIVREF: Como POPADDREF, pero divide.
; ✅ POPMODREF: Como POPADDREF, pero calcula el resto de la division.
; ✅ SUB: Como ADD, pero resta. 
; ✅ MUL: Como ADD, pero multiplica.
; ✅ DIV: Como ADD, pero divide.
; ✅ MOD: Como ADD, pero calcula el resto de la division.
; ✅ CHR: Incrementa cont-prg en 1, quita de la pila dos elementos (un string y un indice), selecciona el char del string indicado por el indice y lo coloca al final de la pila.
; ✅ OR: Como ADD, pero calcula el or entre los dos valores.
; ✅ AND: Como ADD, pero calcula el and entre los dos valores.
; ✅ EQ: Como ADD, pero calcula la operacion relacional = entre los dos valores.
; ✅ NEQ: Como ADD, pero calcula la operacion relacional != entre los dos valores.
; ✅ GT:  Como ADD, pero calcula la operacion relacional > entre los dos valores.
; ✅ GTE: Como ADD, pero calcula la operacion relacional >= entre los dos valores.
; ✅ LT: Como ADD, pero calcula la operacion relacional < entre los dos valores.
; ✅ LTE: Como ADD, pero calcula la operacion relacional <= entre los dos valores.
; ✅ NEG: Incrementa cont-prg en 1, quita de la pila un elemento numerico, le cambia el signo y lo coloca al final de la pila.
; ✅ NOT: Incrementa cont-prg en 1, quita de la pila un elemento booleano, lo niega y lo coloca al final de la pila.
; ✅ TOI: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a entero y lo coloca al final de la pila.
; ✅ TOF: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a punto flotante y lo coloca al final de la pila.
; ✅ SQRT: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su raiz cuadrada y la coloca al final de la pila.
; ✅ SIN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su seno y lo coloca al final de la pila.
; ✅ ATAN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su arcotangente y la coloca al final de la pila.
; ✅ ABS: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su valor absoluto y lo coloca al final de la pila.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn interpretar [cod regs-de-act cont-prg pila mapa-regs]
  (let [fetched (cod cont-prg),
        opcode (if (symbol? fetched) fetched (first fetched)),
        reg-actual (last regs-de-act)]
       (case opcode

          ; Detiene la ejecucion (deja de llamar recursivamente a interpretar)
          HLT nil

          ; PUSHFI: PUSH FROM INSTRUCTION. Direccionamiento inmediato. Incrementa cont-prg en 1 y agrega al final de pila el valor del argumento.
          PUSHFI (recur cod regs-de-act (inc cont-prg) (conj pila (second fetched)) mapa-regs)

          ; PUSHFM: PUSH FROM MEMORY. Direccionamiento directo. Incrementa cont-prg en 1 y agrega al final de pila el elemento ubicado en la posicion de reg-actual indicada por el valor del argumento.
          PUSHFM (recur cod regs-de-act (inc cont-prg) (conj pila (reg-actual (second fetched))) mapa-regs)

          ; JMP: Salto incondicional. Cambia cont-prg por el valor del argumento.
          JMP (recur cod regs-de-act (second fetched) pila mapa-regs)

          ; JC: Salto condicional. Quita el ultimo valor de la pila. Si este es true, cambia cont-prg por el valor del argumento. Si no, incrementa cont-prg en 1.
          JC (if (= (pila (- (count pila) 1)) true)
            (recur cod regs-de-act (second fetched) pila mapa-regs)
            (recur cod regs-de-act (inc cont-prg) pila mapa-regs))

          ; CAL: Llamada a una funcion. Agrega al final de regs-de-act el reg-de-act (proveniente de mapa-regs) indicado por el argumento, cambia cont-prg por el valor del argumento y coloca al final de la pila la direccion de retorno (el valor del argumento incrementado en 1).
          CALL (let [
            argumento (second fetched),
            direc-retorno (inc (second fetched)),
            reg-de-act (mapa-regs (second fetched))]
          (recur cod (conj regs-de-act reg-de-act) argumento (conj pila direc-retorno) mapa-regs))

          ; RETN: Indica el retorno de la llamada a un procedimiento (no funcion). Llama recursivamente a interpretar con valores actualizados de regs-de-act (se elimina el ultimo de ellos), cont-prg (pasa a ser el ultimo valor en la pila) y pila (se quita de ella el nuevo cont-prg).
          RETN (recur cod (pop regs-de-act) (last pila) (dec pila) mapa-regs)

          ; NL: New line. Imprime un salto de linea e incrementa cont-prg en 1. != NOP
          NL (do (println) (recur cod regs-de-act (inc cont-prg) pila mapa-regs))

          ; FLUSH: Purga la salida e incrementa cont-prg en 1.
          FLUSH (do (flush) (recur cod regs-de-act (inc cont-prg) pila mapa-regs))

          ; POPSUB: Como POPADD, pero resta.
          POPSUB (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched -)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPMUL: Como POPADD, pero multiplica.
          POPMUL (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched *)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPDIV: Como POPADD, pero divide.
          POPDIV (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched /)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPMOD: Como POPADD, pero calcula el resto de la division.
          POPMOD (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched mod)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPSUBREF: Como POPADDREF, pero resta. 
          POPSUBREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched -)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPMULREF: Como POPADDREF, pero multiplica.
          POPMULREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched *)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPDIVREF: Como POPADDREF, pero divide.
          POPDIVREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched /)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; POPMODREF: Como POPADDREF, pero calcula el resto de la division.
          POPMODREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched mod)]
            (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; SUB: Como ADD, pero resta. 
          SUB (let [res (aplicar-operador-diadico - pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))
          
          ; MUL: Como ADD, pero multiplica.
          MUL (let [res (aplicar-operador-diadico * pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; DIV: Como ADD, pero divide.
          DIV (let [res (aplicar-operador-diadico / pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; MOD: Como ADD, pero calcula el resto de la division.
          MOD (let [res (aplicar-operador-diadico mod pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))
          
          ; CHR: Incrementa cont-prg en 1, quita de la pila dos elementos (un string y un indice), selecciona el char del string indicado por el indice y lo coloca al final de la pila.
          CHR (let [sub-pila-string (filter string? pila), sub-pila-number (filter number? pila),
            elemento-pila-string (last sub-pila-string), elemento-pila-number (last sub-pila-number)]
          (recur cod regs-de-act (inc cont-prg) (conj pila (get elemento-pila-string elemento-pila-number)) mapa-regs))

          ; OR: Como ADD, pero calcula el or entre los dos valores.
          OR (let [res (aplicar-operador-diadico 'or pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; AND: Como ADD, pero calcula el and entre los dos valores.
          AND (let [res (aplicar-operador-diadico 'and pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; EQ: Como ADD, pero calcula la operacion relacional = entre los dos valores.
          EQ (let [res (aplicar-operador-diadico = pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; NEQ: Como ADD, pero calcula la operacion relacional != entre los dos valores.
          NEQ (let [res (aplicar-operador-diadico not= pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; GT:  Como ADD, pero calcula la operacion relacional > entre los dos valores.
          GT (let [res (aplicar-operador-diadico > pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; GTE: Como ADD, pero calcula la operacion relacional >= entre los dos valores.
          GTE (let [res (aplicar-operador-diadico >= pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; LT: Como ADD, pero calcula la operacion relacional < entre los dos valores.
          LT (let [res (aplicar-operador-diadico < pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; LTE: Como ADD, pero calcula la operacion relacional <= entre los dos valores.
          LTE (let [res (aplicar-operador-diadico <= pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

          ; NEG: Incrementa cont-prg en 1, quita de la pila un elemento numerico, le cambia el signo y lo coloca al final de la pila.
          NEG (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (* ultimo-elemento-numerico-pila -1)) mapa-regs))

          ; NOT: Incrementa cont-prg en 1, quita de la pila un elemento booleano, lo niega y lo coloca al final de la pila.
          NOT (let [ultimo-elemento-booleano-pila (last (filter boolean? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (not ultimo-elemento-booleano-pila)) mapa-regs))

          ; TOI: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a entero y lo coloca al final de la pila.
          TOI (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (pasar-a-int ultimo-elemento-numerico-pila)) mapa-regs))
          
          ; TOF: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a punto flotante y lo coloca al final de la pila.
          TOF (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (pasar-a-float ultimo-elemento-numerico-pila)) mapa-regs))

          ; SQRT: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su raiz cuadrada y la coloca al final de la pila.
          SQRT (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (Math/sqrt ultimo-elemento-numerico-pila)) mapa-regs))

          ; SIN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su seno y lo coloca al final de la pila.
          SIN (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (Math/sin ultimo-elemento-numerico-pila)) mapa-regs))
          
          ; ATAN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su arcotangente y la coloca al final de la pila.
          ATAN (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (Math/atan ultimo-elemento-numerico-pila)) mapa-regs))

          ; ABS: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su valor absoluto y lo coloca al final de la pila.
          ABS (let [ultimo-elemento-numerico-pila (last (filter number? pila)),
                    ultima-posicion-pila (- (count pila) 1)]
          (recur cod regs-de-act (inc cont-prg) (assoc pila ultima-posicion-pila (Math/abs ultimo-elemento-numerico-pila)) mapa-regs))

          ; Incrementa cont-prg en 1 y agrega al final de pila un valor proveniente de regs-de-act cuyas coordenadas [#reg-act, offset] provienen de reg-actual.
          ; Por ejemplo: 
          ; fetched: [PUSHREF 3]
          ; reg-actual: [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]
          ;                                         3:^^^^^^^^^^^
          ; destino = [0 4]
          ; regs-de-act: [[[string "5"] [int 23] [int 5] [int 0] [int 23]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]]
          ;                                                    4:^^^^^^^^
          ;             0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          ; pila recibida: [1 150]
          ; pila al llamar recursivamente a interpretar: [1 150 23]
          PUSHREF (let [destino (second (reg-actual (second fetched)))] 
                    (recur cod regs-de-act (inc cont-prg) (conj pila (second ((regs-de-act (first destino)) (second destino)))) mapa-regs))
                    
          ; Incrementa cont-prg en 1 y agrega al final de pila unas coordenadas [#reg-act, offset]
          ; Por ejemplo: 
          ; fetched: [PUSHADDR 3]
          ; reg-actual: [[string "5"] [int 23] [int 5] [int 0] [int 0]]
          ; regs-de-act: [[[string "5"] [int 23] [int 5] [int 0] [int 0]]]
          ;               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ count = 1
          ; pila recibida: [1 23 5]
          ; pila al llamar recursivamente a interpretar: [1 23 5 [0 3]]
          PUSHADDR (recur cod regs-de-act (inc cont-prg) (conj pila [(dec (count regs-de-act)) (second fetched)]) mapa-regs)

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. Si hay un argumento, este indica donde colocar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Si no lo hay, solo incrementa cont-prg en 1 y quita el elemento de la pila.
          ; Por ejemplo: 
          ; fetched: [POP 4]
          ; regs-de-act recibido: [[[string "5"] [int 23] [int 5] [int 0] [int 23]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]]
          ; reg-actual: [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]
          ; pila recibida: [1 150 5]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "5"] [int 23] [int 5] [int 0] [int 23]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int 5] [int nil]]]
          ;                                                                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                                                                                               4:^^^^^^^
          POP (if (symbol? fetched)
                  (recur cod regs-de-act (inc cont-prg) (vec (butlast pila)) mapa-regs)
                  (let [direc (second fetched),
                        tipo-en-reg (first (reg-actual direc)),
                        dato-en-pila (last pila)] 
                       (if (compatibles? tipo-en-reg dato-en-pila) 
                           (recur cod (cargar-en-ult-reg regs-de-act direc tipo-en-reg dato-en-pila) (inc cont-prg) (vec (butlast pila)) mapa-regs)
                           (do (print "ERROR: ") (println (buscar-mensaje 50)) nil))))

          ; Incrementa cont-prg en 1 y quita el penultimo elemento de pila. El argumento indica donde colocar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo: 
          ; fetched: [POPARG 3]
          ; regs-de-act recibido: [[[string "5"] [int 23] [int 5] [int 0] [int 0]] [[int nil] [int nil] [int nil] [int nil] [int nil] [int nil]]]
          ; reg-actual: [[int nil] [int nil] [int nil] [int nil] [int nil] [int nil]]
          ; pila recibida: [1 23 5 [0 3] [0 4] 150]
          ; pila al llamar recursivamente a interpretar: [1 23 5 [0 3] 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "5"] [int 23] [int 5] [int 0] [int 0]] [[int nil] [int nil] [int nil] [int [0 4]] [int nil] [int nil]]]
          ;                                                                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                                                                                   3:^^^^^^^^^^^
          POPARG (let [direc (second fetched),
                    tipo-en-reg (first (reg-actual direc)),
                    dato-en-pila (last (butlast pila))] 
                   (if (compatibles? tipo-en-reg dato-en-pila) 
                       (recur cod (cargar-en-ult-reg regs-de-act direc tipo-en-reg dato-en-pila) (inc cont-prg) (conj (vec (drop-last 2 pila)) (last pila)) mapa-regs)
                       (do (print "ERROR: ") (println (buscar-mensaje 50)) nil)))

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica en reg-actual las coordenadas [#reg-act, offset] donde colocar el elemento en regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo: 
          ; fetched: [POPREF 3]
          ; regs-de-act recibido: [[[string "5"] [int 23] [int 5] [int 0] [int 0]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]]
          ; reg-actual: [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]
          ;                                         3:^^^^^^^^^^^
          ; pila recibida: [1 150 23]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "5"] [int 23] [int 5] [int 0] [int 23]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int nil] [int nil]]]
          ;                                                                                           4:^^^^^^^^
          ;                                                    0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          POPREF (let [direc (second fetched),
                       destino (second (reg-actual direc)),
                       dato-en-pila (last pila),
                       tipo-en-dest (first ((regs-de-act (first destino)) (second destino)))] 
                       (if (compatibles? tipo-en-dest dato-en-pila) 
                           (recur cod (cargar-en-reg-dest regs-de-act destino tipo-en-dest dato-en-pila) (inc cont-prg) (vec (butlast pila)) mapa-regs)
                           (do (print "ERROR: ") (println (buscar-mensaje 50)) nil)))

          ; Incrementa cont-prg en 1, lee un string desde el teclado y lo coloca en el ultimo de los regs-de-act al llamar recursivamente a interpretar
          ; Por ejemplo: 
          ; fetched: [IN 0]
          ; regs-de-act recibido: [[[string ""] [int 23] [int nil] [int nil] [int nil]]]
          ; reg-actual: [[string ""] [int 23] [int nil] [int nil] [int nil]]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "5"] [int 23] [int nil] [int nil] [int nil]]]
          ;                                                     0:^^^^^^^^^^^^
          IN (let [entr (read-line)]
                  (recur cod (cargar-en-ult-reg regs-de-act (second fetched) 'string entr) (inc cont-prg) pila mapa-regs))

          ; Incrementa cont-prg en 1, quita de la pila el contador de argumentos y los argumentos, y coloca al final de la pila un string con estos ultimos (correctamente formateados)
          ; Por ejemplo: 
          ; fetched: FMT
          ; pila recibida: [1 153 "Resto: {}" 9 2]
          ; pila: [1 153 "Resto: 9"]
          OUT (let [cant-args (last pila),
                    args (take-last cant-args (butlast pila))]
                   (do (if (pos? cant-args) (apply printf args))
                       (recur cod regs-de-act (inc cont-prg) (vec (drop-last (+ cant-args 1) pila)) mapa-regs)))

          ; Indica el retorno de la llamada a una funcion (no procedimiento). Llama recursivamente a interpretar con valores actualizados de regs-de-act (se elimina el ultimo de ellos), cont-prg (pasa a ser el penultimo valor en la pila) y pila (se quita de ella el nuevo cont-prg).
          ; Por ejemplo: 
          ; fetched: RET
          ; regs-de-act recibido: [[[string "15"] [int 12] [int 15]] [[int 3] [int 3]]]
          ; cont-prg recibido: 40
          ; pila recibida: [1 "{} es el MCD entre " 81 3]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "15"] [int 12] [int 15]]]
          ; cont-prg al llamar recursivamente a interpretar: 81
          ; pila al llamar recursivamente a interpretar: [1 "{} es el MCD entre " 3]
          RET (recur cod (vec (butlast regs-de-act)) (last (butlast pila)) (vec (conj (vec (drop-last 2 pila)) (last pila))) mapa-regs)


          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica donde sumar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo: 
          ; fetched: [POPADD 2]
          ; regs-de-act recibido: [[[string "6"] [int 6] [int 5]]]
          ; reg-actual: [[string "6"] [int 6] [int 5]]
          ; pila recibida: [1 2]
          ; pila al llamar recursivamente a interpretar: [1]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "6"] [int 6] [int 7]]]
          ;                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                          2:^^^^^^^
          POPADD (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched +)]
                      (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica en reg-actual las coordenadas [#reg-act, offset] donde sumar el elemento en regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo: 
          ; fetched: [POPADDREF 2]
          ; regs-de-act recibido: [[[string "5"] [int 23] [int 5] [int 0] [int 3]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int 5] [int 20]]]
          ; reg-actual: [[int 23] [int 5] [int [0 3]] [int [0 4]] [int 5] [int 20]]
          ;                             2:^^^^^^^^^^^
          ; pila recibida: [1 150 1]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[string "5"] [int 23] [int 5] [int 1] [int 3]] [[int 23] [int 5] [int [0 3]] [int [0 4]] [int 5] [int 20]]]
          ;                                                                                   3:^^^^^^^
          ;                                                    0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          POPADDREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched +)]
                         (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; Incrementa cont-prg en 1, quita de la pila dos elementos, calcula su suma y la coloca al final de la pila 
          ; fetched: ADD
          ; pila recibida: [1 0 0 3 4]
          ; pila al llamar recursivamente a interpretar: [1 0 0 7]
          ADD (let [res (aplicar-operador-diadico + pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs)))

       )
  )
)

true