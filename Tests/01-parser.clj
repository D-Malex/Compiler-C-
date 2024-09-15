; DECLARACIONES
(declare asignacion?)
(declare booleano?)
(declare buscar-mensaje)
(declare bytecode)
(declare cadena?)
(declare contexto)
(declare dar-error)
(declare declaracion-const)
(declare declaracion-fn)
(declare declaracion-include)
(declare declaracion-using)
(declare declarar-mas-fn)
(declare declarar-opcional-param)
(declare driver-loop)
(declare dump)
(declare escanear)
(declare escanear-arch)
(declare estado)
(declare expresion)
(declare expresion-aditiva)
(declare expresion-and)
(declare expresion-atomica)
(declare expresion-igualdad)
(declare expresion-multiplicativa)
(declare expresion-relacional)
(declare expresion-unaria )
(declare identificador?)
(declare interpretar)
(declare listar)
(declare mapa-regs-de-act)
(declare numero?)
(declare palabra-reservada?)
(declare parsear)
(declare procesar-asignacion)
(declare procesar-asignacion-llamada)
(declare procesar-declaraciones-fn)
(declare procesar-declaracion-variable)
(declare procesar-expresion-endl)
(declare procesar-firma-fn)
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
(declare procesar-tipo-variable)
(declare programa)
(declare proposicion)
(declare proposicion?)
(declare proposicion-compuesta)
(declare prox-var)
(declare simb-actual)
(declare simb-no-parseados-aun)
(declare simb-ya-parseados)
(declare spy)
(declare tipo?)

; FUNCIONES

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
      (println "SALIR: volver al REPL de Clojure")
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
    21 "SE ESPERABA UN TIPO DE VARIABLE:  int, double, bool O string"
    22 "SE ESPERABA UN INICIO DE EXPRESION"
    23 "SE ESPERABA UNA COMA:  ,"
    24 "SE ESPERABA LA PALABRA RESERVADA cin"
    25 "SE ESPERABA UN OPERADOR DE ASIGNACION (=, +=, ETC.) O ABRIR UN PARENTESIS:  ("
    26 "SE ESPERABA LA PALABRA RESERVADA substr"
    27 "SE ESPERABA EL OPERADOR <<"
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

(defn identificador? [x]
  (let [res (re-matches #"\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*" (str x))] 
       (and (some? res) (not (palabra-reservada? res))))
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

(defn programa [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (procesar-opcional-declaraciones-include)  
          (procesar-opcional-declaraciones-using)  
          (procesar-opcional-declaraciones-const)
          (procesar-declaraciones-fn))
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
          (procesar-terminal ,,, '> 5))
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
             int (-> amb
                     (escanear)
                     (procesar-firma-fn)
                     (procesar-fn-con-retorno))
          double (-> amb
                     (escanear)
                     (procesar-firma-fn)
                     (procesar-fn-con-retorno))
            bool (-> amb
                     (escanear)
                     (procesar-firma-fn)
                     (procesar-fn-con-retorno))
          string (-> amb
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
          (procesar-terminal ,,, identificador? 11)
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

(defn procesar-fn-con-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (proposicion-compuesta))
      amb)
)

(defn procesar-fn-sin-retorno [amb]
  (if (= (estado amb) :sin-errores)
      (-> amb
          (proposicion-compuesta))
      amb)
)

(defn procesar-mas-param [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol ","))
          (-> amb
              (escanear)
              (declarar-opcional-param)
              (recur))
          amb)
      amb)
)

(defn procesar-opcional-asignacion [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) (symbol "="))
          (let [direc (dec (prox-var amb))]
               (-> amb
                   (escanear)
                   (expresion)))
          amb)
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

(defn procesar-declaracion-variable [amb]
  (if (= (estado amb) :sin-errores)
      (procesar-asignacion amb)
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
              int (-> amb
                      (escanear)
                      (procesar-terminal ,,, identificador? 11)
                      (procesar-declaracion-variable)
                      (procesar-terminal ,,, (symbol ";") 7))
           double (-> amb
                      (escanear)
                      (procesar-terminal ,,, identificador? 11)
                      (procesar-declaracion-variable)
                      (procesar-terminal ,,, (symbol ";") 7))
             bool (-> amb
                      (escanear)
                      (procesar-terminal ,,, identificador? 11)
                      (procesar-declaracion-variable)
                      (procesar-terminal ,,, (symbol ";") 7))
           string (-> amb
                      (escanear)
                      (procesar-terminal ,,, identificador? 11)
                      (procesar-declaracion-variable)
                      (procesar-terminal ,,, (symbol ";") 7))
           return (-> amb
                      (escanear)
                      (expresion)
                      (procesar-terminal ,,, (symbol ";") 7))
          getline (-> amb
                      (escanear)
                      (procesar-terminal ,,, (symbol "(") 15)
                      (procesar-terminal ,,, 'cin 24)
                      (procesar-terminal ,,, (symbol ",") 23)
                      (procesar-terminal ,,, identificador? 11)
                      (procesar-terminal ,,, (symbol ")") 16)
                      (procesar-terminal ,,, (symbol ";") 7))
             cout (-> amb
                      (escanear)
                      (procesar-terminal ,,, (symbol "<<") 27)
                      (procesar-expresion-endl)
                      (procesar-mas-expresiones-a-imprimir)
                      (procesar-terminal ,,, (symbol ";") 7))
               if (let [primera-fase (-> amb
                                         (escanear)
                                         (expresion)
                                         (proposicion))]
                       (if (= (estado primera-fase) :sin-errores)
                           (if (= (simb-actual primera-fase) 'else)
                               (-> primera-fase
                                   (escanear)
                                   (proposicion))
                               primera-fase)
                           primera-fase))
            while (let [primera-fase (escanear amb),
                        segunda-fase (expresion primera-fase)]
                       (if (= (estado segunda-fase) :sin-errores)
                           (-> segunda-fase
                               (proposicion))
                           segunda-fase))
            (proposicion-compuesta amb)))
      amb)
)

(defn procesar-asignacion-llamada [amb]
  (if (= (estado amb) :sin-errores)
      (cond
         (asignacion? (simb-actual amb))
           (procesar-asignacion amb)
         (= (simb-actual amb) (symbol "("))
           (procesar-llamada amb)
         :else (dar-error amb 25))
      amb)
)

(defn procesar-expresion-endl [amb]
  (if (= (estado amb) :sin-errores)
      (if (= (simb-actual amb) 'endl)
          (-> amb
              (escanear))
          (expresion amb))
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

(defn procesar-llamada [amb]
  (if (= (estado amb) :sin-errores)
  (let [ident (last (simb-ya-parseados amb))]
      (-> amb
          (escanear)
          (procesar-opcional-expresiones)
          (procesar-terminal ,,, (symbol ")") 17)
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
                   (escanear)
                   (procesar-terminal ,,, 'substr 26)
                   (procesar-terminal ,,, (symbol "(") 15)
                   (expresion)
                   (procesar-terminal ,,, (symbol ",") 23)
                   (expresion)
                   (procesar-terminal ,,, (symbol ")") 17)
                   )
             :else amb)
      amb)
)

(defn expresion-atomica [amb]
  (if (= (estado amb) :sin-errores)
      (cond
        (numero? (simb-actual amb))
          (-> amb
              (escanear))
        (cadena? (simb-actual amb))
          (-> amb
              (escanear))
        (booleano? (simb-actual amb))
          (-> amb
              (escanear))
        (identificador? (simb-actual amb))
          (-> amb
              (escanear)
              (procesar-opcional-llamada-punto))
          :else
    (case (simb-actual amb) 
     sqrt (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
      sin (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
     atan (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
      abs (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
     stoi (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
     stof (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 15)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 16))
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
               (expresion-atomica))
         ! (-> amb
               (escanear)
               (expresion-atomica))
         (expresion-atomica amb))
      amb)
)

(defn procesar-mas-expresion-unaria [amb]
  (if (= (estado amb) :sin-errores)
      (case (simb-actual amb) 
         * (-> amb
               (escanear)
               (expresion-unaria)
               (recur))
         / (-> amb
               (escanear)
               (expresion-unaria)
               (recur))
         % (-> amb
               (escanear)
               (expresion-unaria)
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
                (recur))
          < (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
         >= (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
          > (-> amb
                (escanear)
                (expresion-aditiva)
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
                (recur))
         == (-> amb
                (escanear)
                (expresion-relacional)
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

true