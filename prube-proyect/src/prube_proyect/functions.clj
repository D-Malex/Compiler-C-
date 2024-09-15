; LOADS
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "expression.clj")
(load-file "miscellany.clj")
(load-file "predicates.clj")
(load-file "programa.clj")
(load-file "proposition.clj")


; DECLARES
(declare procesar-declaraciones-fn)
(declare declaracion-fn)
(declare declarar-mas-fn)
(declare declarar-opcional-param)
(declare procesar-firma-fn)
(declare procesar-fn-con-retorno)
(declare procesar-fn-sin-retorno)
(declare procesar-llamada)
(declare procesar-mas-param)
(declare procesar-opcional-llamada-punto)
(declare procesar-opcional-por-ref)
(declare procesar-tipo-param)


; FUNCTIONS
; Delega la declaracion de funciones
(defn procesar-declaraciones-fn [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (declaracion-fn)
            (declarar-mas-fn))
    amb)
)

; Delega para mas funciones
(defn declarar-mas-fn [amb]
    (if (= (estado amb) :sin-errores)
        (if (not= (simb-actual amb) 'EOF)
            (-> amb
                (declaracion-fn)
                (recur))
            amb)
        amb)
)

; Espera el tipo de retorno de la funcion
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

; Espera el nombre de la funcion, los (parentesis) y los parametros
(defn procesar-firma-fn [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-terminal ,,, identificador? 11)
            (procesar-terminal ,,, (symbol "(") 15)
            (declarar-opcional-param)
            (procesar-terminal ,,, (symbol ")") 17))
        amb)
)

; Delega la definicion de parametros en la firma de la funcion
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

; Espera el tipo de parametro
(defn procesar-tipo-param [amb]
    (if (= (estado amb) :sin-errores)
        (if (tipo? (simb-actual amb))
            (-> amb (escanear))
            (dar-error amb 18)
        )
    amb)
)

; Espera el &
(defn procesar-opcional-por-ref [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (symbol "&"))
        (-> amb (escanear))
        amb)
    amb)
)

; Espera , y utiliza recursion para el resto de parametros que se deseen
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

; Cuando hay retorno se delega a proposicion compuesta
(defn procesar-fn-con-retorno [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb (proposicion-compuesta))
    amb)
)

; Cuadno no hay retorno se delega a proposicion compuesta
(defn procesar-fn-sin-retorno [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb (proposicion-compuesta))
    amb)
)

; Espera las llaves de funcion {}
(defn proposicion-compuesta [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-terminal ,,, (symbol "{") 19)
            (procesar-opcional-proposiciones)
            (procesar-terminal ,,, (symbol "}") 20))
        amb)
)


; Permite hacer llamadas a los metodos de un objetos
(defn procesar-opcional-llamada-punto [amb]
    (if (= (estado amb) :sin-errores)
        (cond
            (= (simb-actual amb) (symbol "(")) (procesar-llamada amb)
            (= (simb-actual amb) (symbol ".")) (-> amb
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

; En caso de ser llamado de funciones
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

; Esta parte del programa se encarga de hacer esto:
;    void funsionBuena(int a, double& b) {}
;    funsionBuena(a, b)
;    funsionBuena.substr(a,b)
