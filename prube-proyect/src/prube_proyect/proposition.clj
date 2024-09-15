; LOADS
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "expression.clj")
(load-file "functions.clj")
(load-file "miscellany.clj")
(load-file "predicates.clj")
(load-file "programa.clj")
(load-file "proposition.clj")


; DECLARE
(declare procesar-opcional-proposiciones)
(declare procesar-asignacion)
(declare procesar-asignacion-llamada)
(declare procesar-declaracion-variable)
(declare proposicion)
(declare proposicion-compuesta)


; FUNCTIONS
; Espera mas proposiciones
(defn procesar-opcional-proposiciones [amb]
    (if (= (estado amb) :sin-errores)
        (if (or (proposicion? (simb-actual amb)) (= (simb-actual amb) (symbol "{")))
            (-> amb
                (proposicion)
                (recur))
            amb)
        amb)
)

; Procesa los distintos tipos de proposiciones
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

                while (let [primera-fase (escanear amb), segunda-fase (expresion primera-fase)]
                    (if (= (estado segunda-fase) :sin-errores)
                        (-> segunda-fase (proposicion)) 
                        segunda-fase))

                (proposicion-compuesta amb)))
    amb)
)

; Si se escribe un identificador procesa una asignacion
(defn procesar-asignacion-llamada [amb]
    (if (= (estado amb) :sin-errores)
        (cond
            (asignacion? (simb-actual amb)) (procesar-asignacion amb)
            (= (simb-actual amb) (symbol "(")) (procesar-llamada amb)
            :else (dar-error amb 25))
    amb
    )
)

; Si se va a procesar una asignacion delega a expresion
(defn procesar-asignacion [amb]
    (if (= (estado amb) :sin-errores)
        (if (asignacion? (simb-actual amb))
            (-> amb
                (escanear)
                (expresion))
        amb)
    amb)
)

; Espera {} con opcionales multiples proposiciones dentro
(defn proposicion-compuesta [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-terminal ,,, (symbol "{") 19)
            (procesar-opcional-proposiciones)
            (procesar-terminal ,,, (symbol "}") 20))
        amb)
)

; Delega a procesar asignacion
(defn procesar-declaracion-variable [amb]
    (if (= (estado amb) :sin-errores)
        (procesar-asignacion amb)
        amb)
)

; Espera un identificador para luego una expresion que va a entrar en la variable
(defn procesar-asignacion [amb]
    (if (= (estado amb) :sin-errores)
        (if (asignacion? (simb-actual amb))
            (-> amb
                (escanear)
                (expresion))
        amb) 
    amb)
)