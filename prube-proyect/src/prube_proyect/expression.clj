; LOADS
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "functions.clj")
(load-file "miscellany.clj")
(load-file "predicates.clj")
(load-file "programa.clj")
(load-file "proposition.clj")


; DECLARES
(declare expresion)
(declare expresion-aditiva)
(declare expresion-and)
(declare expresion-atomica)
(declare expresion-igualdad)
(declare expresion-multiplicativa)
(declare expresion-relacional)
(declare expresion-unaria)
(declare procesar-expresion-endl)
(declare procesar-mas-expresion-aditiva)
(declare procesar-mas-expresion-and)
(declare procesar-mas-expresiones-a-imprimir)
(declare procesar-mas-expresion-igualdad)
(declare procesar-mas-expresion-multiplicativa)
(declare procesar-mas-expresion-relacional)
(declare procesar-mas-expresion-unaria)
(declare procesar-opcional-expresiones)


; FUNCTIONS
; Delega a una posible expresion and
(defn expresion [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-and)
            (procesar-mas-expresion-and))
    amb)
)


; Delega a una posible expresion igualdad
(defn expresion-and [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-igualdad)
            (procesar-mas-expresion-igualdad))
        amb)
)

; Espera || y delega a mas expresiones and
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


; Delega a expresione relacional
(defn expresion-igualdad [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-relacional)
            (procesar-mas-expresion-relacional))
    amb)
)

; Espera && y delega a mas expresiones igualdad
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


; Delega a expresion aditiva
(defn expresion-relacional [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-aditiva)
            (procesar-mas-expresion-aditiva))
    amb)
)

; Experaa != o == y delega a expresion relacional
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


; Delega a expresion multiplicidad
(defn expresion-aditiva [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-multiplicativa)
            (procesar-mas-expresion-multiplicativa))
        amb)
)

; Experaa <=, <, >= o > y delega a expresion aditiva
(defn procesar-mas-expresion-aditiva [amb]
    (if (= (estado amb) :sin-errores)
        (case (simb-actual amb) 
            <= (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
            <  (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
            >= (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
            >  (-> amb
                (escanear)
                (expresion-aditiva)
                (recur))
        amb)
    amb)
)


; Delega a expresion unaria
(defn expresion-multiplicativa [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (expresion-unaria)
            (procesar-mas-expresion-unaria))
    amb)
)

; Espera - o + y delega a expresion multiplicativa
(defn procesar-mas-expresion-multiplicativa [amb]
    (if (= (estado amb) :sin-errores)
        (case (simb-actual amb)
        - (-> amb
            (escanear)
            (expresion-multiplicativa)
            (recur))
        + (-> amb
            (escanear)
            (expresion-multiplicativa)
            (recur))
        amb)
    amb)
)


; Delega a expresion atomica
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

; Esperea *, / o % y delega a expresion unaria
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


; Espera expresiones
(defn expresion-atomica [amb]
    (if (= (estado amb) :sin-errores)
        (cond
            (numero? (simb-actual amb)) (-> amb (escanear))
            (cadena? (simb-actual amb)) (-> amb (escanear))
            (booleano? (simb-actual amb)) (-> amb (escanear))
            (identificador? (simb-actual amb))
                (-> amb
                    (escanear)
                    (procesar-opcional-llamada-punto))
            :else (case (simb-actual amb) 
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
                (if (= (simb-actual amb) (symbol "("))
                    (-> amb
                        (escanear)
                        (expresion)
                        (procesar-terminal ,,, (symbol ")") 16))
                (dar-error amb 22))))
        amb)
    amb
)

; Espera mas expresiones hasta el )
(defn procesar-opcional-expresiones [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (symbol ")"))
            amb
            (-> amb
                (expresion)
                (procesar-mas-opcional-expresion)))
    amb)
)

; En caso de llamarse a mas expresiones es pera la ,
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

; Espera endl
(defn procesar-expresion-endl [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) 'endl)
            (-> amb (escanear))
            (expresion amb))
        amb)
)

; Espera mas expresiones recursivamente y
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