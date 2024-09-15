; LOADS
(ns prube-proyect.core (:gen-class))
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "expression.clj")
(load-file "functions.clj")
(load-file "miscellany.clj")
(load-file "predicates.clj")
(load-file "programa.clj")
(load-file "proposition.clj")

; DECLARES
(declare -main)
(declare dump)
(declare interpretar)
;(declare procesar-tipo-variable)


; FUNCTIONS
(defn -main ([]
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
        (try (let [linea (clojure.string/split (clojure.string/trim (read-line)) #" "), cabeza (clojure.string/upper-case (first linea))]
            (cond
                (= cabeza "SALIR") 'CHAU
                
                (= cabeza "AYUDA") (driver-loop)
                
                (= cabeza "ESCAN") (let [nom (second linea)]
                    (if (not (.exists (clojure.java.io/file nom)))
                        (do (print "ERROR: ") (println (buscar-mensaje 2) (str (symbol "(") nom (symbol ")"))) (flush) (driver-loop status))
                        (do (listar (escanear-arch nom)) (driver-loop status))
                    ))
                
                (= cabeza "VIRTU") (let [nom (second linea)]
                    (if (not (.exists (clojure.java.io/file nom)))
                        (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                        (let [res (parsear (escanear-arch nom))]
                            (do (if (= (estado res) :sin-errores) (dump (bytecode res)))
                            (driver-loop status)))
                    ))
                
                (= cabeza "INTER") (let [nom (second linea)]
                    (if (not (.exists (clojure.java.io/file nom)))
                        (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                        (let [res (parsear (escanear-arch nom))]
                            (do (if (= (estado res) :sin-errores)
                                (interpretar (bytecode res) [] 0 [] (mapa-regs-de-act res)))
                                (driver-loop status)))
                    ))
                
                (= cabeza "") (driver-loop status)
                
                :else (do 
                    (print "ERROR: ") 
                    (println (buscar-mensaje 1) (str (symbol "(") (first linea) (symbol ")"))) (flush) 
                    (driver-loop status))
            ))
            
        (catch Exception e 
            (println "ERROR ->" (clojure.string/trim (clojure.string/upper-case 
                (let [msg-err (get (Throwable->map e) :cause)] 
                (if (nil? msg-err) "desconocido" msg-err))))) 
                    (driver-loop status))
        ))
)

true