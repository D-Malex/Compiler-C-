; LOADS
(load-file "ambient.clj")
(load-file "definitions.clj")
(laod-file "errors.clj")
(load-file "expression.clj")
(load-file "functions.clj")
(load-file "miscellany.clj")
(load-file "predicates.clj")
(load-file "proposition.clj")

; DECLARES
(declare programa)
; -------------#include<>------------- ;
(declare procesar-opcional-declaraciones-include)
(declare declaraciones-include)
(declare procesar-opciones-include)
; -------------using------------- ;
(declare procesar-opcional-declaraciones-using)
(declare declaracion-using)
(declare procesar-std)
; -------------const------------- ;
(declare procesar-opcional-declaraciones-const)
(declare declaracion-const)
(declare procesar-tipo-const)
(declare procesar-declaracion-constante)
(declare procesar-asignacion-constante)


; FUNTIONS
; Inicia con el programa y los includes, using, const y funciones
(defn programa [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-opcional-declaraciones-include)  
            (procesar-opcional-declaraciones-using)
            (procesar-opcional-declaraciones-const)
            (procesar-declaraciones-fn))
    amb)
)


; -------------#include<>------------- ;

; Espera eL #include
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

; Espera el los < >
(defn declaracion-include [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-terminal ,,, '< 4)
            (procesar-opciones-include)
            (procesar-terminal ,,, '> 5))
    amb)
)

; Espera lo que se incluye
(defn procesar-opciones-include [amb]
    (if (= (estado amb) :sin-errores)
        (case (simb-actual amb) 
                iostream (-> amb (escanear))
                cmath    (-> amb (escanear))
                (dar-error amb 6))
        amb)
)

; -------------using------------- ;

; Espera el using
(defn procesar-opcional-declaraciones-using [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (simbol "using"))
            (-> amb
                (escanear)
                (declarecion-using))
                (procesar-terminal ,,, (symbol ";") 7)
                (recur)
            amb)
        amb)
)

; Espera el namespace
(defn declaracion-using [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (symbol "namespace"))
            (-> amb 
                (escanear)
                (procesar-std))
            (dar-error amb 8))
    amb)
)

; Espera std
(defn procesar-std [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (symbol "std"))
            (-> amb (escanear))
            (dar-error amb 9))
    amb)
)

; -------------const------------- ;

; Espera el const
(defn procesar-opcional-declaraciones-const [amb]
    (if (= (estado amb) :sin-errores) 
        (if (= (simb-actual amb) (symbol "const"))
            (-> amb
                (escanear)
                (declaracion-const)
                (recur))
        amb)
    amb)
)

; Se encarga de toda la estructura de la definicion del const
(defn declaracion-const [amb]
    (if (= (estado amb) :sin-errores)
        (-> amb
            (procesar-tipo-const)
            (procesar-terminal ,,, identificador? 11)
            (procesar-declaracion-constante)
            (procesar-terminal ,,, (symbol ";") 7))        
    amb)
)

; Espera el tipo de constante (tambien podria ser string o bool)
(defn procesar-tipo-const [amb]
    (if (= (estado amb) :sin-errores)
        (case (simb-actual amb)
            int (-> amb (escanear))
            double (-> amb (escanear))
            (dar-error amb 10))
    amb)
)

; Espera el =
(defn procesar-declaracion-constante [amb]
    (if (= (estado amb) :sin-errores)
        (if (= (simb-actual amb) (symbol "="))
            (-> amb 
                (escanear)
                (procesar-asignacion-constante))
            (dar-error amb 12))
    amb)
)

; Espera un numero (en caso de bool y string tendrias que modificar esta funcion tambien)
(defn procesar-asignacion-constante [amb]
    (if (= (estado amb) :sin-errores)
        (if (numero? (simb-actual amb))
            (-> amb (escanear))
            (dar-error amb 13)
        )
    amb)
)


; Esta parte del programa se encarga de hacer esto:
; [1]      #include<iostream>
; [2]      using namespace std;
; [3]      const ident int = 3;