; LOADS
(load-file "ambient.clj")


; DECLARES
(declare buscar-mensaje)
(declare dar-error)


; FUNCTIONS
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

; Muestra visual del error
(defn dar-error [amb cod]
  (if (= (estado amb) :sin-errores)
    (do (prn)
      (println "ERROR AL INTERPRETAR EL PROGRAMA!")
      (println "*********************************")
      (prn)
      (listar (simb-ya-parseados amb))                                              ;Te muestra todo lo que se parseo correctamente
      (prn) 
      (println ">") 
      (println ">>" (buscar-mensaje cod))                                           ;Se muestra el codigo de error con su descripcion, en el lugar donde se encontro el error
      (println ">") 
      (prn)
      (pr (simb-actual amb))                                                        ;Te muestra el simbolo del error
      (print " ")
      (listar (simb-no-parseados-aun amb))                                          ;Luego te muestra todo lo que no se llego a interpretar 
      (prn)
      (flush)
      [(simb-actual amb) '() (simb-ya-parseados amb) cod]                           ;RETORNO
    )
  amb)
)