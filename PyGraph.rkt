#lang eopl

;; Marcela Mazo Castro - 1843612
;; marcela.mazo@correounivalle.edu.co

;************************************************************************************************************************************************************

#| Gramatica definida

<pyGraph> ::=  <expresion>
               pyGraph-program (exp)

<expresion> ::= <numero>
                numero-lit (num)
            ::= '<caracter>'
                 caracter-exp (caracter)
            ::= "<cadena>"
                 cadena-exp (cadena)
            ::= <expr-bool>
                bool-expr (expr-bool)
            ::= <identificador>
                identificador-exp (id)          
            ::= var {<identificador> = <expresion>}*(,) in <expresion>
                var-exp (ids exps cuerpo)
            ::= const {<identificador> = <expresion>}*(,) in <expresion>
                const-exp (ids exps cuerpo)>
            ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
                rec-exp (lproc ids cuerpos cuerporec)
            ::= <lista>
                lista-exp (lista)            
            ::= <vector>
                vector-exp (vector)
            ::= <ejes>
                ejes-exp (vertices1, vertices2)
            ::= <vertices>
                vertices-exp (vertices)
            ::= <grafo>
                grafo-exp (vertices, ejes)
            ::= <lista_vertices>
                lista_vertices-exp (vertices)
            ::= <lista_ejes>
                lista_ejes-exp (ejes)
            ::= <registro>
                registro-exp (registro)
            ::= begin {<expresion>}+(;) end
                begin-exp (exp lexps)
            ::= if <expr-bool> then <expresion> else <expresion> end
                if-exp (expb exp1 exp2)
            ::= while <expr-bool> do <expresion> done
                while-exp (expb body)
            ::= for <identificador> = <expresion> (to | downto) <expresion> do <expresion> done
                for-exp (id start to end body)

******************************Primitivas binarias******************************            
<primitiva-binaria>        ::= + | - | * | % | /
                           ::= cons | append
                           ::= concat

******************************Primitivas unarias******************************
<primitiva-unaria>         ::= lenght 
                           ::= add1 | sub1 | add1x8 | sub1x8| add1x16 | sub1x16| add1x32 | sub1x32
                           ::= empty? | list? | car | cdr

******************************************************************************
<lista>              ::= empty
                         empty-list
                     ::= lenght
                         lenght-list
                     ::= [{<expresion>}*(;)]
                         lista1 (lexps)

<vector>             ::= empty
                         empty-vector
                     ::= lenght
                         lenght-vector
                     ::= concat
                         concat-vector
                     ::= [{<expresion>}*(;)]
                         vector1 (lexps)

<grafo>             ::= empty
                        empty-grafo
                    ::= lenght
                        lenght-grafo
                    ::= [{<lista_vertices> <lista_ejes>}]
                        graph1 (vertices, ejes)

<ejes>              ::= empty
                        empty-ejes
                    ::= lenght
                        lenght-ejes
                    ::= concat
                        concat-ejes
                    ::= [{<expresion>, <expresion>}*(;)]
                        ejes1 (vertices)

<vertices>          ::= empty
                        empty-vertices
                    ::= lenght
                        lenght-vertices
                    ::= concat
                        concat-vertices
                    ::= [{<expresion>}*(;)]
                        vertices1 (lexps)

<lista_vertices>    ::= empty
                        empty-lista_vertices
                    ::= lenght
                        lenght-lista_vertices
                    ::= concat
                        concat-lista_vertices
                    ::= [{<expresion>}*(;)]
                        lista_vertices1 (lexps)

<lista_ejes>        ::= empty
                        empty-lista_ejes
                    ::= lenght
                        lenght-lista_ejes
                    ::= concat
                        concat-lista_ejes
                    ::= [{<expresion>}*(;)]
                        lista_ejes1 (lexps)

<registro>           ::= empty
                         empty-registro
                     ::= delete
                         delete-registro
                     ::= { {<identificador> = <expresion> } + (;) }
                         registro1 (lexps)

<expr-bool>          ::= <pred-prim> (<expresion> , <expresion>)
                         comparacion (pprim exp1 exp2)
                     ::= <oper-bin-bool> (<exp-bool> , <exp-bool>)
                         conjuncion (obbool expb1 expb2)
                     ::= <bool>
                         vlr-bool (bool)
                     ::= <oper-un-bool> (<expr-bool>)
                         op-comp (oubool expb)

<pred-prim>          ::= <|>|<=|>=|==|<>
<oper-bin-bool>      ::= and|or
<oper-un-bool>       ::= not
<bool>               ::= true | false
|#

;;************************************************************Especificación Léxica************************************************************
(define lexico
  '(
    (white-sp (whitespace) skip)
    (comment ("//" (not #\newline)) skip)
    (comment ("/*" (not "*/") "*/") skip)
    (identificador ("@" letter (arbno (or letter digit))) symbol)
    (letras (letter) string)
    (letras (letter (arbno (or letter digit))) string)    
    (numero (digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

;;************************************************************Especificación Sintáctica************************************************************
(define gramatica
  '(    
    (pyGraph (expresion)  pyGraph-program )  
    (expresion (numero) numero-lit)
    
    (expresion ("'" letras "'") caracter-exp) ;; Python
    (expresion ("\"\"" letras "\"\"") cadena-exp) ;; C++
    (expresion (identificador) identificador-exp) ;; C++
    
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)  ;;JavaScript
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion)  const-exp) ;; JavaScript
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp) ;;Python
           
    (expresion (primitiva-binaria "(" expresion "," expresion ")") primbin-exp) ;;DrRacket
    (expresion (primitiva-unaria "(" expresion ")") primun-exp) ;; Python
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" exp-bool "then" expresion "else" expresion "end") if-exp) 
    (expresion ("while" exp-bool "do" expresion "end") while-exp)
    (expresion ("for" identificador "=" expresion ("to") expresion "do" expresion "end") for-exp)

    
    (expresion (lista) lista-exp)
    (lista ("empty") empty-list)
    (lista ("lenght") lenght-list)
    (lista ("[" (separated-list expresion ";") "]") lista1) ;;C++

    (expresion (vector) vector-exp)
    (vector ("empty") empty-vector)
    (vector ("lenght") lenght-vector)
    (vector ("concat") concat-vector)
    (vector ("[" (separated-list expresion ";") "]") vector1) ;;C++

    (expresion (grafo) grafo-exp)
    (grafo ("empty") empty-grafo)
    (grafo ("lenght") lenght-grafo)
    (grafo ("{" (separated-list vertices, ejes ";") "}") grafo1)

    (expresion (ejes) ejes-exp)
    (ejes ("empty") empty-ejes)
    (ejes ("lenght") lenght-ejes)
    (ejes ("concat") concat-ejes)
    (ejes ("{" (separated-list expresion, expresion ";") "}") ejes1)

    (expresion (vertices) vertices-exp)
    (vertices ("empty") empty-vertices)
    (vertices ("lenght") lenght-vertices)
    (vertices ("concat") concat-vertices)
    (vertices ("{" (separated-list expresion ";") "}") vertices1)

    (expresion (lista_ejes) lista_ejes-exp)
    (lista_ejes ("empty") empty-lista_ejes)
    (lista_ejes ("lenght") lenght-lista_ejes)
    (lista_ejes ("concat") concat-lista_ejes)
    (lista_ejes ("{" (separated-list expresion ";") "}") lista_ejes1)

    (expresion (lista_vertices) lista_vertices-exp)
    (lista_vertices ("empty") empty-lista_vertices)
    (lista_vertices ("lenght") lenght-lista_vertices)
    (lista_vertices ("concat") concat-lista_vertices)
    (lista_vertices ("{" (separated-list expresion ";") "}") lista_vertices1)

    (expresion (registro) registro-exp)
    (registro ("empty") empty-registro)
    (registro ("delete") delete-registro)
    (registro ("[" (separated-list expresion ";") "]") registro1) ;;C++        

    (expresion (expr-bool) bool-exp)  
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion) ;;DrRacket
    (expr-bool (oper-bin-bool "(" expr-bool "," exp-bool ")") conjuncion) ;;DrRacket
    (expr-bool (bool) vlr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") op-comp) 
    (bool ("true") true-exp)
    (bool ("false") false-exp)

    ;************primitivas unarias************

    (primitiva-unaria ("add1") add1)
    (primitiva-unaria ("sub1") sub1)    
    (primitiva-unaria ("add1x8") add1x8)
    (primitiva-unaria ("sub1x8") sub1x8)
    (primitiva-unaria ("add1x16") add1x16)
    (primitiva-unaria ("sub1x16") sub1x16)
    (primitiva-unaria ("add1x32") add1x32)
    (primitiva-unaria ("sub1x32") sub1x32)
    (primitiva-unaria ("lenght") lenght-exp)   
    (primitiva-unaria ("list?") lista?-exp)
    (primitiva-unaria ("car") car-exp)
    (primitiva-unaria ("cdr") cdr-exp)

    ;************primitivas binarias************
    
    (primitiva-binaria ("+") suma)
    (primitiva-binaria ("-") resta)
    (primitiva-binaria ("*") mult)
    (primitiva-binaria ("%") modulo-b)
    (primitiva-binaria ("/") division)
    
    (primitiva-binaria ("concat") concat-exp)
    (primitiva-binaria ("append") append-exp)
    (primitiva-binaria ("cons") crear-lista-exp)
        
    (pred-prim ("<") menor-exp)
    (pred-prim (">") mayor-exp)
    (pred-prim ("<=") menor=exp)
    (pred-prim (">=") mayor=exp)
    (pred-prim ("==") igual=exp)
    (pred-prim ("<>") diferente-exp)
    (oper-bin-bool ("and") and-exp)
    (oper-bin-bool ("or") or-exp)
    (oper-un-bool ("not") not-exp)
    )
  )

;************************************************************************************************************************************************************

;; Tipos de datos para la sintaxis
(sllgen:make-define-datatypes lexico gramatica)

;;scan&parse: -> parser
;; Parser - Analizador sintáctico
(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;;show-the-datatypes: -> void
;; Función para mostrar los tipos de datos definidos
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexico gramatica)))

;;just-scan: -> scanner
;; Scanner - Analizador léxico
(define just-scan
  (sllgen:make-string-scanner lexico gramatica))



;;Frontend + Evaluación + señal para lectura - Interpretador
(define interpretador
  (sllgen:make-rep-loop  "-> "
     (lambda (pgm) (eval-program  pgm)) 
       (sllgen:make-stream-parser 
               lexico
               gramatica)
       )
  )

