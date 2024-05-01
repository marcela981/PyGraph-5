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
    (lista ("[" (separated-list expresion ",") "]") lista1) ;;C++

    (expresion (exp-bool) bool-exp)  
    (exp-bool (pred-prim "(" expresion "," expresion ")") comparacion) ;;DrRacket
    (exp-bool (oper-bin-bool "(" exp-bool "," exp-bool ")") conjuncion) ;;DrRacket
    (exp-bool (bool) vlr-bool)
    (exp-bool (oper-un-bool "(" exp-bool ")") op-comp) 
    (bool ("true") true-exp)
    (bool ("false") false-exp)
    )
  )