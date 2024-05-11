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


<vector>             ::= empty
                         empty-vector


<grafo>             ::= empty
                        empty-grafo


<ejes>              ::= empty
                        empty-ejes
                    

<vertices>          ::= empty
                        empty-vertices
                    

<lista_vertices>    ::= empty
                        empty-lista_vertices
                   
<lista_ejes>        ::= empty
                        empty-lista_ejes
                    

<registro>           ::= empty
                         empty-registro
                    

<expr-bool>          ::= <pred-prim> (<expresion> , <expresion>)
                         comparacion (pprim exp1 exp2)
                     ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
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
    
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp) 
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion)  const-exp) 
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp) ;;Python
           
    (expresion (primitiva-binaria "(" expresion "," expresion ")") primbin-exp) ;;DrRacket
    (expresion (primitiva-unaria "(" expresion ")") primun-exp) ;; Python
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expr-bool "then" expresion "else" expresion "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "done") for-exp)
    
    
    (expresion (lista) lista-exp)
    (lista ("empty") empty-list)
    (lista ("{ lista" (separated-list expresion ";") "}") lista1) ;;C++

    (expresion (vector) vector-exp)
    (vector ("{ vector" (separated-list expresion ";") "}") vector1)

    (expresion (grafo) grafo-exp)
    (grafo ("{ grafo" (separated-list expresion expresion ";") "}") grafo1)

    (expresion (ejes) ejes-exp)
    (ejes ("{ eje" (separated-list expresion expresion ";") "}") ejes1)

    (expresion (vertices) vertices-exp)
    (vertices ("{ vertice" (separated-list expresion ";") "}") vertices1)

    (expresion (lista_ejes) lista_ejes-exp)
    (lista_ejes ("{ lista_ejes" (separated-list expresion ";") "}") lista_ejes1)

    (expresion (lista_vertices) lista_vertices-exp)
    (lista_vertices ("{ lista_vertices" (separated-list expresion ";") "}") lista_vertices1)

    (expresion (registro) registro-exp)
    (registro ("{ registro" (separated-list identificador "=" expresion ";") "}") registro1) ;;C++        

    (expresion (expr-bool) bool-expr)  
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion) ;;DrRacket
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") conjuncion) ;;DrRacket
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

;; scan&parse: -> parser
;; Parser - Analizador sintáctico
(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;; show-the-datatypes: -> void
;; Función para mostrar los tipos de datos definidos
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexico gramatica)))

;; just-scan: -> scanner
;; Scanner - Analizador léxico
(define just-scan
  (sllgen:make-string-scanner lexico gramatica))



;; Frontend + Evaluación + señal para lectura - Interpretador
(define interpretador
  (sllgen:make-rep-loop  "-> "
     (lambda (pgm) (eval-program  pgm)) 
       (sllgen:make-stream-parser 
               lexico
               gramatica)
       )
  )

;; eval-program: <programa> -> numero
;; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases pyGraph pgm
      (pyGraph-program (body)
                      (eval-expression body (init-env))))))

;; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@x @y @z)
     '(1 2 3)
     (empty-env))))


(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (cadena-exp (string) string)
      (lista-exp (lexps) (eval-lista lexps env))
      (vector-exp (lexps) (eval-vector lexps env))
      (identificador-exp (id) (apply-env env id))
      (primun-exp (op exp) (eval-unprim op (eval-expression exp env)))
      (primbin-exp (op exp1 exp2)
                   (eval-binprim op
                                 (eval-expression exp1 env)
                                 (eval-expression exp2 env)))
      (var-exp (ids rands body) (let ((args (eval-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (rec-exp (proc-names idss bodies letrec-body)
               (eval-expression letrec-body
                                (extend-env-recursively proc-names idss bodies env)))
      (bool-expr (expr-bool)
                (eval-bool-exp expr-bool env))
      (if-exp (exp-bool true-exp false-exp)
              (if (eval-bool-exp exp-bool env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc))))
      (begin-exp (exp lexps)
                 (if (null? lexps)
                     (eval-expression exp env)
                     (letrec [(recorrer (lambda (L)
                                          (cond [(null? (cdr L)) (eval-expression (car L) env)]
                                                [else (begin (eval-expression (car L) env)
                                                            (recorrer (cdr L)))]))
                       (begin (eval-expression exp env)
                              (recorrer lexps)))])))
(grafo-exp (grafo)
                 (let ((vertices (grafo-vertices grafo))
                       (ejes (grafo-ejes grafo)))
                   (make-grafo (eval-vertices vertices env) (eval-ejes ejes env))))
      (vertices-exp (label)
                  (make-vertices label))
     (ejes-exp (ejes-list)
                (eval-ejes ejes-list env))
      (else (eopl:error "Error! the variable ~s is not defined")))
    ))

;; Representación de un ambiente
;; Se define un ambiente y dentro de ese se representa otro ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))  
    (vals (list-of scheme-value?))  
    (env environment?)) 
  (recursively-extended-env-record
    (proc-names (list-of symbol?))  
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expresion?)) 
    (env environment?))) 

(define scheme-value? (lambda (v) #t))


;; empty-env:      -> enviroment
;; función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))


;; extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;; función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))


;; extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;; función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;; función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (make-closure (list-ref idss pos)
                                                           (list-ref bodies pos)
                                                           env)
                                             (apply-env old-env sym))))
      )
    ))


;; función para crear cierres a los procedimientos definidos
(define make-closure
  (lambda (params body env)
    (list 'closure params body env)))


;******************************Booleanos******************************

(define eval-bool-exp
  (lambda (expr-bool env)
    (cases exp-bool expr-bool
      (comparacion (pre-prim exp1 exp2)
                   (eval-pred-prim pre-prim
                                   (eval-expression exp1 env)
                                   (eval-expression exp2 env)))
      (conjuncion (op-bin-bool exp-bool1 exp-bool2)
                  (eval-oper-bin-bool op-bin-bool
                                      (eval-bool-exp exp-bool1 env)
                                      (eval-bool-exp exp-bool2 env)))
      (vlr-bool (valor)
                (cases bool valor
                  (true-exp () #t)
                  (false-exp () #f)))
      (op-comp (op-un-bool exp-bool1)
               (eval-oper-un-bool op-un-bool (eval-bool-exp exp-bool1 env)))
      )
    ))


;; Evaluar predicado boleano
(define eval-pred-prim
  (lambda (operator exp1 exp2)
    (case operator
      ((<) (< exp1 exp2))
      ((>) (> exp1 exp2))
      ((<=) (<= exp1 exp2))
      ((>=) (>= exp1 exp2))
      ((==) (equal? exp1 exp2))
      ((<>) (not (equal? exp1 exp2))))))


;; Auxiliares 
(lista-exp (lexps)
  (eval-lista lexps env))


(ejes-exp (ejes-list)
          (eval-ejes ejes-list env))

(define eval-vector eval-lista)


(define eval-lista
  (lambda (lexps env)
    (map (lambda (exp) (eval-expression exp env)) lexps)))


(define eval-vertices
  (lambda (vertices env)
    vertices))


(define eval-ejes
  (lambda (ejes-list env)
    (map (lambda (eje)
           (let ((from (first eje))
                 (to (second eje)))
             (make-ejes (eval-expression from env)
                        (eval-expression to env))))
         ejes-list)))



