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
            ::= <exp-bool>
                bool-exp (exp-bool)
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


(define-datatype expresion-xp expresion-xp?
  (numeroxd (num number?))
  (caracterxd (caracter string?))
  (cadenaxd (cadena string?))
  (listaxd-exp (lexps list))
  (vectorxd-exp (vexps list))
  (identificadorxd-exp (id symbol?))
  (primunxd-exp (op symbol?) (exp expresion-xp?))
  (primbinxd-exp (op symbol?) (exp1 expresion-xp?) (exp2 expresion-xp?))
  (varxd-exp (ids list?) (rands list?) (body expresion-xp?))
  (recxd-exp (proc-names list?) (idss list?) (bodies list?) (letrec-body expresion-xp?))
  (boolxd-exp (expr-bool expresion-xp?))
  (ifxd-exp (exp-bool expresion-xp?) (true-exp expresion-xp?) (false-exp expresion-xp?))
  (procxd-exp (ids list?) (body expresion-xp?))
  (appxd-exp (rator expresion-xp?) (rands list?))
  (beginxd-exp (exp expresion-xp?) (lexps list?))
  (grafoxd (vertices list) (ejecitos list))
  (verticesxd-exp (label list))
  (ejesxd-exp (from expresion-xp?) (to expresion-xp?))
)


(define eval-expression
  (lambda (exp env)
    (cases expresion-xp exp
      (numeroxd (num) num)
      (caracterxd (caracter) caracter)
      (cadenaxd  (cadena) cadena)
      (listaxd-exp (lexps) (eval-lista lexps env))
      (vectorxd-exp (lexps) (eval-vector lexps env))
      (identificadorxd-exp (id) (apply-env env id))
      (primunxd-exp (op exp) (eval-unariaprim op (eval-expression exp env)))
      (primbinxd-exp (op exp1 exp2)
                   (eval-binariaprim op
                                 (eval-expression exp1 env)
                                 (eval-expression exp2 env)))
      (varxd-exp (ids rands body) (let ((args (eval-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (recxd-exp (proc-names idss bodies letrec-body)
               (eval-expression letrec-body
                                (extend-env-recursively proc-names idss bodies env)))
      (boolxd-exp (expr-bool)
                (eval-bool-exp expr-bool env))
      (ifxd-exp (exp-bool true-exp false-exp)
              (if (eval-bool-exp exp-bool env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (procxd-exp (ids body)
                (make-closure ids body env))
      (appxd-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (list? proc)
                      (if (eq? (car proc) 'closure)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc))
                    (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc))))
      (beginxd-exp (exp lexps)
                 (if (null? lexps)
                     (eval-expression exp env)
                     (letrec
                         [(recorrer (lambda (L)
                                      (cond
                                        [(null? (cdr L)) (eval-expression (car L) env)]
                                        [else (begin (eval-expression (car L) env)
                                                     (recorrer (cdr L))
                                                     )]
                                        )
                                      ))
                          ]
                       (begin
                         (eval-expression exp env)
                         (recorrer lexps))
                       )
                     )
                 )
(grafoxd (vertices ejecito)
                   (create-grafito (eval-vertices vertices env)
                                   (eval-ejecitos ejecitos env)))
      
      (verticesxd-exp (label)
                  (make-vertices label))
      
     (ejesxd-exp (from to)
               (make-ejes (eval-expression from env)
                (eval-ejecitos ejecitos env)))
      
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


;******************************Booleanos******************************

(define eval-bool-exp
  (lambda (exp-bool env)
    (cases expr-bool exp-bool
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


;; funciones auxiliares para aplicar eval-expression a cada elemento de una 
;; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

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


;; Evaluar operaciones binarias booleanas
(define eval-oper-bin-bool
  (lambda (op op1 op2)
    (cases oper-bin-bool op
      (and-exp () (and op1 op2))
      (or-exp () (or op1 op2))
      )
    "Evalúa operaciones binarias booleanas basadas en el operador específico (op), aplicando los valores booleanos op1 y op2."
    )
  )


;; Evaluar operaciones unarias booleanas
(define eval-oper-un-bool
  (lambda (op op1)
    (cases oper-un-bool op
      (not-exp () (not op1))
      )
    "Evalúa operaciones unarias booleanas basadas en el operador específico (op), aplicando el valor booleano op1."
    )
  )

;***************************** Bignum ******************************

(define zero
  (lambda()
    '()
  )
)


(define is-zero?
  (lambda(n)
    (null? n)
  )
)

(define base 16)

(define predecesor
  (lambda (n)
    (cond
      ((is-zero? n) (eopl:error "No hay un predecesor para cero"))
      ((>= (car n) base) (eopl:error "Debe ser menor que 16"))
      ((equal? n '(1)) '())
      ((zero? (car n))
      (if (null? (cdr n))
        (eopl:error "No hay un predecesor para cero")
        (cons (- base 1) (predecesor (cdr n)))
        ))
      (else (cons (- (car n) 1) (cdr n)))
    )
  )
)

(define sucesor
  (lambda (n)
    (if (is-zero? n)
      '(1)
      (let ((t (+ (car n) 1)))
        (if (= t base)
          (cons 0 (sucesor (cdr n)))
          (cons t (cdr n))
        )
      )
    )
  )
)

(define sum-bignum
  (lambda (x y)
    (if (is-zero? x)
      y
      (sucesor (sum-bignum (predecesor x) y))
    )
  )
)

(define res-bignum
  (lambda (x y)
    (if (is-zero? y)
      x
      (predecesor (res-bignum x (predecesor y)))
    )
  )
)

(define mult-bignum
  (lambda (x y)
    (if (is-zero? y)
      x
      (predecesor (res-bignum x (predecesor y)))
    )
  )
)


;***************************** Auxiliares ******************************


;; funciones auxiliares para encontrar la posición de un símbolo
;; en la lista de símbolos de unambiente
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))


(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))


;; Funcion para evaluar listas
(define eval-lista
  (lambda (l-exp env)
    (cases lista l-exp
      (empty-list () '())  ; Si la lista está vacía, retorna una lista vacía de Scheme.
      (lista1 (lexps)
        (map (lambda (exp) (eval-expression exp env)) lexps))  ; Evalúa cada expresión en la lista y retorna una lista de resultados.
      )
    ))



;; primitivas unarias
(define eval-unariaprim
  (lambda (op op1 env)
    (cases primitiva-unaria op
      (add1 () (+ op1 1))
      (sub1 () (- op1 1))
      (add1x8 () (sum-bignum op1 '(1) 8))
      (sub1x8 () (res-bignum op1 '(1) 8))
      (add1x16 () (sum-bignum op1 '(1) 16))
      (sub1x16 () (res-bignum op1 '(1) 16))
      (add1x32 () (sum-bignum op1 '(1) 32))
      (sub1x32 () (res-bignum op1 '(1) 32))
      (lenght-exp () (length op1))
      (lista?-exp () (list? op1))
      (car-exp () (if (list? op1) (car op1) (eopl:error 'eval-unariaprim "car expects a list")))
      (cdr-exp () (if (list? op1) (cdr op1) (eopl:error 'eval-unariaprim "cdr expects a list")))
      (else (eopl:error 'eval-unariaprim "Unknown unary primitive operation" op))
      )
    )
  )


;; primitivas binarias
(define eval-binariaprim
  (lambda (op op1 op2 env)
    (cases primitiva-binaria op
      (suma () (+ op1 op2))
      (resta () (- op1 op2))
      (mult () (* op1 op2))
      (modulo-b () (if (= op2 0)
                       (eopl:error 'eval-binariaprim "División por cero en módulo")
                       (modulo op1 op2)))
      (division () (if (= op2 0)
                       (eopl:error 'eval-binariaprim "División por cero")
                       (/ op1 op2)))
      (concat-exp () (if (and (string? op1) (string? op2))
                         (string-append op1 op2)
                         (eopl:error 'evalbinariaprim "Los operandos deben ser cadenas para 'concat'")))
      (append-exp () (if (and (list? op1) (list? op2))
                         (append op1 op2)
                         (eopl:error 'eval-binariaprim "Los operandos deben ser listas para 'append'")))
      (crear-lista-exp () (cons op1 op2))  ; Asume que op1 es un elemento y op2 una lista
      (else (eopl:error 'eval-binariaprim "Operación binaria desconocida" op))
      )
    )
  )

;; predicado primario

(define eval-predprim
  (lambda (op op1 op2 env)
    (cases pred-prim op
      (menor-exp () (< op1 op2))
      (mayor-exp () (> op1 op2))
      (menor=exp () (<= op1 op2))
      (mayor=exp () (>= op1 op2))
      (igual=exp () (equal? op1 op2))
      (diferente-exp () (not (equal? op1 op2)))
    )))



; clousure
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)
   )
  )


(define apply-procedure
  (lambda (proc args)
    (if (and (list? proc) (eq? (car proc) 'closure))
        (let ((ids (cadr proc))
              (body (caddr proc))
              (env (cadddr proc)))
          (eval-expression body (extend-env ids args env)))
        (eopl:error 'apply-procedure "Attempt to apply non-procedure ~s" proc))))


(define eval-vector
  (lambda (v-exp env)
    (cases vector v-exp
      ;(empty-vector () '())  ; Si el vector está vacío, retorna un vector vacío de Scheme.
      (vector1 (vexps)
        (map (lambda (exp) (eval-expression exp env)) vexps)))  ; Evalúa cada expresión en el vector y retorna un vector de resultados.
  )
)


(define eval-vertices
  (lambda (vertices-list env)
    (map (lambda (vertex) (eval-expression vertex env)) vertices-list)
    ))

(define ejecitos '())

(define eval-ejecitos
  (lambda (ejecitos env)
    (map (lambda (edge) (eval-expression edge env)) ejecitos)
    ))

;***************************** Constructores grafos ******************************


(define create-grafito
  (lambda (vertices-list ejecitos)
    (graph-exp (vertices-exp vertices-list) (edges-exp ejecitos))
    ))


(define make-vertices
  (lambda (label-list)
    (vertices-exp label-list)
    ))


(define make-ejes
  (lambda (from to)
    (edges-exp (list (edges-exp from to)))
  )
)

(define graph-exp
  (lambda (vertices edges)
    (grafoxd vertices edges)
  )
)

(define edges-exp
  (lambda (from to)
    (ejesxd-exp from to)
  )
)

(define graph
  (lambda (l)
    (ejesxd-exp l)
  )
)


;***************************** Predicados grafos ******************************


(define graph?
  (lambda (exp)
    (cases expresion-xp exp
      (grafoxd (vertices edges)
                (and (vertices? vertices) (edges? edges)))
                (else #f)
      )))

(define edges?
  (lambda (exp)
    (cases expresion-xp exp
      (ejesxd-exp (from to)
                  (and (list? from) (list? to)))
                  (else #f))))


;***************************** Extractores grafos ******************************

(define graph->vertices
  (lambda (exp)
    (cases expresion-xp exp
      (grafoxd (vertices edges)
                vertices)
                (else (eopl:error "No es un grafo"))
    )
  )
)

(define vertices->list
  (lambda (exp)
    (cases expresion-xp exp
      (verticesxd-exp(label)
                      label)
      (else (eopl:error "No es una lista de vertices"))
    )
  )
)

(define edges->list
  (lambda (exp)
    (cases expresion-xp exp
      (ejesxd-exp (from to)
                  to)
                  (else (eopl:error "No es una lista de edges")))))




#| 

  ;***************************** Funciones grafos no dirigidos ******************************
(define member-aux
  (lambda (x lst)
    (cond
      ((null? lst) #f)
      ((equal? x (car lst)) #t)
      (else (member-aux x (cdr lst))))))

(define reverse-edge
  (lambda (edge)
    (cons (cdr edge) (car edge))))



(define add-edge
  (lambda (graph edge)
    (cases expresion-xp graph
      ((grafoxd (vertices edges))
       (let ((edges-list (edges->list edges)))
         (if (or (member-aux edge edges-list)
                 (member-aux (reverse-edge edge) edges-list))
             (eopl:error "The given edge already exists in the graph")
             (graph (vertices) (edges (append edges (list edge))))))))))



(define vecinos
  (lambda (graph node)
    (let ((edges-list (edges->list graph)))
      (letrec ((find-neighbors
                (lambda (edges)
                  (cond
                    ((null? edges) '())
                    ((equal? (car (car edges)) node) (cons (cdr (car edges)) (find-neighbors (cdr edges))))
                    ((equal? (cdr (car edges)) node) (cons (car (car edges)) (find-neighbors (cdr edges))))
                    (else (find-neighbors (cdr edges)))))))
        (find-neighbors edges-list))))) |#
 
