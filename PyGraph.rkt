#lang eopl

;; Marcela Mazo Castro - 1843612
;; marcela.mazo@correounivalle.edu.co

;************************************************************************************************************************************************************

#| Gramatica definida

<pyGraph> :=  <expresion>
               pyGraph-program (exp)

<expresion> := <int>
              ...
            
            := <double>
              ...

            := <char>
              ...

            := <string>
              ...

            := <bool>
              ...

            := <list>
              ...

            := <record>
              ...

            := <vector>
              ...

            := <undirectGraph>
              ...

            := <Vertex>
              ...

            := <edge>
              ...

            := <void>
              ...
               
           .            
           .
           .

<primitiva-binaria>

  :=  + (primitiva-suma)

  :=  ~ (primitiva-resta)

  :=  / (primitiva-div)

  :=  * (primitiva-multi)

  :=  concat (primitiva-concat)

<primitiva-unaria>
     
            :=  add1 (primitiva-add1)

            :=  sub1 (primitiva-sub1)

            :=  longitud (primitiva-longitud)


|#