(1) natural(0).
(2) natural(suc(X)) :- natural(X).
(3) menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
(4) menorOIgual(X,X) :- natural(X).

?- menorOIgual(0, X)
   |
   +- menorOIgual(0, Y₁)  con S = MGU { 0 ≟ X₁, X ≟ suc(Y₁) } = { X₁ := 0, X := suc(Y₁) }
       |
       +- menorOIgual(0, Y₂)  con S = MGU { 0 ≟ X₂, Y₁ ≟ suc(Y₂) } = { X₂ := 0, Y₁ := suc(Y₂) }
       
        ... es el mismo goal... va a pasar lo mismo.


0?

(5)  menorOIgual(0, X).
(+4) natural(0).
(+1) ☐


1?

(5)  menorOIgual(0, X)
(+3) menorOIgual(0, Y₁)  con S = MGU { 0 ≟ X₁, X ≟ suc(Y₁) } = { X₁ := 0, X := suc(Y₁) }
(+4) natural(0)  con S = MGU { 0 ≟ X₂, Y₁ ≟ X₂ } = { X₂ := 0, Y₁ := 0 }
(+1) ☐  
     X = suc(0)


(1) preorder(nil,[]).
(2) preorder(bin(I,R,D),[R|L]) :- append(LI,LD,L), preorder(I,LI), preorder(D,LD).

(3) append([],YS,YS).
(4) append([X|XS],YS,[X|L]) :- append(XS,YS,L).

(5) ?- preorder(bin(bin(nil,2,nil),1,nil),Lista).

(+2 6) append(LI₆,LD₆,L₆), preorder(bin(nil,2,nil),LI₆), preorder(nil,LD₆).         S₆ = { I₆ := bin(nil,2,nil), R₆ := 1, D₆ := nil, Lista := [ 1 | L₆ ] }

(podriamos usar la 3, pero va a dar falla después... )
(+3 7) preorder(bin(nil,2,nil),[]), preorder(nil,LD₆).         S₇ = { YS₇ := LD₆, YS₇ := L₆, LI₆ = [] }
       falla, no unifica ni con 1 ni con 2.
(backtrack)     

(+4  7) append(XS₇,YS₇,L₇), preorder(bin(nil,2,nil),[X₇|XS₇]), preorder(nil,YS₇).     S₇ = mgu { LI₆ ≟ [X₇|XS₇] , LD₆ ≟ YS₇, L₆ ≟ [X₇|L₇] }
(+3  8) preorder(bin(nil,2,nil),[X₇|[]]), preorder(nil,YS₈).                          S₈ = mgu { XS₇ ≟ [], YS₇ ≟ YS₈, L₇ ≟ YS₈}
(+2  9) append(LI₉,LD₉,[]), preorder(nil,LI₉), preorder(nil,LD₉), preorder(nil,YS₈).  S₉ = mgu { bin(nil,2,nil) ≟ bin(I₉,R₉,D₉), [X₇|[]] ≟ [R₉|L₉] } 
                                                                                         = { R₉ := 2, I₉ := nil, D₉ := nil, X₇ := 2, L₉ := []}
(+3 10) preorder(nil,[]), preorder(nil,[]), preorder(nil,YS₈).                       S₁₀ = mgu { [] ≟ LI₉, YS₁₀ ≟ LD₉, YS₁₀ ≟ []}
                                                                                         = {LI₉ := [], YS₁₀ := [], LD₉ := []}
(+1 11) preorder(nil,[]), preorder(nil,YS₈).
(+1 12) preorder(nil,YS₈).
(+1 13) ☐                                                                            S₁₃ = { YS₈ ≟ [] }

{ YS₈ ≟ [] }·{ X₇ := 2 }·{ L₇ := YS₈ }·{ L₆ := [X₇|L₇] }·{ Lista := [ 1 | L₆ ] } = { Lista = [ 1 | [ 2 | [] ]]} = { Lista = [1,2] }