(* Evaluation des expressions simples *)


(* Module abstrayant les expressions *)
module type ExprSimple =
sig
  type t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t-> t -> t
end

(*Module abstrayant les variables*)
module type ExprVar =
sig
  type t
  val def : string -> t -> t -> t
  val var : string -> t
end

module type Expr =
sig
include ExprVar
include (ExprSimple with type t := t)
end

(* Module réalisant l'évaluation d'une expression *)
module EvalSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = c
  let plus e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2
end

(*Module réalisant l'affichage en chaines de caractère d'une chaine d'expression *)
module PrintSimple : ExprSimple with type t = string =
struct
  type t = string
  let const c = string_of_int c
  let plus e1 e2 = "("^e1^"+"^e2^")"

  let mult e1 e2 = "("^e1^"*"^e2^")"
end

(*Module permettant de compter les opérations dans une expression*)
module CompteSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = 0*c
  let plus e1 e2 = 1 + e1 + e2
  let mult e1 e2 =  1 + e1 + e2
end

(* Solution 1 pour tester *)
(* A l'aide de foncteur *)

(* Définition des expressions *)
module ExemplesSimples (E:ExprSimple) =
struct
  (* 1+(2*3) *)
  let exemple1  = E.(plus (const 1) (mult (const 2) (const 3)) )
  (* (5+2)*(2*3) *)
  let exemple2 =  E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )
end

(* Définition des expressions *)
module Exemples (E:Expr) =
struct
  let exemple = E.(def "x" (plus (const 1) (const 2)) (mult (var "x") (const 3)))
end

(*Convertir les expressions avec variables en chaine de caractère*)
module PrintVar : ExprVar with type t = string =
struct
  type t = string
  let def s1 e1 e2 = "let "^s1^"="^e1^" in "^e2
  let var s1 = s1
end 

module Print : Expr with type t = string =
struct
  include PrintVar
  include PrintSimple
end

(* Module d'évaluation des exemples *)
module EvalExemples =  ExemplesSimples (EvalSimple)

let%test _ = (EvalExemples.exemple1 = (7 : int))
let%test _ = (EvalExemples.exemple2 = (42 : int))

module EvalExemples2 =  ExemplesSimples (PrintSimple)

let%test _ = (EvalExemples2.exemple1 = ("(1+(2*3))" : string))
let%test _ = (EvalExemples2.exemple2 = ("((5+2)*(2*3))" : string))

module EvalExemples3 =  ExemplesSimples (CompteSimple)

let%test _ = (EvalExemples3.exemple1 = (2 : int))
let%test _ = (EvalExemples3.exemple2 = (3 : int))