(* Exercice 3 *)
module type Expression = sig
  (* Type pour représenter les expressions *)
  type exp


  (* eval : exp -> int *)
  (* Permet d’évaluer la valeur d’une expression *)
  (* Paramètre : Expression que l'on veut évaluer *)
  (* Résultat : Valeur de l'expression *)
  val eval : exp -> int
end

(* Exercice 4 *)

(* TO DO avec l'aide du fichier  expressionArbreBinaire.txt *)
module ExpAvecArbreBinaire : Expression =
struct
  (* Type pour représenter les expressions binaires *)
  type op = Moins | Plus | Mult | Div
  type exp = Binaire of exp * op * exp | Entier of int

  (* eval *)
  let eval =  fun expr -> 
    let rec eval_rec expr_rec = match expr_rec with
  | Entier(a) -> a
  | Binaire(exp1, oper, exp2) -> 
    match oper with
      |Moins -> (eval_rec exp1) - (eval_rec exp2)
      |Plus -> (eval_rec exp1) + (eval_rec exp2)
      |Mult -> (eval_rec exp1) * (eval_rec exp2)
      |Div -> (eval_rec exp1) / (eval_rec exp2)
  in eval_rec expr
    
    (*match oper with 
    | Moins -> e1-e2
    | Plus -> e1+e2
    | Mult -> e1*e2
    | Div-> e1/e2*)
(* Tests : TO DO *)
let%test _ = eval (Entier(1)) = 1
let%test _ = eval (Binaire(Entier(2),Plus,Entier(2))) = 4
let%test _ = eval (Binaire(Entier(2),Moins,Entier(2))) = 0
let%test _ = eval (Binaire(Entier(2),Mult,Entier(3))) = 6
let%test _ = eval (Binaire(Entier(2),Div,Entier(2))) = 1
let%test _ = eval (Binaire(Entier(2),Div,Entier(1))) = 2
let%test _ = eval (Binaire(Binaire(Entier(6), Moins, Entier(5)),Plus,Entier(1))) = 2
let%test _ = eval (Binaire(Entier(2),Mult,Entier(10000))) = 20000
end

(* Exercice 5 *)

module ExpAvecArbreNaire : Expression =
struct
    (* Linéarisation des opérateurs binaire associatif gauche et droit *)
    type op = Moins | Plus | Mult | Div
    type exp = Naire of op * exp list | Valeur of int
    (*J'aurai mis ici exception Malformee pour déclarer l'exception Malformee*)
    
  (* bienformee : exp -> bool *)
  (* Vérifie qu'un arbre n-aire représente bien une expression n-aire *)
  (* c'est-à-dire que les opérateurs d'addition et multiplication ont au moins deux opérandes *)
  (* et que les opérateurs de division et soustraction ont exactement deux opérandes.*)
  (* Paramètre : l'arbre n-aire dont ont veut vérifier si il correspond à une expression *)
  let  bienformee = fun expr -> 
    let rec bienformee_rec expr_rec = match expr_rec with
    | Valeur(_) -> true
    | Naire(oper, expl) -> 
      match oper with
        |Moins -> if (List.length expl) = 2 then
          List.for_all (Bool.equal true) (List.map bienformee_rec expl) else false
        |Plus -> if (List.length expl) >= 2 then
          List.for_all (Bool.equal true) (List.map bienformee_rec expl) else false
        |Mult -> if (List.length expl) >= 2 then
          List.for_all (Bool.equal true) (List.map bienformee_rec expl) else false
        |Div -> if (List.length expl) = 2 then
          List.for_all (Bool.equal true) (List.map bienformee_rec expl) else false
    in bienformee_rec expr
  let en1 = Naire (Plus, [ Valeur 3; Valeur 4; Valeur 12 ])
  let en2 = Naire (Moins, [ en1; Valeur 5 ])
  let en3 = Naire (Mult, [ en1; en2; en1 ])
  let en4 = Naire (Div, [ en3; Valeur 2 ])
  let en1err = Naire (Plus, [ Valeur 3 ])
  let en2err = Naire (Moins, [ en1; Valeur 5; Valeur 4 ])
  let en3err = Naire (Mult, [ en1 ])
  let en4err = Naire (Div, [ en3; Valeur 2; Valeur 3 ])
  
  let%test _ = bienformee en1
  let%test _ = bienformee en2
  let%test _ = bienformee en3
  let%test _ = bienformee en4
  let%test _ = not (bienformee en1err)
  let%test _ = not (bienformee en2err)
  let%test _ = not (bienformee en3err)
  let%test _ = not (bienformee en4err)
  
  (* eval : exp-> int *)
  (* Calcule la valeur d'une expression n-aire *)
  (* Paramètre : l'expression dont on veut calculer la valeur *)
  (* Précondition : l'expression est bien formée *)
  (* Résultat : la valeur de l'expression *)
  let  eval_bienformee =  fun expr ->
    let rec eval_bienformee_rec expr_rec = match expr_rec with
    | Valeur(a) -> a
    | Naire(oper, expl) -> 
      match oper with
        |Moins -> let rec soustr expadd = 
          match expadd with 
          | [] -> 0
          | Valeur(t)::q -> (t)-(soustr q)
          | _ -> 0
        in soustr expl
        |Plus -> let rec addition expadd = 
          match expadd with 
          | [] -> 0
          | Valeur(t)::q -> (t)+(addition q)
          | _ -> 0
        in addition expl
        |Mult ->let rec multi expadd = 
          match expadd with 
          | [] -> 0
          | Valeur(t)::q -> (t)*(multi q)
          | _ -> 0
        in multi expl
        |Div -> let rec div expadd = 
          match expadd with 
          | [] -> 0
          | Valeur(t)::q -> (t)/(div q)
          | _ -> 0
        in div expl
    in eval_bienformee_rec expr
  
  let%test _ = eval_bienformee en1 = 19
  let%test _ = eval_bienformee en2 = 14
  let%test _ = eval_bienformee en3 = 5054
  let%test _ = eval_bienformee en4 = 2527
  
  (* Définition de l'exception Malformee *)
  (* TO DO *)
  
  (* eval : exp-> int *)
  (* Calcule la valeur d'une expression n-aire *)
  (* Paramètre : l'expression dont on veut calculer la valeur *)
  (* Résultat : la valeur de l'expression *)
  (* Exception  Malformee si le paramètre est mal formé *)
  let eval  = fun _ -> assert false
  
  (*let%test _ = eval en1 = 19
  let%test _ = eval en2 = 14
  let%test _ = eval en3 = 5054
  let%test _ = eval en4 = 2527
  
  let%test _ =
    try
      let _ = eval en1err in
      false
    with Malformee -> true
  
  let%test _ =
    try
      let _ = eval en2err in
      false
    with Malformee -> true
  
  let%test _ =
    try
      let _ = eval en3err in
      false
    with Malformee -> true
  
  let%test _ =
    try
      let _ = eval en4err in
      false
    with Malformee -> true*)

  end

(* TO DO avec l'aide du fichier  expressionArbreNaire.txt *)