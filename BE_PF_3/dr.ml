(*  Module qui permet la décomposition et la recomposition de données **)
(*  Passage du type t1 vers une liste d'éléments de type t2 (décompose) **)
(*  et inversement (recopose).**)
module type DecomposeRecompose =
sig
  (*  Type de la donnée **)
  type mot
  (*  Type des symboles de l'alphabet de t1 **)
  type symbole

  val decompose : mot -> symbole list
  val recompose : symbole list -> mot
end

(*Module qui implémente la décomposition et la recomposition de donnée avec en mot un string et en symbole un char*)
(*Passage du type string vers une liste d'éléments de type char (decompose)*)
(*et inversement (recompose)*)
module DRString : DecomposeRecompose with type mot = string and type symbole = char =
struct
    (*  Type de la donnée **)
    type mot = string
    (*  Type des symboles de l'alphabet de t1 **)
    type symbole = char

    let decompose s = 
      let rec decompose_rec i accu =
        if i < 0 then accu
        else decompose_rec (i-1) (s.[i]::accu)
      in decompose_rec (String.length s - 1) []

    let recompose lc = 
      List.fold_right (fun t q -> String.make 1 t ^ q) lc ""
  
end

(*Module qui implémente la décomposition et la recomposition de donnée avec en mot un nombre et en symbole un entier entre 0 et 9*)
(*Passage du type string vers une liste d'éléments de type char (decompose)*)
(*et inversement (recompose)*)
module DRString : DecomposeRecompose with type mot = int and type symbole = int =
struct
    (*  Type de la donnée **)
    type mot = int
    (*  Type des symboles de l'alphabet de t1 **)
    type symbole = int

    let decompose s = 
      let rec decompose_rec i accu =
        if i < 0 then accu
        else decompose_rec (i-1) (s.[i]::accu)
      in decompose_rec (Int.length s - 1) []

    let recompose lc = 
      List.fold_right (fun t q -> Int.make 1 t ^ q) lc 
  
end