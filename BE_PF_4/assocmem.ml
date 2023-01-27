open Util
open Mem

(* CONTRAT :get_assoc:
Renvoie la valeur associée à la clef e dans la liste l ou la valeur fournie def si la clé n'existe pas
parmaètres : e la clef recherchée
             l liste dans laquelle on recherche une clef
             def valeur par défaut renvoyée si la clef n'est pas trouvée
 *)
let rec get_assoc e l def = match l with
    | (clef,x)::q when clef = e -> x
    | _ :: q -> get_assoc e q def
    | [] -> def

(* Tests unitaires : TODO *)
let%test _ = get_assoc 3 [(1,2);(2,3);(3,4)] 0 = 3
let%test _ = get_assoc 8 [(1,2);(2,3);(3,4)] 0 = 0
let%test _ = get_assoc 1 [(1,2);(2,3);(3,4)] 0 = 2

(* set_assoc : TODO
 *)
let rec set_assoc e l x = match l with
    | (clef,x)::q when clef = e -> (e,x)::q
    | _ :: q -> set_assoc e q x
    | [] -> (e,x)::l

(* Tests unitaires : TODO *)


module AssocMemory : Memory =
struct
    (* Type = liste qui associe des adresses (entiers) à des valeurs (caractères) *)
    type mem_type = (int*char) list

    (* Un type qui contient la mémoire + la taille de son bus d'adressage *)
    type mem = int * mem_type

    (* Nom de l'implémentation *)
    let name = "assoc"

    (* Taille du bus d'adressage *)
    let bussize (bs, _) = bs

    (* Taille maximale de la mémoire *)
    let size (bs, _) = pow2 bs

    (* Taille de la mémoire en mémoire *)
    let allocsize (bs, m) = List.filter ( ) m

    (* Nombre de cases utilisées *)
    let busyness (bs, m) = failwith "TODO"

    (* Construire une mémoire vide *)
    let clear bs = failwith

    (* Lire une valeur *)
    let read (bs, m) addr = addr

    (* Écrire une valeur *)
    let write (bs, m) addr x = x
end
