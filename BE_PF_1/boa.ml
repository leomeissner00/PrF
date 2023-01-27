(*CONTRAT
   Module qui détermine les règles *)
module type Regle =
sig
  type tid = int
  type td
  val id : tid
  
  (*CONTRAT
    Fonction qui applique la règle définie dans ce module
    Paramètre : td le type des formes
    Retour Liste de type de forme*)
  val appliquer : td -> td list
end

module Regle1 : Regle with type td = char =
struct
  type tid = int
  type td = char

  let id = 1

  let appliquer c =  
end



(*module type ArbreReecriture =
sig
  (*
  type tid = int
  type td
  type arbre_reecriture = ...

  val creer_noeud : ...

  val racine : ...
  val fils : ..

  val appartient : td -> arbre_reecriture -> bool
  *)
end*)