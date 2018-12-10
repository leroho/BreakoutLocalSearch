(* authorized graph u l renvoie la liste des noeuds de la liste l qui sont voisins de u *)
let authorized = fun graph u l->
  List.filter (fun a-> Graph.is_voisin graph a u) l

(* first_solution u graph renvoie une clique maxiximale (dans le sens on ne peut plus ajouter de noeud à la clique) contenant le noeud u *)
let first_solution = fun u graph -> 
  let rec update = fun c auth_list ->	(* mettre à jour la liste des noeuds autorisés à être ajouter à la clique *)
    match auth_list with
      [] ->  c							(* on arrête quand on ne peut plus ajouter de noeud à la clique *)
    |tete::queue -> update (Graph.SS.add tete c) (authorized graph tete queue) in
  update (Graph.SS.singleton u) (Graph.get_voisins graph u)

(* list_to_pqueue l renvoie une pqueue composée des noeuds de la liste l ordonnés selon leurs poids *)
let list_to_pqueue = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert b.Graph.weight b a) q l
    
(* create_pa_liste graph c renvoie une pqueue composé des noeuds autorisés à être ajouté à la clique c *)
let create_pa_list = fun graph c->
  let rec create = fun l current_pa->
    match l with
      [] -> current_pa
    | tete::queue -> create queue (authorized graph tete current_pa) in
  create (Graph.SS.elements c) (Array.to_list graph.Graph.nodes)

(* mise à jour de pa après un mouvement de type M1 *)
let new_pa_m1 = fun graph new_queue v->
  list_to_pqueue (authorized graph v (Pqueue.elements new_queue))

(* mise à jour de pa après un mouvement de type M2 *)  
let new_pa_m2 = fun graph c->
  let pa_list = create_pa_list graph c in
  list_to_pqueue pa_list

(* possible_swap graph u c renvoie la liste des couple de noeuds (u, v) où v est un noeud voisin de tous les noeuds de la clique c sauf u *)  
let possible_swap = fun graph u c->
  let l = create_pa_list graph (Graph.SS.remove u c) in
  List.filter (fun a -> not (Graph.is_voisin graph a u)) l
     
(* create_om graph c revoie la pqueue de tous les couple de noeuds (u, v) pouvant être "swaper" ordonnés selon (v.weight - u.weight) *) 
let create_om = fun graph c ->
  let om = Pqueue.empty in
  let insert_possible_swap = fun u pqueue->
    List.fold_left (fun a b -> Pqueue.insert (b.Graph.weight -. u.Graph.weight) (u, b) a) pqueue (possible_swap graph u c)in
  List.fold_left (fun a b -> insert_possible_swap b a) om (Graph.SS.elements c)
    
let ()=
  let demo = Graph.graph_demo () in
  let u = Graph.get_node_id demo 3 in
  let init = first_solution u demo in
  Printf.printf "%d" (List.length (Graph.SS.elements init));
  Draw.draw demo init;;
