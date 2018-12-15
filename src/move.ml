type t = M1 | M2 | M3 of Graph.node  | M4 of float

let cmp = fun x y -> 0-(compare x y)

(* authorized graph u l renvoie la liste des noeuds de la liste l qui sont voisins de u *)
let authorized = fun graph u l->
  List.filter (fun a-> Graph.is_voisin graph a u) l

(* first_solution graph renvoie une clique maxiximale (dans le sens on ne peut plus ajouter de noeud à la clique) dont le noeud initial est choisi au hasard *)
let first_solution = fun graph ->
  Random.self_init () ;
  let u = Graph.get_node_id graph (1 + Random.int (Array.length graph.Graph.nodes)) in
  let rec update = fun c auth_list ->	(* mettre à jour la liste des noeuds autorisés à être ajouter à la clique *)
    match auth_list with
      [] ->  c							(* on arrête quand on ne peut plus ajouter de noeud à la clique *)
    |tete::queue -> update (Graph.SS.add tete c) (authorized graph tete queue) in
  update (Graph.SS.singleton u) (Graph.get_voisins graph u)

(* list_to_pqueue l renvoie une pqueue composée des noeuds de la liste l ordonnés selon leurs poids *)
let list_to_pqueue = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert ~cmp b.Graph.weight b a) q l
    
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
    List.fold_left (fun a b -> Pqueue.insert ~cmp (b.Graph.weight -. u.Graph.weight) (u, b) a) pqueue (possible_swap graph u c) in
  List.fold_left (fun a b -> insert_possible_swap b a) om (Graph.SS.elements c)

let eval_move = fun m pa om obj->
  match m with
  M1 ->
  begin
    let (prio,_,_) = Pqueue.extract ~cmp pa in
    obj +. prio
  end
  | M2 ->
  begin
    let (prio,_,_) = Pqueue.extract ~cmp om in
    obj +. prio
  end
  | M3 node -> 
	obj -. (Graph.get_weight node) 
  | _ -> failwith "non utilisée"
  
let best_move = fun pa om obj ->
    let obj_m1 = eval_move M1 pa om obj in
    let obj_m2 = eval_move M2 pa om obj in
    if (obj_m1 < obj_m2) then M2 else M1

let apply_move = fun m graph c pa om obj ->
    match m with
    M1 -> 
    begin
		Printf.printf "\nmove : M1\n";
      let (prio, v, reste_pa) = Pqueue.extract ~cmp !pa in
      if !obj +. prio > !obj then
        (
		Printf.printf "objectif courant : 		%f\n" !obj;
		Printf.printf "clique améliorable !\n";
		Printf.printf "noeud ajouté : 			%d\n" v.Graph.id;
		Printf.printf "nouvel objectif : 		%f\n" (!obj +. prio);
        c := Graph.SS.add v !c;
        obj := !obj +. prio;
        pa := new_pa_m1 graph reste_pa v;
        om := create_om graph !c;
		Printf.printf "taille de la clique : 			%d\n" (List.length (Graph.SS.elements !c));
        )
    end
    | M2 ->
        begin
	  Printf.printf "\nmove : M2\n";
          let (prio, (u, v), reste_om) = Pqueue.extract ~cmp !om in
          if !obj +. prio > !obj then
            (
	     Printf.printf "objectif courant : 		%f\n" !obj;
	     Printf.printf "clique améliorable !\n";
	     Printf.printf "noeud ajouté : 	        %d\n" v.Graph.id;
	     Printf.printf "noeud retirer : 		%d\n" u.Graph.id;
	     Printf.printf "nouvel objectif : 	        %f\n" (!obj +. prio);
             c := Graph.SS.add v !c;
             c := Graph.SS.remove u !c;
             obj := !obj +. prio;
             pa := new_pa_m2 graph !c;
             om := create_om graph !c;
	     Printf.printf "taille de la clique : 			%d\n" (List.length (Graph.SS.elements !c));
            )
        end
    | M3 node ->
	Printf.printf "\nmove :                 M3\n";
	Printf.printf "noeud retire :           %d\n" node.Graph.id;
        begin
          c := Graph.SS.remove node !c;
          obj := !obj -. node.Graph.weight;
          pa := new_pa_m2 graph !c;
          om := create_om graph !c;
        end;
	Printf.printf "nouvel objectif :       	%f\n" (!obj);
        Printf.printf "taille de la clique :   	%d\n" (List.length (Graph.SS.elements !c));
    | M4 alpha ->
	Printf.printf "\nmove :                 M4\n";
        begin
          let list_nodes = Array.to_list graph.Graph.nodes in
          let rec iter_list = fun l ->
            match l with
              [] -> ()
            | v::queue ->
                begin
                  let exist = Graph.SS.exists (fun x -> x=v) !c in
                  let neighbour = ref Graph.SS.empty in
                  let somme = (Graph.SS.fold
                                 (fun x somme -> if (Graph.is_voisin graph x v) then
                                   (
                                    neighbour := Graph.SS.add x !neighbour;
                                    x.Graph.weight +. somme
                                   )
                                 else
                                   somme
                                 ) !c v.Graph.weight) in
                  let condition = somme > alpha *. !obj in 
                  if ((not exist) && condition) then
                    begin
                      obj := somme;
                      c := Graph.SS.add v !neighbour;
                      pa := new_pa_m2 graph !c;
                      om := create_om graph !c;
                    end
                  else
                    iter_list queue
                end
          in
          iter_list list_nodes
        end;
	Printf.printf "nouvel objectif :       	%f\n" (!obj);
	Printf.printf "taille de la clique :  	%d\n" (List.length (Graph.SS.elements !c));
          
    
(*let ()=
  let demo = Graph.generate_random_graph 10 0.5 10. in
  let init = first_solution demo in
  Printf.printf "%d" (List.length (Graph.SS.elements init));
  Draw.draw demo init;;*)
