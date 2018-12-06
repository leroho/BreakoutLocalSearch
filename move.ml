let authorized = fun graph u l->
  List.filter (fun a-> Graph.is_voisin graph a u) l

let first_solution = fun u graph -> 
  let rec update = fun c auth_list ->
    match auth_list with
      [] ->  c
    |tete::queue -> update (Graph.SS.add tete c) (authorized graph tete queue) in
  update (Graph.SS.singleton u) (Graph.get_voisins graph u)

let list_to_pqueue = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert b.Graph.weight b a) q l
    
let create_pa_list = fun graph c->
  let rec create = fun l current_pa->
    match l with
      [] -> current_pa
    | tete::queue -> create queue (authorized graph tete current_pa) in
  create (Graph.SS.elements c) (Array.to_list graph.Graph.nodes)

let new_pa_m1 = fun graph new_queue v->
  list_to_pqueue (authorized graph v (Pqueue.elements new_queue))

let new_pa_m2 = fun graph c->
  let pa_list = create_pa_list graph c in
  list_to_pqueue pa_list

let possible_swap = fun graph u c->
  let l = create_pa_list graph (Graph.SS.remove u c) in
  List.filter (fun a -> not (Graph.is_voisin graph a u)) l
      
let create_om = fun graph c ->
  let om = Pqueue.empty in
  let insert_possible_swap = fun u pqueue->
    List.fold_left (fun a b -> Pqueue.insert (b.Graph.weight -. u.Graph.weight) (u, b) a) pqueue (possible_swap graph u c)in
  List.fold_left (fun a b -> insert_possible_swap b a) om (Graph.SS.elements c)

    
(*let ()=
  let demo = Graph.generate_random_graph 5 0.7 10. in
  let u = Graph.get_node_id demo 3 in
  let init = Graph.SS.elements (first_solution u demo) in
  init;()
*)
