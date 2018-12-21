let func_eval = fun c ->
  Graph.SS.fold (fun a b -> a.Graph.weight + b) c 0
    
let cmp = fun x y-> 0-(compare x y)

let in_local_search = fun graph nbIter tl pa om c ->
  let rec accept = fun pqueue_om->
    if Pqueue.is_empty pqueue_om then false
    else
      let (prio, (u, v), reste_om) = Pqueue.extract ~cmp pqueue_om in
      if prio > 0 then true
      else if prio = 0 then
        let n_c = Graph.SS.add v c in
        let new_c = Graph.SS.remove u n_c in
        let new_pa = Move.new_pa_m2 graph new_c in
        let new_om = Move.create_om graph new_c in
        
        if List.length (Pqueue.elements new_pa) > List.length (Pqueue.elements pa) ||
        (if not (Pqueue.is_empty new_om) then let (new_prio, _, _) = Pqueue.extract ~cmp new_om in new_prio > 0 else false) then true
        else accept reste_om
            
      else false in
  
  let pa_cond = not (Pqueue.is_empty pa) in
  let om_cond = accept om in
  pa_cond || om_cond

let isClique = fun graph clique ->
  (* on teste chaque élément de la clique *)
  let checkElt = fun u ->
    let checkOthers = fun v ->
      if (u=v) then true
      else Graph.is_voisin graph u v
    in  
    Graph.SS.for_all checkOthers clique
  in
  Graph.SS.for_all checkElt clique
    
let bls = fun graph t l0 lmax->
  let time_start = Unix.time () in
  (*Graphics.open_graph (Printf.sprintf " %dx%d+50-0" 800 800);*)
  Random.self_init () ;
  let c = ref (Move.first_solution graph) in
  let pa = ref Pqueue.empty in
  let om = ref (Move.create_om graph !c) in
  let fc = ref (func_eval !c) in
  (*let fc_array = ref [|(0, !fc * 10)|] in*)
  let cbest = ref !c in
  let fbest = ref !fc in
  let cp = ref !c in
  let w = ref 0 in
  let l = ref l0 in
  let tl = Array.map (fun a -> (0-max_int, 0)) graph.Graph.nodes in
  let nbIter = ref 0 in
  
  let alpha_r = 0.85 and alpha_s=0.4 and p0 = 0.6 in
  
  while !nbIter < 2000 do
    while in_local_search graph !nbIter tl !pa !om !c do
      if Pqueue.is_empty !pa then
	(let m = Move.M2 in
	Move.apply_move m graph c pa om fc !nbIter tl)
      else if Pqueue.is_empty !om then
	(let m = Move.M1 in
	Move.apply_move m graph c pa om fc !nbIter tl)
      else
	(let m = Move.best_move !pa !om !fc in
	Move.apply_move m graph c pa om fc !nbIter tl);
      nbIter := !nbIter + 1
    done;
    (*fc_array:= Array.append !fc_array [|(!nbIter/2, !fc * 10)|];
    Draw.draw graph !c;*)
    begin
      if fc > fbest then
        (cbest := !c;
         fbest := !fc;
         w := 0) 
      else
        w := !w + 1
    end;
    begin
      if float_of_int !w > t then
        (l := lmax;
         w := 0)
      else if !c = !cp then
        l := !l + 1
      else
        l := l0
    end;
    
    cp := !c;
    Perturbation.perturbation graph pa om fc c !l tl nbIter !w alpha_r alpha_s t p0 fbest;
    (*fc_array:= Array.append !fc_array [|(!nbIter/2, !fc * 10)|];
    Graphics.draw_poly_line !fc_array;
    Draw.draw graph !c;*)
    l := l0
  done;
  let time_end = Unix.time () in
  Printf.printf "durrée : %f\n" (time_end -. time_start);
  Printf.printf "fbest: %d\n" !fbest;
  !cbest;;

let ()=
  let demo = Graph.create_graph_DIMACS "data/dimacs.txt" in(*Graph.generate_random_graph 200 0.5 5*)
  let c = bls demo 100. 2 20 in ();
  Graph.SS.iter (fun node -> Printf.printf "%d," node.Graph.id) c;
  if isClique demo c then Printf.printf "\nOK";;
