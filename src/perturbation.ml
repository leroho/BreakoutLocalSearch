(*let ordonne = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert b.Graph.weight b a) q l;;
let func_eval = fun c ->
  let sum_weight = 0. in
  Graph.SS.fold (fun a b -> a.Graph.weight +. b) c sum_weight;;*)
(*choisi le premier movement de la list*)
let choose_move = fun move_list ->
  match move_list with 
    [] -> failwith "move_list vide!"
  | tete::queue -> tete
        
let cmp = fun x y -> 0-(compare x y)
    
(*let update_vertex = fun graph c move ->
  match move with
  Move.M1 -> Move.new_pa_m1 graph new_queue v
  | Move.M2 -> Move.new_pa_m2 graph c*)
    
let not_prohibited = fun m tl iter pa om ->
  match m with 
    Move.M1 ->
      if Pqueue.is_empty pa then false else true
  | Move.M2 ->
      if Pqueue.is_empty om then false
      else
        let (prio, (u,v), reste_om) = Pqueue.extract ~cmp om in
        let i = v.Graph.id - 1 in 
        let (last_iter, gamma) = tl.(i) in
        (iter - last_iter) > gamma 
  | Move.M3 node -> let i = node.Graph.id - 1 in
    let (last_iter, gamma) = tl.(i) in
				    (iter - last_iter) > gamma
  | _ -> failwith "not_prohibited:mouvement M4 non utilisée"    
        
let compute_set_A = fun c pa om f_best obj node tl iter ->
  let delta = fun m ->
    match m with
      Move.M1 ->
        if Pqueue.is_empty pa then 0. -. (float_of_int max_int) else (Move.eval_move Move.M1 pa om obj ) -. obj
    | Move.M2 ->
        if Pqueue.is_empty om then 0. -. (float_of_int max_int) else (Move.eval_move Move.M2 pa om obj ) -. obj
    | Move.M3 node -> (Move.eval_move (Move.M3 node) pa om obj ) -. obj
    | _ -> failwith "compute_set_A:mouvement M4 non utilisée"
  in
  let move_list =  Move.M1 ::  Move.M2 ::  (Move.M3 node) :: [] in
  let authorized_mv_list = List.fold_left (fun a m ->
    let delta_m = delta m in
    if (not_prohibited m tl iter pa om) || (delta_m +. obj > f_best) then (m, delta_m) :: a else a) [] move_list in
  let rec max_m = fun l ->
    match l with
      [] -> ( Move.M1, 0. -. float_of_int max_int)
    | (a,b)::queue ->
        let (m_queue, delta_queue) = max_m queue in
        if (b >delta_queue) then (a,b)
        else (m_queue, delta_queue) in
  let (move_max, delta_max) = max_m authorized_mv_list in
  if delta_max = (0. -. float_of_int max_int) then []
  else 
    let set_a = List.fold_left (fun a (m,delta_m) ->
      if m <> move_max && delta_m = delta_max then m::a else a ) [] authorized_mv_list in
    move_max::set_a
    
let compute_p = fun w t p ->
  let proba = exp ((float_of_int (0 - w)) /. t) in
  if  proba > p then proba
  else p
          
          
let perturbation = fun graph pa om obj c l tl iter w alpha_r alpha_s t p0 f_best ->
  Random.self_init () ;
  (*Printf.printf "\nin perturbation-----------------------------------------------\n";
  Printf.printf "w :       	    %d\n" w;
  Printf.printf "l :              %d\n" l;*)
  let perturb = fun c l type_perturb alpha -> 
    for i=1 to l do
	Random.self_init () ;
    begin
	if type_perturb = 0 then
	  begin
	    let move = Move.M4 alpha in
	    Move.apply_move move graph c pa om obj !iter tl;
	  end
	else
	  begin
	    let node = (Array.of_list (Graph.SS.elements !c)).(Random.int (Graph.SS.cardinal !c)) in
	    let move_array = compute_set_A c !pa !om !f_best !obj node tl !iter in
            match move_array with
              [] -> ()
            | _ ->
                begin
                let move = choose_move move_array in
				Move.apply_move move graph c pa om obj !iter tl;
                end
	end;
	iter := !iter + 1
      end
    done in
  
  if w = 0 then
     perturb c l 0 alpha_s
  else
    begin
      let p = compute_p w t p0 in
      if p < Random.float 1. then
         perturb c l 1 0.
      else
        perturb c l 0 alpha_r
    end
      
