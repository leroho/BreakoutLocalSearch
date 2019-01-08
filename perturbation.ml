(*let ordonne = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert b.Graph.weight b a) q l;;
let func_eval = fun c ->
  let sum_weight = 0. in
  Graph.SS.fold (fun a b -> a.Graph.weight +. b) c sum_weight;;*)
(*choisi le premier movement de la list*)
let choose_move = fun move_list ->
  (Array.of_list move_list).(Random.int (List.length move_list))
        
let cmp = fun x y -> 0-(compare x y)
    
(*let update_vertex = fun graph c move ->
  match move with
  Move.M1 -> Move.new_pa_m1 graph new_queue v
  | Move.M2 -> Move.new_pa_m2 graph c*)
    
let not_prohibited = fun m tl iter pa om ->
  match m with 
    Move.M1 ->
      if Pqueue.is_empty pa then false
	  else
		let (prio, v, reste_pa) = Pqueue.extract ~cmp pa in
        let i = v.Graph.id - 1 in 
        let (last_iter, gamma) = tl.(i) in
        (iter - last_iter) > gamma 
  | Move.M2 ->
      if Pqueue.is_empty om then false
      else
        let (prio, (u,v), reste_om) = Pqueue.extract ~cmp om in
        let i = v.Graph.id - 1 in 
        let (last_iter, gamma) = tl.(i) in
        (iter - last_iter) > gamma 
  | Move.M3 node -> true
  | _ -> failwith "not_prohibited:mouvement M4 non utilisée"    
        
let compute_set_A = fun c pa om f_best obj tl iter ->
  let rec delta = fun m pqueue_pa pqueue_om ->
    match m with
      Move.M1 ->
        if Pqueue.is_empty pqueue_pa then (0 - max_int) 
		else 
			let (prio, v, reste_pa) = Pqueue.extract ~cmp pqueue_pa in
			if Move.not_prohibited m tl iter pqueue_pa pqueue_om then prio
			else delta m reste_pa pqueue_om
    | Move.M2 ->
        if Pqueue.is_empty pqueue_om then (0 - max_int) 
		else 
			let (prio, (u, v), reste_om) = Pqueue.extract ~cmp pqueue_om in
			if Move.not_prohibited m tl iter pqueue_pa pqueue_om then prio
			else delta m pqueue_pa reste_om
    | Move.M3 node -> 
		if Move.not_prohibited m tl iter pqueue_pa pqueue_om then 0 - Graph.get_weight node
		else 0 - max_int
    | _ -> failwith "compute_set_A:mouvement M4 non utilisée"
  in
  let move_list =  Move.M1 ::  Move.M2 ::  List.map (fun node -> Move.M3 node) (Graph.SS.elements c) in
  let authorized_mv_list = List.fold_left (fun a m ->
    let delta_m = delta m pa om in
    if (delta_m > 0 - max_int) || (delta_m + obj > f_best) then (m, delta_m) :: a else a) [] move_list in
  let rec max_m = fun l ->
    match l with
      [] -> ( Move.M1, 0 - max_int)
    | (a,b)::queue ->
        let (m_queue, delta_queue) = max_m queue in
        if (b >delta_queue) then (a,b)
        else (m_queue, delta_queue) in
  let (move_max, delta_max) = max_m authorized_mv_list in
  if delta_max = (0 - max_int) then []
  else 
    List.fold_left (fun a (m,delta_m) -> if delta_m = delta_max then m::a else a ) [] authorized_mv_list
    
let compute_p = fun w t p ->
  let proba = exp ((float_of_int (0 - w)) /. t) in
  if  proba > p then proba
  else p
          
          
let perturbation = fun graph pa om obj c l tl iter w alpha_r alpha_s t p0 f_best ->
  let perturb = fun c l type_perturb alpha -> 
    for i=1 to l do
    begin
	if type_perturb = 0 then
	  begin
	    let move = Move.M4 alpha in
	    Move.apply_move move graph c pa om obj !iter tl;
	  end
	else
	  begin
	    let move_array = compute_set_A !c !pa !om !f_best !obj tl !iter in
            match move_array with
              [] -> ()
            | _ ->
                begin
					let move = choose_move move_array and pert = 1 in
					Move.apply_move ~pert move graph c pa om obj !iter tl
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
      