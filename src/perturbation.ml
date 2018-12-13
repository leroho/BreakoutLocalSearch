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


(*let update_vertex = fun graph c move ->
  match move with
    Move.M1 -> Move.new_pa_m1 graph new_queue v
  | Move.M2 -> Move.new_pa_m2 graph c*)

let not_prohibited = fun m ->
  (1=1)
    
let compute_set_A = fun c pa om f_best obj ->
  let delta = fun m ->
    match m with
      Move.M1 -> (Move.eval_move Move.M1 pa om obj ) -. obj
    | Move.M2 -> (Move.eval_move Move.M2 pa om obj ) -. obj
    | Move.M3 -> (Move.eval_move Move.M3 pa om obj ) -. obj
    | _ -> failwith "mouvement M4 non utilisée"
  in
  let move_list =  Move.M1 ::  Move.M2 ::  Move.M3 :: [] in
  let authorized_mv_list = List.fold_left (fun a m -> if not_prohibited m || delta m > obj then (m, delta m) :: a else a) [] move_list in
  let rec max_m = fun l ->
    match l with
      [] -> ( Move.M1, 0. -. float_of_int max_int)
    | (a,b)::queue ->
        let (m_queue, delta_queue) = max_m queue in
        if (b >delta_queue) then (a,b)
        else (m_queue, delta_queue) in
  let (move_max, delta_max) = max_m authorized_mv_list in
  let set_a = List.fold_left (fun a (m,delta_m) -> if m <> move_max && delta_m = delta_max then m::a else a ) [] authorized_mv_list in
  move_max::set_a;;           
    
let compute_p = fun w t p ->
  let proba = exp ((float_of_int (0 - w)) /. t) in
  if  proba > p then proba
  else p
        
let perturbation = fun graph pa om obj c l tl iter w alpha_r alpha_s t p0 f_best ->
  let perturb = fun c l move_array -> 
    for i=1 to l do
      begin
        let move = choose_move move_array in
        Move.apply_move move graph c pa om obj ;
        (*tl := update_tl m iter;*)
        (*update_vertex graph c move;*)
        iter := !iter + 1
      end
    done;
    !c in
    
  if w = 0 then
    c := perturb c l [Move.M4 alpha_s]
  else
    begin
      let p = compute_p w t p0 in
      if p < Random.float 1. then
        begin
          let a = compute_set_A c !pa !om !f_best !obj in
          c := perturb c l a
        end
      else
        c := perturb c l [Move.M4 alpha_r]
    end
    
  

