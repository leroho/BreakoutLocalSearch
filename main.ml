let func_eval = fun c ->
  let sum_weight = 0. in
  Graph.SS.fold (fun a b -> a.Graph.weight +. b) c sum_weight

let bls = fun graph ->
  let c = ref Move.first_solution graph in
  let pa = ref Pqueue.empty in
  let om = ref Move.create_om graph c in
  let fc = ref (func_eval c) in
  let c_best = ref !c in
  let f_best = ref !fc in
  let cp = ref !c in
  let w = ref 0 in
  let nb_iter = ref 0 in
  while (not Pqueue.is_empty p) && (not Pqueue.is_empty om) do
    if Pqueue.is_empty p then
      let m = Move.M2 in
      Move.apply_move m c pa om fc
    else if Pqueue.is_empty om then
      let m = Move.M1 in
      Move.apply_move m c pa om fc
    else
      (let m = Move.best_move in
      Move.apply_move m c pa om fc)
        if 
        
      
  
