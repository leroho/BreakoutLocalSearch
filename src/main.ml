let func_eval = fun c ->
  let sum_weight = 0. in
  Graph.SS.fold (fun a b -> a.Graph.weight +. b) c sum_weight

let cmp = fun x y -> 0-(compare x y)
  
let bls = fun graph ->
  let c = ref (Move.first_solution graph) in
  let pa = ref Pqueue.empty in
  let om = ref (Move.create_om graph !c) in
  let fc = ref (func_eval !c) in
  let nbIter = ref 0 in
  
  while (not (Pqueue.is_empty !pa)) || (not (Pqueue.is_empty !om) && (let (prio, (u, v), reste_om) = Pqueue.extract ~cmp !om in prio)>0.) do
    if Pqueue.is_empty !pa then
		(let m = Move.M2 in
		Move.apply_move m graph c pa om fc)
    else if Pqueue.is_empty !om then
		(let m = Move.M1 in
		Move.apply_move m graph c pa om fc)
    else
		(let m = Move.best_move !pa !om !fc in
		Move.apply_move m graph c pa om fc);
	nbIter := !nbIter + 1
  done;
  !c;;

let ()=
  let demo = Graph.generate_random_graph 100 0.5 10. in
  let c = bls demo in ();;
  (*Draw.draw demo c;;*)
