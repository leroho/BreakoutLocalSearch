let func_eval = fun c ->
  let sum_weight = 0. in
  Graph.SS.fold (fun a b -> a.Graph.weight +. b) c sum_weight

let cmp = fun x y-> 0-(compare x y)
  
let bls = fun graph t l0 lmax->
  let c = ref (Move.first_solution graph) in
  let pa = ref Pqueue.empty in
  let om = ref (Move.create_om graph !c) in
  let fc = ref (func_eval !c) in
  let cbest = ref !c in
  let fbest = ref !fc in
  let cp = ref !c in
  let w = ref 0 in
  let l = ref l0 in
  let tl = ref (Array.map (fun a -> (0, 0)) graph.Graph.nodes) in
  let nbIter = ref 0 in

  let alpha_r =0.8 and alpha_s=0.8 and p0 = 0.75 in
  
  while !nbIter < 1000 do
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
    l := l0
  done;
  !cbest;;

let ()=
  let demo = Graph.generate_random_graph 50 0.5 10. in
  let c = bls demo 100. 1 10 in
  Draw.draw demo c;;
