let () =
	let n = 50 in
  for j = 0 to 4 do
    let filename = String.concat "" ["stats";(string_of_int j);".txt"] in
    let dimacs_file = String.concat "" ["data/dimacs_test";(string_of_int j);".txt"] in
    let fic = open_out filename in
    let demo = Graph.create_graph_DIMACS dimacs_file in
    let avg = ref 0. in
    let max_val = ref 0 in
    let min_val = ref max_int in
    let sigma = ref 0. in
    let fbest_array = Array.make 100 0 in
    
    for i = 0 to (n-1) do
      let (fbest, c) = Main.bls demo 1000. in
      avg := !avg +. (float_of_int fbest);
      Array.set fbest_array i fbest;
      min_val := min fbest !min_val;
      max_val := max fbest !max_val;
      Printf.fprintf fic "%d %d\n" i fbest;
      if Main.isClique demo c then Printf.fprintf fic "OK\n" else Printf.fprintf fic "NOT OK\n"
    done;
    avg := !avg /. (float_of_int n);
    Printf.fprintf fic "\navg = %f" !avg;
    Array.iter (fun elt -> sigma := !sigma +. ((float_of_int elt) -. !avg)**2.) fbest_array;
    sigma := sqrt (!sigma /. (float_of_int n));
    Printf.fprintf fic ",    sigma = %f" !sigma;
    Printf.fprintf fic ",    min = %d" !min_val;
    Printf.fprintf fic ",    max = %d" !max_val;
    close_out fic
  done;;
