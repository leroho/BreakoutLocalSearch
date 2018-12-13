
type node = {
	id : int ;
	weight : float ;
}

module SS = Set.Make(
	struct
    let compare = Pervasives.compare
    type t = node
  end		
)

type graph = {
	nodes : node array ;
	edges :  SS.t array 
}


(* une liste des voisins de u , pour avoir les poids parcourir la liste 'nodes' *)
let get_voisins = fun graph u ->
	SS.elements graph.edges.(u.id - 1)

 
let get_voisins_id = fun graph u_id ->
	SS.elements graph.edges.(u_id - 1)

let get_node_id = fun graph u_id ->
	graph.nodes.(u_id - 1 ) 

(* creation d'un graphe de n sommets de poids 1 *)
let create_graph = fun n ->
	{nodes = Array.init n (fun i -> {id = i + 1; weight = 1.}) ; edges = Array.make n SS.empty}


(* change le poids de u en w *)
let change_weight = fun graph u w ->
	Array.set graph.nodes (u.id - 1) {id = u.id; weight = w}

let change_weight_id = fun graph u_id w ->
	Array.set graph.nodes (u_id - 1) {id = u_id; weight = w}

let get_weight = fun u ->
	u.weight

let get_weight_id = fun graph u_id ->
	graph.nodes.(u_id - 1).weight

(* ajoute un arete entre u et v *)
let add_edge = fun graph u v ->
	Array.set graph.edges (u.id - 1) (SS.add v (graph.edges.(u.id - 1) ) ) ;
	Array.set graph.edges (v.id - 1) (SS.add u (graph.edges.(v.id - 1) ) )

let add_edge_id = fun graph u_id v_id ->
	Array.set graph.edges (u_id - 1) (SS.add (graph.nodes.(v_id - 1) ) (graph.edges.(u_id - 1) ) );
	Array.set graph.edges (v_id - 1) (SS.add (graph.nodes.(u_id - 1) ) (graph.edges.(v_id - 1) ) )


(* true si u est voisin de v, false sinon *)
let is_voisin = fun graph u v ->
	SS.exists (fun x -> u = x) (graph.edges.(v.id - 1))
	
let is_voisin_id = fun graph u_id v_id ->
	SS.exists (fun x -> u_id = x.id) (graph.edges.(v_id - 1))

(* retourne un graphe avec un sommet d'un poids w ajoute *)
let add_node = fun graph w ->
	{	nodes = Array.append graph.nodes [|{id = Array.length graph.nodes ; weight = w}|] ;
    	edges = Array.append graph.edges [|SS.empty|] }


(* un exemple de graph *)
let graph_demo = fun () -> 
	let g = create_graph 5 in
	add_edge_id g 1 2 ;
	add_edge_id g 1 4 ;
	add_edge_id g 1 5 ;
	add_edge_id g 2 3 ;
	add_edge_id g 2 4 ;
	add_edge_id g 2 5 ;
	add_edge_id g 3 4 ;
	add_edge_id g 4 5 ;
	g


(* fonctions utilisees dans generate_random_graph *)


let generate_voisins_all = fun graph densite ->
	let n = (Array.length graph.nodes) in
	let rec gen = fun graph i ->
		if i = n+1 then graph
		else (
			let rec gen2 = fun j ->
				if j = n+1 then ()
				else (let r = Random.float 1. in
					if densite > r then add_edge_id graph i j; gen2 (j+1) )
			in (gen2 (i+1) ); gen graph (i+1) )
	in ( gen graph 1 )
	
				


(* generer un graphe aleatoire non pondere de n sommets et de densite  0 <= d <= 1 *)

let generate_random_graph_unweighted = fun n densite ->
	generate_voisins_all (create_graph n) densite
	
(* generer un graphe aleatoire pondere de n sommets et un poids maximal de max_w *)

let generate_random_graph = fun n densite max_w ->
	let g_bis = create_graph n in
	let rec gen_w = fun i ->
		if i = (n + 1) then g
		else let r = Random.float max_w in
		change_weight_id g i r ; gen_w (i+1)
	in let g = generate_voisins_all g_bis densite in
	gen_w 1
		



(* Conversion DIMACS vers graph *)


let create_graph_DIMACS = fun filename ->
	let ic = open_in filename in
	let comment_flag = ref true in
	let num_edges = ref 0 in
	let num_nodes = ref 0 in
		while !comment_flag do
			let line = input_line ic in
			let line_type = String.get line 0 in
			if line_type = 'p' then 
				(let l = Str.split (Str.regexp " ") line in
				num_edges := int_of_string (List.nth l 3);
				num_nodes := int_of_string (List.nth l 2);
				comment_flag := false )
		done;
		let graph = create_graph !num_nodes in
		for i = 1 to !num_edges do
			let line = input_line ic in
			let l = Str.split (Str.regexp " ") line in
			add_edge_id graph (int_of_string (List.nth l 1) ) (int_of_string (List.nth l 2) )
		done;
		graph
	








