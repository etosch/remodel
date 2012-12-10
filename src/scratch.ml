type 'a bin_tree = Empty | Node of 'a bin_tree * 'a * 'a bin_tree;;

let myTree = Node(Empty, 0, Empty);;

(* The following does not compile; I get a type error, even with annotations -> was applying the function twoce!
let rec in_order (t : 'a bin_tree) = function
  Empty -> ([] : 'a list) 
| Node((lb : 'a bin_tree), (v : 'a), (rb : 'a bin_tree)) -> (in_order lb) @ [v] @ (in_order rb);; *)

let rec list_of_tree = function
	Empty -> [] |
	Node(lb, v, rb) -> (in_order lb) @ (v :: (in_order rb));;

let rec insert x = function 
	Empty -> Node(Empty, x, Empty) |
	Node(lb, v, rb) -> if (x < v) then Node(insert x lb, v, rb) else Node(lb, v, insert x rb);;

let rec tree_of_list = function
	[] -> Empty |
	h :: t -> insert h (tree_of_list t);;

let sort thing = list_of_tree tree_of_list thing;;


let syscall (cmd : string) : string =
	let ic, oc = Unix.open_process cmd in
	let buf = Buffer.create 16 in
	(try 
		while true do
			Buffer.add_channel buf ic 1
		done
	with End_of_file -> ());
	let _ = Unix.close_process (ic, oc) in
	(Buffer.contents buf);;

let here = Unix.getcwd ();;


(* Need to transform a parsed example into a dependency graph *)
(* A graph node has associated with it the file name, the current md5 sum, and the md5 sum stored in .remodel/dependencies.ml *)
type md5 = MD5 of string;;
type node = Node of filename * md5 * dependency;;
type graph = Graph of node list;;


in  let create_node (f : filename) = 
	match find_target(f) with
	| None -> let Filename(fname) = f in
		raise (NodeCreationException fname)
	| Some(Production(_, Dependency(filename_list), cmd)) = Node(f, get_md5 f, cmd, filename_list)
and let insert_node (n : Node) (g : Graph) =
		let (Node(_,_,_,filename_list), Graph(node_list)) = (n , g) in
		match filename_list with
		| [] -> Graph(n::node_list)


(*	let helper s = match String.index *)
let old_dependencies =
	let rec split_string s = 
		try
			let space_index = String.index s ' ' in
			let first_half = String.sub s 0 space_index
			and second_half = String.sub s (space_index + 1) (String.length s - space_index - 1) in
			first_half::(split_string second_half)
		with Not_found -> [s] 
in	let read_old_deps =
		let open_file = open_in ".remodel/history" in
		try
			let line = split_string (input_line open_file) in
			let f::mds5 = line in

				if String.sub !line 0 (String.index !line ' ') = f 
				then switch := false
				else ()
			done;
		with End_of_file -> line := "");
		close_in open_file; "" (* do something with !line *)
	try 
		
	with Sys_error(msg) -> (Printf.printf "%s" ("Warning : "^msg^"\n")); ""







in let rec find_node_by_filename (f : filename) (n : node_list) =
	if List.length n = 0
	then Independent(f)
	else
		let candidates = List.filter (fun nn -> match nn with Independent(ff) | Dependent(ff,_,_,_) -> ff = f) n in
		match List.length candidates with
		| 0 -> find_node_by_filename f (List.fold_left List.concat [] (List.map (fun nn -> match nn with Independent(_) -> [] | Dependent(_,_,_,l) -> l) n))
		| 1 -> List.hd candidates
		| _ -> let Filename(fname) = f in raise (MoreThanOneTargetException "There is more than one node of "^f)
in let add_node (f : filename) (dep_list : dependency) (cmd : command) (g : Graph) = 
	let Graph(_, n) = g 
	in 
		let dependency_node_list = List.map (fun f -> find_node_by_filename f n) dep_list
		and 




in  let rec unique_files = function
	| (d, []) -> (d, [])
	| (d, h::t) -> unique_files (h::d, (List.filter (fun f -> f <> h) t))
in let rec all_files = function
	| [] -> []
	| Production(Target(l1),Dependency(l2),_)::t -> l1 @ l2 @ all_files(t)


