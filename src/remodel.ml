(* Future Improvements? *)
(* 1. When handling multiple dependencies/targets, always sort - i.e. don't just use them in the same order as the REMODELFILE? *)

try 
	Unix.mkdir ".remodel" 0o755 ;
	Unix.close (Unix.openfile ".remodel/history" [Unix.O_CREAT] 0o755) ; 
with Unix.Unix_error(_,_,_) -> ();;

type command = Command of string;;
type filename = Filename of string;;
type target = Target of filename list;;
type dependency = Dependency of filename list;;
type production = Production of target * dependency * command;;
type program = Program of production list;;

type md5 = MDNone | MD5 of string;;
type node = Empty | Node of filename * md5 * command * filename list;;
type graph = Uninstantiated | Graph of md5 * node list;;

exception TargetException of string;;
exception NodeException of string;;
exception CommandException of string;;
exception DataIntegrityException of string;;

let make_md5 (Filename(f)) = 
	if f = "DEFAULT" then MDNone
	else if Sys.file_exists f then MD5(Digest.to_hex (Digest.file f))
	else MDNone;;
let get_md5 = function
 | MD5(s) -> s
 | MDNone -> ""

let old_dependencies =
	let rec split_string s = 
		try
			let space_index = String.index s ' ' in
			let first_half = String.sub s 0 space_index
			and second_half = String.sub s (space_index + 1) (String.length s - space_index - 1) in
			first_half::(split_string second_half)
		with Not_found -> [s] 
in	let open_file = open_in ".remodel/history" in
	let rec read_old_deps () =
		try
			let line = split_string (input_line open_file) in
			match line with
			| [] -> []
			| f::md5s -> (Filename(f), List.map (fun s -> if f = "DEFAULT" then MDNone else MD5(s)) md5s)::(read_old_deps ())
		with End_of_file -> [] in
	let dep_hash_alist = read_old_deps ()
	in close_in open_file; dep_hash_alist ;;

let make_serial_commands (Program(prod_list)) (target : filename) =
	let targets = List.flatten (List.map (fun (Production(Target(file_list),_,_)) -> file_list) prod_list) in
	let rec make_node_list = function
		| [] -> []	
 	 	| Production(Target([]), _, _)::t -> make_node_list t
 	 	| Production(Target(h::t1), Dependency(dep_list), cmd)::t2 -> 
				Node(h, make_md5 h, cmd, dep_list)::make_node_list(Production(Target(t1), Dependency(dep_list), cmd)::t2) in
	let is_valid_target (f : filename) = 
		match List.filter (fun ff -> ff = f) targets with 
	 	| [] -> false
	 	| h::[] -> true
	 	| h::t -> let Filename(fname) = f in raise (TargetException ("More than one target of "^fname)) in
	let rec get_from_node_list (f : filename) (n : node list)	=
		match n with
	 	| [] -> Empty
	 	| (Node(ff,_,_,_) as h)::t when ff = f -> h
	 	| _::t -> get_from_node_list f t in
	let rec get_commands_for_target (f : filename) (n : node list) =
		match get_from_node_list f n with
		| Empty -> []
		| Node(_,_,cmd,[]) as nn -> [nn] 
		| Node(_,_,cmd,dep_list) as nn -> (List.flatten (List.map (fun a -> get_commands_for_target a n) dep_list)) @ [nn] in
	let rec get_old_hashes (f : filename) (hashes : (filename * md5 list) list) = 
		match hashes with
		| [] -> raise (DataIntegrityException "Hash not found")
		| (ff, md5s)::t when ff = f -> md5s
		| h::t -> get_old_hashes f t
in 	assert (is_valid_target target) ; get_commands_for_target target (make_node_list prod_list);;



let exec f m (Command(cmd)) = 
	Sys.command cmd;;

let exec_serial_commands (node_list : node list)= 
	let rec helper = function
 	 | [] | Empty::_ -> []
 	 | Node(Filename("DEFAULT"), _, Command(""), _)::t -> helper t
 	 | Node(Filename(fname) as f, m, c, _)::t -> 
			if exec f m c <> 0
 			then raise (CommandException "Error executing command : Nonzero exit code")
 			else assert (Sys.file_exists fname) ; helper t
 	in let _ = helper node_list in ();;

let record_dependencies (Program(prod_list)) = 
	let rec helper = function
	 | [] -> ""
	 | Production(Target([]), _, _)::t -> helper(t)
	 | Production(Target((Filename(f) as h)::t1), Dependency(l), cmd)::t2 -> 
		let concat_hashes a b = a^" "^b
	in 	let make_md5s_for_deps = List.map (fun f -> get_md5 (make_md5 f)) l
	in 	let prefix = if f = "DEFAULT" then f else f^" "^(get_md5 (make_md5 h))
	in 	(List.fold_left concat_hashes prefix make_md5s_for_deps)^"\n"^helper(Production(Target(t1), Dependency(l), cmd)::t2)
in 	let deps = helper prod_list 
and history = open_out ".remodel/history"
in 	output_string history deps; close_out history;; 

(* let test_suite (arg : unit) = *)
	let example1 = Program([Production(Target([Filename("DEFAULT")]), Dependency([Filename("baz")]), Command("")) ;
							Production(Target([Filename("baz")]), Dependency([Filename("foo.o") ; Filename("bar.o")]), Command("g++ foo.o bar.o -o baz")) ;
							Production(Target([Filename("foo.o")]), Dependency([Filename("foo.cpp")]), Command("g++ -c foo.cpp -o foo.o")) ;
							Production(Target([Filename("bar.o")]), Dependency([Filename("bar.cpp")]), Command("g++ -c bar.cpp -o bar.o"))
							]) ;;
(* in record_dependencies example1 *)
(* 
	let Program(prod_list) = example1
	in let nodes = make_node_list prod_list
	in get_commands_for_target (Filename("DEFAULT")) nodes ;;
*)


(* program entry *)
let args = Sys.argv 
in let target = match Array.length args with
	| 1 -> Filename("DEFAULT") 
	| 2 -> Filename(args.(1))
	| _ -> 	raise (TargetException "Multiple initial target values are not currently supported")
in exec_serial_commands (make_serial_commands example1 target) ; record_dependencies example1 ;;

 
