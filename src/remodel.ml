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

type md5 = MD5 of string;;
type node = Empty | Node of filename * md5 * command * filename list;;
type graph = Uninstantiated | Graph of md5 * node list;;

exception MoreThanOneTargetException of string;;
exception NodeCreationException of string;;

let make_md5 (Filename(f)) = if f = "DEFAULT" then MD5("") else MD5(Digest.to_hex (Digest.file f));;
let get_md5 (MD5(s)) = s;;

let make_serial_commands (Program(prod_list)) (target : filename) =
	let targets = List.flatten (List.map (fun (Production(Target(file_list),_,_)) -> file_list) prod_list)
in	let rec make_node_list = function
 	 | [] -> []	
 	 | Production(Target([]), _, _)::t -> make_node_list t
 	 | Production(Target(h::t1), Dependency(dep_list), cmd)::t2 -> 
		Node(h, make_md5 h, cmd, dep_list)::make_node_list(Production(Target(t1), Dependency(dep_list), cmd)::t2)
in  let is_valid_target (f : filename) = 
	match List.filter (fun ff -> ff = f) targets with 
	 | [] -> false
	 | h::[] -> true
	 | h::t -> let Filename(fname) = f in raise (MoreThanOneTargetException fname)
in 	let rec get_from_node_list (f : filename) (n : node list)	=
	match n with
	 | [] -> Empty
	 | (Node(ff,_,_,_) as h)::t when ff = f -> h
	 | _::t -> get_from_node_list f t
in  let rec get_commands_for_target (f : filename) (n : node list) =
	match get_from_node_list f n with
	| Empty -> []
	| Node(_,_,cmd,[]) -> [cmd] 
	| Node(_,_,cmd,dep_list) -> (List.flatten (List.map (fun a -> get_commands_for_target a n) dep_list)) @ [cmd]
in get_commands_for_target target (make_node_list prod_list);;

(* let exec_serial_commands (cmds : command list) = *)

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


exception MultipleInitialTargetException of string;;


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
	| _ -> 	raise (MultipleInitialTargetException "Multiple initial target values are not currently supported")
in true;

 