try 
	Unix.mkdir ".remodel" 0o700 
with Unix.Unix_error(_,_,_) -> ();;
(*
 Remodel: Make Done Right
 Due Date: December 12 2012

 For this individual project, you will write a replacement program for "make" called "remodel." Make takes a series of dependencies and builds targets by executing commands. Unfortunately, its use of UNIX timestamps as a happens-before relation means that it can fail to build properly. This is especially true during parallel builds.

 Your program, "remodel", will also take a series of dependencies and targets, etc. (grammar below), but unlike make, it will use MD5 hashes to detect new content and provide, together with the dependency chain, a happens-before relation (md5diff(oldA,newA) => everything that depends on A must be rebuilt). It must also execute all independent (non-conflicting) commands in parallel, using threads or processes.

 remodel uses a different grammar than make. Dependencies can appear in any order. If you execute "remodel" with no arguments, it should start with the pseudo-target "DEFAULT". Otherwise, the root is the argument to "remodel", as in "remodel foo.o".
*)
type command = Command of string;;
type filename = Filename of string;;
type target = Target of filename list;;
type dependency = Dependency of filename list;;
type production = Production of target * dependency * command;;
type program = Program of production list;;

(* will need to write a parser to produce this. for now write these files like so - i.e. put off parsing until the end *)
(*
 Here's an example that builds the program "baz" from two source files, "foo.cpp" and "bar.cpp".

 DEFAULT <- baz
 baz <- foo.o, bar.o: "g++ foo.o bar.o -o baz"
 foo.o <- foo.cpp : "g++ -c foo.cpp -o foo.o"
 bar.o <- bar.cpp: "g++ -c bar.cpp -o bar.o"
*)
let example1 = Program([Production(Target([Filename("DEFAULT")]), Dependency([Filename("baz")]), Command("")) ;
						Production(Target([Filename("baz")]), Dependency([Filename("foo.o") ; Filename("bar.o")]), Command("g++ foo.o bar.o -o baz")) ;
						Production(Target([Filename("foo.o")]), Dependency([Filename("foo.cpp")]), Command("g++ -c foo.cpp -o foo.o")) ;
						Production(Target([Filename("bar.o")]), Dependency([Filename("bar.cpp")]), Command("g++ -c bar.cpp -o bar.o"))
						]);;
(*
 You should store the dependencies on disk in a special directory called ".remodel/", so that "remodel" will not re-execute any commands unless a dependency has been violated.
*)
(* The following implementation for executing a system command and returning the values from stdout is from rosettacode.org. *)

exception MoreThanOneTargetException of string;;
exception NodeCreationException of string;;
type md5 = MD5 of string;;
type node = Empty | Node of filename * md5 * command * node list;;
type graph = Uninstantiated | Graph of md5 * node list;;

let get_md5 (Filename(f)) = MD5(Digest.to_hex (Digest.file f))

let make_dependency_graph (prod_list : production list) =
	let targets = List.flatten (List.map (fun (Production(Target(file_list),_,_)) -> file_list) prod_list)
in  let rec unique_files = function
	| (d, []) -> (d, [])
	| (d, h::t) -> unique_files (h::d, (List.filter (fun f -> f <> h) t))
in let rec all_files = function
	| [] -> []
	| Production(Target(l1),Dependency(l2),_)::t -> l1 @ l2 @ all_files(t)
in let find_target (f : filename) = 
	match List.filter (fun ff -> ff = f) targets with 
	| [] -> None (* files that are dependencies but not targets will be picked up *)
	| h::[] -> Some(List.hd targets) 
	| h::t -> let Filename(fname) = f in
		raise (MoreThanOneTargetException fname)


exception MultipleInitialTargetException of string;;

let filename_string (Filename(fname)) = fname;;

let args = Sys.argv 
in let target = match Array.length args with
	| 1 -> Filename("DEFAULT") 
	| 2 -> Filename(args.(1))
	| _ -> 	raise (MultipleInitialTargetException "Multiple initial target values are not currently supported")
in let make_md5s line =
	let split_string s = 
		let space_index = String.index s ' ' in
		let first_half = String.sub s 0 space_index
		and second_half = String.sub s (space_index + 1) (String.length s - space_index - 1) in
		(first_half, second_half)
	let helper s = match String.index 

	(* first thing is the target name, so lob this off *)
in let get_old_dependencies (Filename(f)) =
	try 
		let open_file = open_in ".remodel/history" 
		and line = ref("")
		and switch = ref(true) in
		(try 
			while !switch
			do
				line := (input_line open_file);
				if String.sub !line 0 (String.index !line ' ') = f 
				then switch := false
				else ()
			done;
		with End_of_file -> line := "");
		close_in open_file; make_md5s !line
	with Sys_error(msg) -> (Printf.printf "%s" ("Warning : "^msg^"\n"	)); ""
in Printf.printf "%s" (get_old_dependencies target);;			
