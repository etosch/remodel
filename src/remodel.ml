(* Future Improvements? *)
(* 1. When handling multiple dependencies/targets, always sort - i.e. don't just use them in the same order as the REMODELFILE? *)
(* 2. Check that the hashes of the dependencies match the hashes of their targets -> i.e. make sure that the build didn't get screwed up *)
(* 3. Allow multiple initial targets and execute in parallel *)
if not (Sys.file_exists ".remodel")
then Unix.mkdir ".remodel" 0o755
else ();;
if not (Sys.file_exists ".remodel/history") 
then let o = open_out ".remodel/history" in output_string o "" ; close_out o else () ;;
(* let o = open_out ".remodel/logdone" in output_string o "" ; close_out o;; *)
Sys.command "echo \"\" > .remodel/logdone";;

type command = Command of string;;
type filename = Filename of string;;
type target = Target of filename list;;
type dependency = Dependency of filename list;;
type production = Production of target * dependency * command;;
type program = Program of production list;;

type md5 = MDNone | MD5 of string;;
type node = Empty | Node of filename * md5 * command * (filename * md5) list;;
type graph = Uninstantiated | Graph of md5 * node list;;

type direction = Front | Back ;;

exception TargetException of string;;
exception NodeException of string;;
exception CommandException of string;;
exception DataIntegrityException of string;;
exception ParseException of string;;
exception SynchError of string;;

let is_whitespace = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false ;;

let contains_whitespace s = 
  let f = String.contains s in
    f ' ' || f 'n' || f '\r' || f '\t' ;;

let rec trim s = 
  if s = "" then s else
  let trim_one s d = match d with
    | Front -> String.sub s 1 (String.length s - 1) 
    | Back -> String.sub s 0 (String.length s - 1) in
  match (is_whitespace s.[0], is_whitespace s.[String.length s - 1]) with
  | (true, true) -> trim (trim_one (trim_one s Front) Back)
  | (true, false) -> trim (trim_one s Front)
  | (false, true) -> trim (trim_one s Back)
  | (false, false) -> s ;;

let rec string_split target delim = 
  let get_chunk str = 
    try
      let i = String.index str delim in
      (String.sub str 0 i, i+1) 
    with Not_found -> (str, -1) in
  match get_chunk target with
  | ("", _) -> [] 
  | (chunk, -1) -> [chunk] 
  | (chunk, i) -> chunk :: (string_split (String.sub target i ((String.length target) - i)) delim) ;;

let string_partition_at_index target i =
  if String.length target > i 
  then (String.sub target 0 i, String.sub target i (String.length target - i))
  else (target, "")


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
    with Not_found -> [s] in
  let open_file = open_in ".remodel/history" in
  let rec read_old_deps () =
    try
      let line = split_string (input_line open_file) in
      match line with
      | [] -> []
      | f::md5s -> (Filename(f), List.map (fun s -> if f = "DEFAULT" then MDNone else MD5(s)) md5s)::(read_old_deps ())
    with End_of_file -> [] in
  let dep_hash_alist = read_old_deps ()
  in close_in open_file; dep_hash_alist ;;

let exec_parallel_commands (Program(prod_list)) (target : filename) =
  let targets = List.flatten (List.map (fun (Production(Target(file_list),_,_)) -> file_list) prod_list) in
  let rec get_old_hashes (f : filename) (hashes : (filename * md5 list) list) = 
    match hashes with
    | [] -> [MDNone]
    | (ff, md5s)::t when ff = f -> md5s
    | h::t -> get_old_hashes f t in
  let rec make_node_list = function
    | [] -> []  
    | Production(Target([]), _, _)::t -> make_node_list t
    | Production(Target(h::t1), Dependency(dep_list), cmd)::t2 -> 
        let old_hashes = get_old_hashes h old_dependencies in
        let (this_hash, dep_hashes) = if List.length old_hashes <> List.length dep_list + 1
                                      then (MDNone, List.map (fun _ -> MDNone) dep_list)
                                      else (List.hd old_hashes, List.tl old_hashes) in
        Node(h, this_hash, cmd, List.map2 (fun a b -> (a, b)) dep_list dep_hashes)::make_node_list(Production(Target(t1), Dependency(dep_list), cmd)::t2) in
  let is_valid_target (f : filename) = 
    match List.filter (fun ff -> ff = f) targets with 
    | [] -> false
    | h::[] -> true
    | h::t -> let Filename(fname) = f in raise (TargetException ("More than one target of "^fname)) in
  let rec get_from_node_list (f : filename) (n : node list) =
    match n with
    | [] -> Empty
    | (Node(ff,_,_,_) as h)::t when ff = f -> h
    | _::t -> get_from_node_list f t in
  let already_executed (Filename(fname)) =
    let b = ref(false)
    and l = open_in ".remodel/logdone" in
    while (not !b) do
      try b := trim (input_line l) = fname; with End_of_file -> ();
    done; !b in
  let log_done (f : filename) (Command(cmd)) = 
    if already_executed f 
    then raise (SynchError ("Already executed "^cmd)) (* this isn't atomic, so who knows if it works? *)
    else 
      let o = open_out_gen [Open_append] 0o755 ".remodel/logdone" 
      and Filename(fname) = f in 
      output_string o (fname^"\n") ; close_out o in
  let exec (n : node) pid = 
    let run_cmd (nn : node) =
      match nn with
      | Empty -> raise (NodeException "Empty node passed to run_cmd; this is VERY BAD")
      | Node(f,m,c,_) ->
        let Filename(fname) = f and Command(cmd) = c in
        if already_executed f then ()
        else (* wait until the last possible minute to check if it's already executed *)
          Printf.printf "target:%s\tmd5_old:%s\tmd5_new:%s\tcmd:%s\tpid:%d\n" fname (get_md5 m) (get_md5 (make_md5 f)) cmd pid ; 
          ignore (Sys.command cmd) ; log_done f c in
    match n with
    | Empty -> () (* something went horribly awry? *)
    | Node(f, m, c, []) -> if make_md5 f <> m then run_cmd n else ()
    | Node(f,m,c,dep_hash_alist) -> 
      if (List.for_all (fun (fname, hash) -> make_md5 fname = hash) ((f, m)::dep_hash_alist)) then () else run_cmd n in
  let rec exec_commands (f : filename) (n : node list) =
    match get_from_node_list f n with
    | Empty -> []
    | Node(_,_,_,[]) as nn -> ignore (exec nn (Unix.getpid ())) ; [nn] 
    | Node(f,_,c,dep_hash_alist) as nn -> 
      flush stdout ;
      match Unix.fork() with
      | 0 -> (List.flatten (List.map (fun (a,_) -> exec_commands a n) dep_hash_alist)) @ [nn] (* waiting for dependencies to return *)
      | pid -> ignore (Unix.waitpid [] pid) ; ignore (exec nn (Unix.getpid ())) ; log_done f c; []
  in assert (is_valid_target target) ; exec_commands target (make_node_list prod_list) ;; 

let record_dependencies (Program(prod_list)) = 
  let rec helper = function
   | [] -> ""
   | Production(Target([]), _, _)::t -> helper(t)
   | Production(Target((Filename(f) as h)::t1), Dependency(l), cmd)::t2 -> 
    let concat_hashes a b = a^" "^b in
    let make_md5s_for_deps = List.map (fun f -> get_md5 (make_md5 f)) l in
    let prefix = if f = "DEFAULT" then f else f^" "^(get_md5 (make_md5 h)) in
    (List.fold_left concat_hashes prefix make_md5s_for_deps)^"\n"^helper(Production(Target(t1), Dependency(l), cmd)::t2) in
  let deps = helper prod_list 
  and history = open_out ".remodel/history" in
  output_string history deps; close_out history;; 

let example1 = Program([Production(Target([Filename("DEFAULT")]), Dependency([Filename("baz")]), Command("")) ;
              Production(Target([Filename("baz")]), Dependency([Filename("foo.o") ; Filename("bar.o")]), Command("g++ foo.o bar.o -o baz")) ;
              Production(Target([Filename("foo.o")]), Dependency([Filename("foo.cpp")]), Command("g++ -c foo.cpp -o foo.o")) ;
              Production(Target([Filename("bar.o")]), Dependency([Filename("bar.cpp")]), Command("g++ -c bar.cpp -o bar.o")) ;
              Production(Target([Filename("existence")]), Dependency([Filename("existence.ml") ; Filename("a.txt")]), Command("ocamlc -o existence existence.ml ; ./existence")) ;
              Production(Target([Filename("a.txt") ; Filename("b.txt")]), Dependency([Filename("do.sh")]), Command("chmod +x do.sh ; ./do.sh")) ;
              Production(Target([Filename("do.sh")]), Dependency([]), Command("echo \"touch a.txt; touch b.txt\" > do.sh"))
              ]) ;;

(* type lexeme = Lint of int | Lident of string;;
type token = FileTok of string | CmdTok of string ;;
let remodelfile_lexer l = Genlex.make_lexer ["<-" ; ":" ; ","] (Stream.of_string l) ;; *)
(* naive approach for now *)

(*
let parse_remodelfile () =
  let buf = Buffer.create 80 
  and prod_list = ref([])
  and f = open_in "REMODELFILE" in
  (try
    let rec read_for_arrow i =
      while not (String.contains_from (Buffer.contents buf) i '-')
      do Buffer.add_string buf (input_line f) done;
      if (String.contains_from (Buffer.contents buf) i '<') && (String.index_from (Buffer.contents buf) i '-') - (String.index_from (Buffer.contents buf) i '<') = 1
      then (String.index_from (Buffer.contents buf) i '<')
      else read_for_arrow (i) in
    let read_for_colon () =
      (* even if there's a colon in the path, we MUST use the colon before the command, so it's safe to read for the first one *)
      while not (String.contains (Buffer.contents buf) ':')
      do Buffer.add_string buf (input_line f) done;
      String.index (Buffer.contents buf) ':' in
    let get_next_target buf =
      (* get the substring of things before the <-. then split on commas. then check each substr for spaces. return the target of filenames and update the buffer *)
      let (target_string, new_buffer_contents) = string_partition_at_index (Buffer.contents buf) (read_for_arrow 0) in
      let targets = List.map trim (string_split target_string ',') in
      (* ignore (Printf.printf "%d\n" (Unix.getpid())) ; *)
      assert (List.for_all (fun s -> not (contains_whitespace s)) targets) ; 
      Buffer.clear buf ; Buffer.add_string buf new_buffer_contents ;
      Target (List.map (fun f -> Filename(f)) targets) 
    and get_next_deps buf =
      (* Since I've just called next target, the first - will be from a <-. Read until a command. per the grammar, there must be deps between <- and : or another <- *)
      let start_index = String.index (Buffer.contents buf) '-' in (*
      and colon_index = read_for_colon () in
      let next_dep_index = try read_for_arrow start_index with End_of_file -> colon_index + 1 in
      Printf.printf "%d\n%d\n%d" start_index colon_index next_dep_index;
      if colon_index < next_dep_index
      then 
        let (temp_dep_string, new_buffer_contents) = string_partition_at_index (Buffer.contents buf) colon_index in
        let deps = List.map trim (string_split temp_dep_string ',') in
        ignore (List.map (fun t -> Printf.printf "%sx\n" t) deps) ;
        assert (List.for_all (fun s -> not (contains_whitespace s)) deps) ;
        Buffer.clear buf ; Buffer.add_string buf new_buffer_contents ;
        Dependency (List.map (fun f -> Filename(f)) deps)
      else
        let (temp_dep_string, _) = string_partition_at_index (Buffer.contents buf) next_dep_index in
        let rec take_until lst p = match lst with | [] -> [] | h::t when p h -> [List.hd (string_split h ' ')] | h::t -> h::(take_until t p) in
        let deps = take_until (List.map trim (string_split temp_dep_string ',')) contains_whitespace in
        let new_buffer_contents = String.sub (Buffer.contents buf) 0 (List.fold_left (+) 0 (List.map String.length deps)) in
        ignore (List.map (fun t -> Printf.printf "%sx\n" t) deps) ;
        assert (List.for_all (fun s -> not (contains_whitespace s)) deps) ;
        Buffer.clear buf ; Buffer.add_string buf new_buffer_contents ;
        Dependency (List.map (fun f -> Filename(f)) deps)
*)      Dependency([]) 
    and get_next_cmd buf = Command("") in
(*    while true do *)
      let target = get_next_target buf in
      let deps = get_next_deps buf in
      let cmd = get_next_cmd buf in
      prod_list := Production(target, deps, cmd)::(!prod_list)
(*    done;  *)
  with
  | End_of_file -> if Buffer.contents buf = "" then () else raise (ParseException "Incomplete production."));
  close_in f ; Program(!prod_list);;
*)
(* let program = parse_remodelfile () ;;  *)

(* program entry *)
let args = Sys.argv in 
  let target = match Array.length args with
    | 1 -> Filename("DEFAULT") 
    | 2 -> Filename(args.(1))
    | _ ->  raise (TargetException "Multiple initial target values are not currently supported") in 
  ignore(exec_parallel_commands example1 target) ; 
  record_dependencies example1 ;;

 
