(* Future Improvements? *)
(* 1. When handling multiple dependencies/targets, always sort - i.e. don't just use them in the same order as the REMODELFILE? *)
(* 2. Check that the hashes of the dependencies match the hashes of their targets -> i.e. make sure that the build didn't get screwed up *)
(* 3. Allow multiple initial targets and execute in parallel *)
(* 4. Would like to support double quotes *)
open Remodel_types
let _LOGDONE = ".remodel/logdone/" 
and _HISTORY = ".remodel/history"
and _REMODELFILE = "REMODELFILE" ;;

type md5 = MDNone | MD5 of string;;
type node = Empty | Node of filename * md5 * command * (filename * md5) list;;
type graph = Uninstantiated | Graph of md5 * node list;;

exception TargetException of string;;
exception NodeException of string;;
exception CommandException of string;;
exception DataIntegrityException of string;;
exception ParseException of string;;
exception SynchError of string;;

let old_dependencies = ref([Filename(""), []])

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

let make_md5 (Filename(f)) = 
  if f = "DEFAULT" then MDNone
  else if Sys.file_exists f then MD5(Digest.to_hex (Digest.file f))
  else MDNone;;

let get_md5 = function
 | MD5(s) -> s
 | MDNone -> ""

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
        let old_hashes = get_old_hashes h !old_dependencies in
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
  let already_executed (Filename(fname)) (Command(cmd)) =
    let recorded = Sys.readdir _LOGDONE in
    match List.filter (fun s -> fname = List.hd (string_split s '_' )) (Array.to_list recorded) with
    | [] -> false
    | cmds -> List.exists (fun f -> try
                                      let ff = open_in (_LOGDONE^f) in
                                      let retval = (input_line ff = cmd) 
                                      in close_in ff; retval
                                    with Sys_error(msg) -> raise (DataIntegrityException ("already_executed\t"^msg))
                                    | End_of_file -> false)
              cmds in
  let log_done (f : filename) (c : command) = 
    let Command(cmd) = c in 
    if already_executed f c
    then raise (SynchError ("Command "^cmd^" already executed"))
    else try
      let Filename(fname) = f in 
      let o = open_out_gen [Open_append;Open_creat] 0o755 (".remodel/logdone/"^fname^"_"^(string_of_int (Unix.getpid () ) )) in
      output_string o cmd ; close_out o 
    with Sys_error(msg) -> raise (DataIntegrityException "Some problem in log_done") in
  let exec (n : node) pid = 
    let run_cmd (nn : node) =
      match nn with
      | Empty -> raise (NodeException "Empty node passed to run_cmd; this is VERY BAD")
      | Node(f,m,c,_) ->
        try
          let Filename(fname) = f and Command(cmd) = c in
          (* if already_executed f then () else *)
          (* wait until the last possible minute to check if it's already executed *)
          (* Printf.printf "target:%s\tmd5_old:%s\tmd5_new:%s\tcmd:%s\tpid:%d\n" fname (get_md5 m) (get_md5 (make_md5 f)) cmd pid ; *)
            match (already_executed f c) with true -> () | false -> ignore (Sys.command cmd) ; log_done f c 
        with Sys_error(msg) -> raise (DataIntegrityException ("Some problem in exec: "^msg)) in
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
      | pid -> ignore (Unix.waitpid [] pid) ; ignore (exec nn (Unix.getpid ())) ; []  in
  assert (is_valid_target target) ; exec_commands target (make_node_list prod_list) ;;

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
  and history = open_out _HISTORY in
  output_string history deps; close_out history;; 

let get_old_dependencies () =
  let rec split_string s = 
    try
      let space_index = String.index s ' ' in
      let first_half = String.sub s 0 space_index
      and second_half = String.sub s (space_index + 1) (String.length s - space_index - 1) in
      first_half::(split_string second_half)
    with Not_found -> [s] in
  let open_file = open_in _HISTORY in
  let rec read_old_deps () =
    try
      let line = split_string (input_line open_file) in
      match line with
      | [] -> []
      | f::md5s -> (Filename(f), List.map (fun s -> if f = "DEFAULT" then MDNone else MD5(s)) md5s)::(read_old_deps ())
    with End_of_file -> [] in
  let dep_hash_alist = read_old_deps ()
  in close_in open_file; dep_hash_alist ;;

let prep_disk () =
  (try 
    if not (Sys.file_exists ".remodel")
    then Unix.mkdir ".remodel" 0o755
    else () ;
  with 
  | Unix.Unix_error(num,_,_) -> Printf.printf "Error in creating .remodel/ : %s\n" (Unix.error_message(num)) ; exit (-1) ;
  | Sys_error(msg) -> Printf.printf "Error eminating from creating .remodel/ : %s\n" msg ; exit (-1));
  (try
    if not (Sys.file_exists _HISTORY) 
    then ignore (Sys.command ("touch "^_HISTORY)) else () ;
  with 
  | Unix.Unix_error(num,_,_) -> Printf.printf "Error in creating %s : %s\n" _HISTORY (Unix.error_message(num)) ; exit (-1)
  | Sys_error(msg) -> Printf.printf "Error eminating from creating %s : %s\n" _HISTORY msg ; exit (-1));
  (try
    if not (Sys.file_exists _LOGDONE)
    then Unix.mkdir _LOGDONE 0o755
    else 
      let logdir = Unix.opendir _LOGDONE in
      try
        while true 
        do 
          match Unix.readdir logdir with
          | "." | ".." -> ()
          | f -> Unix.unlink (_LOGDONE^f)
        done;
      with End_of_file -> ();
      Unix.closedir logdir;
  with 
  | Unix.Unix_error(num,_,_) -> Printf.printf "Error in creating %s : %s\n" _LOGDONE (Unix.error_message(num)) ; exit (-1)
  | Sys_error(msg) -> Printf.printf "Error eminating from creating %s : %s\n" _LOGDONE msg ; exit (-1));;

let parse_remodelfile () = 
  let f = open_in _REMODELFILE in
  let lexbuf = Lexing.from_channel f in
  try
    let p = Remodel_parser.toplevel Remodel_lexer.lexer lexbuf in
    close_in f ; p
      (* from http://stackoverflow.com/questions/1933101/ocamlyacc-parse-error-what-token  - exeception above *)
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
(*      let tail = Remodel_lexer.ruleTail "" lexbuf in *)
      raise (ParseException tok)
    end;;

let main program target =
  ignore(exec_parallel_commands program target) ;
  record_dependencies program ;;

(* program entry *)
let args = Sys.argv in 
  prep_disk(); 
  old_dependencies := get_old_dependencies() ;
  let program = parse_remodelfile ()
  and target = match Array.length args with
    | 1 -> [| Filename("DEFAULT") |]
    | _ -> Array.map (fun f -> Filename(f)) (Array.sub args 1 (Array.length args - 1)) in
  Array.map (function t -> main program t) target
  
   ;; 