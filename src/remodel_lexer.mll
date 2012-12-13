{
	open Remodel_parser ;;
	let print s t = 
		print_string ("\nPRINT "^t^":\tx"^s^"x\n");;
	print_string (Sys.getcwd());;
}

rule lexer = parse 
| "<-" as a				{ print a "a" ; Ldeparrow }
| ":" as b				{ print_char b ; Lcolon }
| "," as c 			{ print_char c; Lcomma }
| '"' [^ '"']* '"' as d 	{ print d "d" ; Lcommand (Lexing.lexeme lexbuf) }
| [' ' '\t' '\n'] as e 	{ print_char e ; lexer lexbuf }
| eof	as f			{ print f "f"; Leof }
| ['A'-'Z' 'a'-'z' '.' '/' '0'-'9']+ as s			{ print s "s"; Lfilename (Lexing.lexeme lexbuf)}