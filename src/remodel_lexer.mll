{
	open Remodel_parser ;;
	let strip_quotes s =
		String.sub s 1 (String.length s - 2)
}

rule lexer = parse 
| "<-"									{ Ldeparrow }
| ":" 									{ Lcolon }
| ","  									{ Lcomma }
| '"' [^ '"']* '"'  					{ Lcommand (strip_quotes (Lexing.lexeme lexbuf)) }
| [' ' '\t' '\n']  						{ lexer lexbuf }
| eof									{ Leof }
| ['A'-'Z' 'a'-'z' '.' '/' '0'-'9']+ 	{ Lfilename (Lexing.lexeme lexbuf) }