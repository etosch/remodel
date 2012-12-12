{
	open Remodel_parser ;;
}

rule lexer = parse 
| "<-"				{ Ldeparrow }
| ":"				{ Lcolon }
| ","				{ Lcomma }
| '"' [^ '"']* '"' 	{ Lcommand (Lexing.lexeme lexbuf) }
| [' ' '\t' '\n']	{ lexer lexbuf }
| eof				{ Leof }
| _					{ Lfilename (Lexing.lexeme lexbuf)}