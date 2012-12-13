%{
	open Remodel_types ;;
%}

%token Lcomma Lcolon Ldeparrow Leof
%token <string> Lfilename 
%token <string> Lcommand
%left Ldeparrow
%start toplevel
%type <Remodel_types.program> toplevel
 %%

toplevel : 
  | program Leof								{ Program($1) }

program : 
  | production 									{ [$1] }
  | production program							{ $1::$2 }

production :
  | target Ldeparrow dependency					{ Production(Target($1), Dependency($3), Command("")) }
  | target Ldeparrow dependency Lcolon Lcommand	{ Production(Target($1), Dependency($3), Command($5)) }

target :
  | Lfilename									{ [Filename($1)] }
  | Lfilename Lcomma target        				{ Filename($1)::$3 }

dependency :
  | Lfilename									{ [Filename($1)] }
  | Lfilename Lcomma dependency 				{ Filename($1)::$3 }


%%