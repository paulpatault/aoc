%{
%}

%token EOF

%token GAME
%token <int> NUM
%token BLUE GREEN RED
%token COMMA SEMI COLON

%start input
%type <Types2.tgame> input

%%

input: game+ EOF { $1 };

game:
| GAME g=NUM COLON l=separated_nonempty_list(SEMI, values) { g,l }

values:
| separated_nonempty_list(COMMA, colored) { $1 }

colored:
| NUM BLUE  { Blue $1 }
| NUM GREEN { Green $1 }
| NUM RED   { Red $1 }
