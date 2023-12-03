{
  open Parser2
}

let num = ['1'-'9'] ['0'-'9']*

rule token = parse
  | [' ' '\t' '\r' '\n']+ { token lexbuf }
  | "Game"   { GAME }
  | num as n { NUM (int_of_string n) }
  | "blue"   { BLUE }
  | "green"  { GREEN }
  | "red"    { RED }
  | ","      { COMMA }
  | ";"      { SEMI }
  | ":"      { COLON }
  | eof      { EOF }

{ }
