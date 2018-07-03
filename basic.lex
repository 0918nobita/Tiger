datatype lexresult =
  Comment of string
  | Print
  | Integer of int
  | Identifier of string
  | String of string
  | EOF;

val linenum = ref 1;

val error = fn x => print (x ^ "\n");

val eof = fn () => EOF;

%%

%structure BasicLex

alphanum = [A-Za-z0-9];
alpha = [A-Za-z];
digit = [0-9];
string = \"[.*]+\";
ws = [\ \t\n];

%%

"print" => (Print);
{ws}+ => (lex());
%(.*)\n => (Comment yytext);
[a-z]{alphanum}* => (Identifier yytext);
string => (String yytext);
{digit}+ => (Integer (foldl (fn(a, r) => (ord(a) - ord(#"0")) + 10*r) 0 (explode yytext)));
