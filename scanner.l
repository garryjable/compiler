%{
#include <cstdlib>

#include "parser.hpp"

extern "C" int yylex();
%}

%%

"(array|ARRAY)" {return ARRAY;}
"(begin|BEGIN)" {return BEGIN;}
"(chr|CHR)" {return CHR;}
"(const|CONST)" {return CONST;}
"(do|DO)" {return DO;}
"(downto|DOWNTO)" {return DOWNTO;}
"(else|ELSE)" {return ELSE;}
"(elseif|ELSEIF)" {return ELSEIF;}
"(end|END)" {return END;}
"(for|FOR)" {return FOR;}
"(forward|FORWARD)" {return FORWARD;}
"(function|FUNCTION)" {return FUNCTION;}
"(if|IF)" {return IF;}
"(of|OF)" {return OF;}
"(ord|ORD)" {return ORD;}
"(pred|PRED)" {return PRED;}
"(procedure|PROCEDURE)" {return PROCEDURE;}
"(read|READ)" {return READ;}
"(record|RECORD)" {return RECORD;}
"(ref|REF)" {return REF;}
"(repeat|REPEAT)" {return REPEAT;}
"(return|RETURN)" {return RETURN;}
"(stop|STOP)" {return STOP;}
"(succ|SUCC)" {return SUCC;}
"(then|THEN)" {return THEN;}
"(to|TO)" {return TO;}
"(type|TYPE)" {return TYPE;}
"(until|UNTIL)" {return UNTIL;}
"(var|VAR)" {return VAR;}
"(while|WHILE)" {return WHILE;}
"(write|WRITE)" {return WRITE;}
[a-zA-Z][a-zA-Z_]* {yylval.id = strdup(yytext);return ID;}
"+" {return ADD;}
"-" {return SUB;}
"*" {return MULT;}
"/" {return DIV;}
"&" {return AND;}
"|" {return OR;}
"~" {return TILDE;}
"=" {return EQUAL;}
"<>" {return NOT_EQUAL;}
"<" {return LESS_THAN;}
"<=" {return LESS_EQUAL;}
">" {return GREATER_THAN;}
">=" {return GREATER_EQUAL;}
"." {return DOT;}
"," {return COMMA;}
":" {return COLON;}
";" {return SEMICOLON;}
"(" {return LEFT_PAREN;}
")" {return RIGHT_PAREN;}
"[" {return LEFT_BRACKET;}
"]" {return RIGHT_BRACKET;}
":=" {return ASSIGNMENT;}
"%" {return MODULUS;}

[0][0-9]* {yylval.val = atof(yytext);return OCTAL;}
[0x][0-9]* {yylval.val = atof(yytext);return HEX;}
[1-9][0-9]* {yylval.val = atof(yytext);return DECIMAL;}

['][a-zA-Z0-9]['] {yylval.val = yytext;return CHAR;}
["][a-zA-Z0-9]*["] {yylval.val = yytext;return STRING;}

[$][.]*[\n] {;}

[ \n\t\r\b\f] {;}
%%


