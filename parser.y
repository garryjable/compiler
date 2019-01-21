%{
#include <iostream>
#include <map>

#include "symbol_table.hpp"

extern "C" int yylex();
void yyerror(const char*);
%}

%union
{
float val;
char* id;
}
%token ARRAY
%token BEGIN
%token CHR
%token CONST
%token DO
%token DOWNTO
%token ELSE
%token ELSEIF
%token END
%token FOR
%token FORWARD
%token FUNCTION
%token IF
%token OF
%token ORD
%token PRED
%token PROCEDURE
%token READ
%token RECORD
%token REF
%token REPEAT
%token RETURN
%token STOP
%token SUCC
%token THEN
%token TO
%token TYPE
%token UNTIL
%token VAR
%token WHILE
%token WRITE
%token ID
%token ADD
%token SUB
%token MULT
%token DIV
%token AND 
%token OR
%token TILDE
%token EQUAL
%token NOT_EQUAL
%token LESS_THAN
%token LESS_EQUAL
%token GREATER_THAN
%token GREATER_EQUAL
%token DOT
%token COMMA
%token COLON
%token SEMICOLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token ASSIGNMENT
%token MODULUS

%type <val> INTEGER
%type <val> CHAR
%type <val> BOOLEAN
%type <val> STRING
%type <id> ID

/*note there are also 2 constant values true and false*/

/*
%union
{
float val;
char* id;
}

%token ADD
%token SUB
%token MULT
%token DIV
%token OPEN
%token CLOSE
%token DONE
%token NUMBER
%token ID
%token EQUAL
%token LET

%type <val> NUMBER
%type <val> Expression
%type <val> Factor
%type <val> Term
%type <id> ID
*/
%%

StatementList : StatementList Statement{}
              | {};
Statement : Expression DONE {std::cout << $1 << std::endl;}
          | LET ID EQUAL Expression DONE{symbol_table.store($2,$4);delete($2);}
          | DONE{};
Expression : Expression ADD Term {$$ = $1 + $3;}
           | Expression SUB Term {$$ = $1 - $3;}
           | Term {$$ = $1;};

Term : Term MULT Factor { $$ = $1 * $3;}
     | Term Factor { $$ = $1 * $2;}
     | Term DIV Factor { $$ = $1 / $3;}
     | Factor {$$ = $1;}
     ;
Factor : OPEN Expression CLOSE {$$ = $2;}
       | NUMBER {$$ = $1;}
       | ID {$$ = symbol_table.lookup($1);delete($1);}
       ;

%%

void yyerror(const char* msg)
{
  std::cerr << msg << std::endl;
}

