%{
#include <iostream>
#include <map>

#include "symbol_table.hpp"

extern "C" int yylex();
void yyerror(const char*);
%}

%union
{
char character;
float val;
char* id;
bool boolean;
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
%type <character> CHAR
%type <boolean> BOOLEAN
%type <val> STRING
%type <id> ID

%%
/* some practice rules */

Program: OptConstantDecl OptTypeDecl OptVarDecl PorFDeclarations Block {}
       ;

PorFDeclarations: PorFDeclaration
                | PfDeclarations PfDeclaration
                | /*empty*/
                ;

PorFDeclaration: ProcedureDecl
               | FunctionDecl
               ;

OptConstDecl: ConstantDecl
            | /*empty*/
            ;

OptTypeDecl: TypeDecl
           | /*empty*/
           ;

OptVarDecl: OptVarDecl
           | /*empty*/
           ;

/*constand declarations 3.1.1*/
ConstantDecl: CONST ConstAssigns
            ;

ConstAssigns: ConstAssign
       | ConstAssigns ConstAssign
       ;

ConstAssign: ID ASSIGNMENT Expression SEMICOLON
           ;

/*3.1.2 Procedure and FUnction Declarations */
ProcedureDecl: PROCEDURE ID LEFTPAREN FormalParameters RIGHTPAREN SEMICOLON FORWARD SEMICOLON {}
             | PROCEDURE ID LEFTPAREN FormalParameters RIGHTPAREN SEMICOLON Body SEMICOLON {}
             ;
FunctionDecl: FUNCTION ID LEFTPAREN FormalParameters RIGHTPAREN COLON Type SEMICOLON FORWARD SEMICOLON {}
            | FUNCTION ID LEFTPAREN FormalParameters RIGHTPAREN COLON Type SEMICOLON Body SEMICOLON {} 
            ;
FormalParameters: {}
                | /*empty*/
                | OptVarRef IdentList COLON Type AdditionalParameters 
                ;
AdditionalParameters: /*empty*/
                    | SEMICOLON OptVarRef IdentList COLON Type AdditionalParameters
                    ;
OptVarRef : var
          | ref
          | /*empty*/
          ;

Body: OptConstDecl OptTypeDecl OptVarDecl Block
    ;
Block: BEGIN StatementSequence END
     ;
/* Type declarations */
TypeDecl: TYPE ID = TYPE +; 

Type: SimpleType
    | RecordType
    | ArrayType
    ;

SimpleType: ID
          ;
RecordType: RECORD IdentList: Type END
          ;
ArrayType: ARRAY Expression : Expression of TYPE
          ;
IdentList: ID (, ID)*
         ;

/* 3.1.4 Variable Declarations */
VarDecl: VAR IdentList Type
       ;

/* 3.2 CPSL Statements */

StatementSequence: Statement (; Statement) *
                 ;

Statement: Assignment
         | IfStatement
         | WhileStatement
         | RepeatStatement
         | ForStatement
         | StopStatement
         | ReturnStatement
         | ReadStatement
         | WriteStatement
         | ProcedureCall
         | NullStatement
         ;

Assignment: LValue ASSIGNMENT Expression
          ;
IfStatement: IF Expression THEN StatementSequence ELSEIF Expression THEN StatementSequence ELSE StatementSequence END
           ;
WhileStatement: WHILE Expression DO StatementSequence END
              ;
RepeatStatement: REPEAT StatementSequence UNTIL Expression
               ;
ForStatement: FOR ID ASSIGN Expression to|downto Expression DO StatementSequence END
            ;
StopStatement: STOP
             ;
ReturnStatement: RETURN Expression
               ;
ReadStatement: READ LValue (,LValue)*
             ;
WriteStatement: WRITE Expression (,Expression)*
              ;
ProcedureCall: ID Expression (,Expression)
             ;
NullStatement: {}
             ;

/* 3.3 Expressions */

Expression: Expression OR Expression
          | Expression AND Expression
          | Expression EQUAL Expression
          | Expression NOTEQUAL Expression
          | Expression LESSEQUAL Expression
          | Expression GREATEREQUAL Expression
          | Expression LESSTHAN Expression
          | Expression GREATERTHAN Expression
          | Expression ADD Expression
          | Expression SUB Expression
          | Expression MULT Expression
          | Expression DIV Expression
          | Expression MODULUS Expression
          | TILDE Expression
          | SUB Expression
          | LEFTPAREN Expression RIGHTPAREN
          | ID LEFTPAREN Expression  (,Expression)*  RIGHTPAREN
          | CHR LEFTPAREN Expression RIGHTPAREN
          | ORD LEFTPAREN Expression RIGHTPAREN
          | PRED LEFTPAREN Expression RIGHTPAREN
          | SUCC LEFTPAREN Expression RIGHTPAREN
          | LValue
          ;

LValue: ID ((ID() 
      ;

/* 4.1 Constant Expression */

ConstExpression: ConstExpression OR ConstExpression
               | ConstExpression AND ConstExpression
               | ConstExpression EQUAL ConstExpression
               | ConstExpression NOTEQUAL ConstExpression
               | ConstExpression LESSEQUAL ConstExpression
               | ConstExpression GREATEREQUAL ConstExpression
               | ConstExpression LESSTHAN ConstExpression
               | ConstExpression GREATERTHAN ConstExpression
               | ConstExpression ADD ConstExpression
               | ConstExpression SUB ConstExpression
               | ConstExpression MULT ConstExpression
               | ConstExpression DIV ConstExpression
               | ConstExpression MODULUS ConstExpression
               | TILDE ConstExpression
               | SUB ConstExpression
               | LEFTPAREN ConstExpression RIGHTPAREN
               | INT /*what is this */
               | CHAR /*what is this */
               | STR /*what is this */
               | ID /*what is this */






/*original rules*/
StatementList : StatementList Statement {}
              | {}
              ;
Statement : Expression DONE {std::cout << $1 << std::endl;}
          | LET ID EQUAL Expression DONE {symbol_table.store($2,$4);delete($2);}
          | DONE {}
          ;
Expression : Expression ADD Term {$$ = $1 + $3;}
           | Expression SUB Term {$$ = $1 - $3;}
           | Term {$$ = $1;}
           ;

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

