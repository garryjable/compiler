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
%token L_PAREN
%token R_PAREN
%token L_BRACKET
%token R_BRACKET
%token ASSIGN
%token MOD

%type <val> INTEGER
%type <character> CHAR
%type <boolean> BOOLEAN
%type <val> STRING
%type <id> ID

%%
/* some practice rules */

Program: OptConstDecl OptTypeDecl OptVarDecl ProcFuncDecls Block {}
       ;

ProcFuncDecls: ProcFuncDecl {}
                | ProcFuncDecls ProcFuncDecl {}
                | /*empty*/ {}
                ;

ProcFuncDecl: ProcDecl {}
            | FuncDecl {}
            ;

OptConstDecl: ConstantDecl {}
            | /*empty*/ {}
            ;

OptTypeDecl: TypeDecl {}
           | /*empty*/ {}
           ;

OptVarDecl: VarDecl {}
           | /*empty*/ {}
           ;

/*constand declarations 3.1.1*/
ConstantDecl: CONST ConstAssigns {}
            ;

ConstAssigns: ConstAssign {}
       | ConstAssigns ConstAssign {}
       ;

ConstAssign: ID ASSIGN Expression SEMICOLON {} /* this needs a thing*/
           ;

/*3.1.2 Procedure and FUnction Declarations */
ProcedureDecl: PROCEDURE ID L_PAREN FormalParameters R_PAREN SEMICOLON FORWARD SEMICOLON {}
             | PROCEDURE ID L_PAREN FormalParameters R_PAREN SEMICOLON Body SEMICOLON {}
             ;
FunctionDecl: FUNCTION ID L_PAREN FormalParameters R_PAREN COLON Type SEMICOLON FORWARD SEMICOLON {}
            | FUNCTION ID L_PAREN FormalParameters R_PAREN COLON Type SEMICOLON Body SEMICOLON {} 
            ;
FormalParameters: {}
                | /*empty*/ {}
                | OptVarRef IdentList COLON Type AdditionalParameters {}
                ;
AdditionalParameters: /*empty*/
                    | SEMICOLON OptVarRef IdentList COLON Type AdditionalParameters {}
                    ;
OptVarRef : var {}
          | ref {}
          | /*empty*/ {}
          ;

Body: OptConstDecl OptTypeDecl OptVarDecl Block {}
    ;
Block: BEGIN StatementSequence END {}
     ;
/* 3.1.3 Type declarations */
TypeDecl: TYPE TypeAssigns {}
        ;
TypeAssigns: TypeAssign {}
           | TypeAssigns TypeAssign {}
           ;
TypeAssign: ID = TYPE SEMICOLON {} /* this needs C code */
          ; 

Type: SimpleType {}
    | RecordType {}
    | ArrayType {}
    ;

SimpleType: ID {}
          ;
RecordType: RECORD RecordList END {}
          ;

RecordList: IdentList COLON Type {}
          : RecordList IdentList COLON Type {}
          : /* empty */ 
          ;
ArrayType: ARRAY [ Expression : Expression ] OF Type {}
          ;
IdentList: ID AdditionalIdents {}
         ;

AdditionalIdents: COMMA ID {}
                | AdditionalIdents COMMA ID {}
                | /*empty*/ {}
                ;

/* 3.1.4 Variable Declarations */
VarDecl: VAR VarList {}
       ;


VarList: IdentList COLON Type SEMICOLON {}
       | VarList IdentList COLON Type SEMICOLON  {}
       ;

/* 3.2 CPSL Statements */

StatementSequence: Statement AdditionalStatements {}
                 ;
AdditionalStatements: IdentList SEMICOLON Type SEMICOLON {}
                    | AdditionalStatements SEMICOLON Type SEMICOLON {}
                    | /*empty*/ {}
                    ;

Statement: Assignment {}
         | IfStatement {}
         | WhileStatement {}
         | RepeatStatement {}
         | ForStatement {}
         | StopStatement {}
         | ReturnStatement {}
         | ReadStatement {}
         | WriteStatement {}
         | ProcedureCall {}
         | NullStatement {}
         ;

Assignment: LValue ASSIGN Expression {}
          ;
IfStatement: IF Expression THEN StatementSequence AdditionalElseIfs ELSE OptionalElse END {}
           ;
AdditionalElseIfs: ELSEIF Expression THEN StatementSequence {}
                 : AdditionalElseIfs ELSEIF Expression THEN StatementSequence {}
                 : /*empty*/ {}
                 ;
OptionalElse: ELSE StatementSequence {}
            : /* empty */ {}
            ;
WhileStatement: WHILE Expression DO StatementSequence END {}
              ;
RepeatStatement: REPEAT StatementSequence UNTIL Expression {}
               ;
ForStatement: FOR ID ASSIGN Expression ToOrDownTo Expression DO StatementSequence END {}
            ;
ToOrDownto: TO {}
          | DOWNTO {}
          ;
StopStatement: STOP {}
             ;
ReturnStatement: RETURN OptExpression {}
               ;
OptExpression: Expression {}
             | /* empty*/ {}
             ;
ReadStatement: READ L_PAREN LValue LValueList R_PAREN {}
             ;
LValueList: COMMA LValue {}
          : LValueList COMMA LValue {}
          : /*empty*/ {}
          ;
WriteStatement: WRITE L_PAREN Expression AdditionalExpressions R_PAREN {}
              ;
AdditionalExpressions: COMMA Expression {}
                     : AdditionalExpressions COMMA Expression {}
                     : /*empty*/ {}
                     ;
ProcedureCall: ID L_PAREN Expression AdditionalExpressions R_PAREN {}
             ;
OptAdditionalExpressions: AdditionalExpressions {}  /*THIS SEEMS IRRELEVANT*/
                        | /* empty */ {}
                        ;
NullStatement: /*empty*/ {}
             ;

/* 3.3 Expressions */

Expression: Expression OR Expression {}
          | Expression AND Expression {}
          | Expression EQUAL Expression {}
          | Expression NOT_EQUAL Expression {}
          | Expression LESS_EQUAL Expression {}
          | Expression GREATER_EQUAL Expression {}
          | Expression LESS_THAN Expression {}
          | Expression GREATER_THAN Expression {}
          | Expression ADD Expression {}
          | Expression SUB Expression {}
          | Expression MULT Expression {}
          | Expression DIV Expression {}
          | Expression MOD Expression {}
          | TILDE Expression {}
          | SUB Expression {}
          | L_PAREN Expression R_PAREN {}
          | ID L_PAREN Expression AdditionalExpressions R_PAREN {}
          | CHR L_PAREN Expression R_PAREN {}
          | ORD L_PAREN Expression R_PAREN {}
          | PRED L_PAREN Expression R_PAREN {}
          | SUCC L_PAREN Expression R_PAREN {}
          | LValue {}
          ;

LValue: ID ((ID() {} /* WTF!!!!!!!!!!!!!!*/
      ;

LValueInternals: DotIdentOrExpression {}
               : LValueInternals DotIdentOrExpression {}
               : /*empty*/ {}
               ;
DotIdentOrExpression: DOT ID {}
                    | L_BRACKET Expression R_BRACKET {}
                    ;

/* 4.1 Constant Expression */

ConstantDecl: CONST ConstantAssignments {}
            ;
ConstantAssignments: ConstExpression SEMICOLON {}
                   : ConstantAssignments ConstExpression SEMICOLON {}
                   ;
ArrayType: ARRAY L_BRACKET ConstExpression COLON ConstExpression R_BRACKET OF Type {}
         ;

ConstExpression: ConstExpression OR ConstExpression {}
               | ConstExpression AND ConstExpression {}
               | ConstExpression EQUAL ConstExpression {}
               | ConstExpression NOT_EQUAL ConstExpression {}
               | ConstExpression LESS_EQUAL ConstExpression {}
               | ConstExpression GREATER_EQUAL ConstExpression {}
               | ConstExpression LESS_THAN ConstExpression {}
               | ConstExpression GREATER_THAN ConstExpression {}
               | ConstExpression ADD ConstExpression {}
               | ConstExpression SUB ConstExpression {}
               | ConstExpression MULT ConstExpression {}
               | ConstExpression DIV ConstExpression {}
               | ConstExpression MOD ConstExpression {}
               | TILDE ConstExpression {}
               | SUB ConstExpression {}
               | L_PAREN ConstExpression R_PAREN {}
               | INT /*what is this */ {}
               | CHAR /*what is this */ {}
               | STR /*what is this */ {}
               | ID /*what is this */ {}
               ;

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

