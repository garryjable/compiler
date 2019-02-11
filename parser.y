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
%token BEG
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
%token INTEGER
%token CHAR
%token BOOLEAN
%token STRING
%token OCTAL
%token HEX
%token DECIMAL

/* octals and HEX? */
/* comments */

%type <val> INTEGER
%type <character> CHAR
%type <boolean> BOOLEAN
%type <val> STRING
%type <id> ID

%right UMINUS
%left MULT DIV MOD
%left ADD SUB
%nonassoc EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%right TILDE
%left AND
%left OR

%%
/* some practice rules */

Program: OptConstDecl OptTypeDecl OptVarDecl ProcFuncDecls Block {}
       ;

ProcFuncDecls: ProcFuncDecl ProcFuncDecls {}
             | /*empty*/ {}
             ;

ProcFuncDecl: ProcedureDecl {}
            | FunctionDecl {}
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
ConstantDecl: CONST ConstAssign ConstAssigns {}
            ;

ConstAssigns: ConstAssign ConstAssigns {}
            | /*empty*/{}
            ;

ConstAssign: ID EQUAL Expression SEMICOLON {} /* this needs a thing*/
           ;


/*3.1.2 Procedure and FUnction Declarations */
ProcedureDecl: PROCEDURE ID L_PAREN FormalParameters R_PAREN SEMICOLON FORWARD SEMICOLON {}
             | PROCEDURE ID L_PAREN FormalParameters R_PAREN SEMICOLON Body SEMICOLON {}
             ;
FunctionDecl: FUNCTION ID L_PAREN FormalParameters R_PAREN COLON Type SEMICOLON FORWARD SEMICOLON {}
            | FUNCTION ID L_PAREN FormalParameters R_PAREN COLON Type SEMICOLON Body SEMICOLON {}
            ;
FormalParameters: OptVarRef IdentList COLON Type AdditionalParameters {}
                | /*empty*/ {}
                ;
AdditionalParameters: SEMICOLON OptVarRef IdentList COLON Type AdditionalParameters {}
                    | /*empty*/
                    ;
OptVarRef : VAR {}
          | REF {}
          | /*empty*/ {}
          ;

Body: OptConstDecl OptTypeDecl OptVarDecl Block {}
    ;
Block: BEG StatementSequence END {}
     ;
/* 3.1.3 Type declarations */
TypeDecl: TYPE TypeAssign TypeAssigns {}
        ;
TypeAssigns: TypeAssign TypeAssigns {}
           | /*empty*/ {}
           ;
TypeAssign: ID EQUAL Type SEMICOLON {} /* this needs C code */
          ; 

Type: SimpleType {}
    | RecordType {}
    | ArrayType {}
    ;

SimpleType: ID {}
          ;
RecordType: RECORD RecordList END {}
          ;

RecordList: IdentList COLON Type SEMICOLON RecordList {}
          | /* empty */ 
          ;
ArrayType: ARRAY L_BRACKET Expression COLON Expression R_BRACKET OF Type {}
         ;
IdentList: ID AdditionalIdents {}
         ;

AdditionalIdents: COMMA ID AdditionalIdents {}
                | /*empty*/ {}
                ;

/* 3.1.4 Variable Declarations */
VarDecl: VAR VarAssign VarAssigns {}
       ;

VarAssigns: VarAssign VarAssigns {}
          | /*empty*/
          ;

VarAssign: IdentList COLON Type SEMICOLON {}
         ;

/* 3.2 CPSL Statements */

StatementSequence: Statement AdditionalStatements {}
                 ;
AdditionalStatements: SEMICOLON Statement AdditionalStatements {}
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
IfStatement: IF Expression THEN StatementSequence AdditionalElseIfs OptionalElse END {}
           ;
AdditionalElseIfs: ELSEIF Expression THEN StatementSequence AdditionalElseIfs {}
                 | /*empty*/ {}
                 ;
OptionalElse: ELSE StatementSequence {}
            | /* empty */ {}
            ;
WhileStatement: WHILE Expression DO StatementSequence END {}
              ;
RepeatStatement: REPEAT StatementSequence UNTIL Expression {}
               ;
ForStatement: FOR ID ASSIGN Expression ToOrDownto Expression DO StatementSequence END {}
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
LValueList: COMMA LValue LValueList {}
          | /*empty*/ {}
          ;
WriteStatement: WRITE L_PAREN Expression AdditionalExpressions R_PAREN {}
              ;
AdditionalExpressions: COMMA Expression AdditionalExpressions {}
                     | /*empty*/ {}
                     ;
ProcedureCall: ID L_PAREN Expression OptExpressions R_PAREN {}
             ;

OptExpressions: Expression AdditionalExpressions
              | /*empty*/
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
          | UMINUS Expression {}
          | L_PAREN Expression R_PAREN {}
          | ID L_PAREN OptExpressions R_PAREN {}
          | CHR L_PAREN Expression R_PAREN {}
          | ORD L_PAREN Expression R_PAREN {}
          | PRED L_PAREN Expression R_PAREN {}
          | SUCC L_PAREN Expression R_PAREN {}
          | LValue {}
          ;

LValue: ID {}
      | LValue DOT ID {}
      | LValue L_BRACKET Expression R_BRACKET {}
      ;

%%

void yyerror(const char* msg)
{
  std::cerr << msg << std::endl;
}

