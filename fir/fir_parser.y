%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include "ast/all.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;    /* double value */
  std::string          *s;	/* symbol name or string literal */

  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  fir::function_declaration_node *function;
  fir::body_node       *body;
  fir::block_node      *block;
};

%token tAND tOR tNE TLE tGE tSIZEOF tEPILOGUE
%token tWRITE tWRITELN
%token tPUBLIC tREQUIRE tPRIVATE
%token tTYPE_INT tTYPE_VOID tNULL tTYPE_FLOAT tTYPE_STRING tTYPE_POINTER tLITERAL 
%token tIF tTHEN tELIF tELSE
%token tWHILE tDO
%token tLEAVE tRESTART tFINALLY tRETURN


%token <i> tINTEGER
%token<d> tFLOAT
%token<s> tSTRING tID 


%type <node>  instruction return
%type <sequence> file instructions opt_instructions expressions opt_expressions
%type <expression> expression opt_initializer  integer float
%type <lvalue> lvalue
%type <block> block

%type <function> fundec
%type <node> declaration vardec argdec
%type <sequence> declarations vardecs argdecs opt_vardecs
%type <body> body

%type <s> string
%type <type> data_type 

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tOR
%left tAND
%right '~'
%left tNE tEQ
%left '<' tLE tGE '>'
%left '+' '-'
%left '*' '/' '%'
%right tUMINUS



%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file         : /* empty */                                                          { compiler->ast($$ = new cdk::sequence_node(LINE)); }
             | declarations                                                         { compiler->ast($$ = $1); }
             ;

declarations :              declaration                                             { $$ = new cdk::sequence_node(LINE, $1);     }
             | declarations declaration                                             { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration  : vardec ';'                                                           { $$ = $1; }
             | fundec                                                               { $$ = $1; }
             ;

vardec       : data_type tREQUIRE  tID                                              { $$ = new fir::variable_declaration_node(LINE, tPUBLIC,  $1, *$3, nullptr); }
             | data_type tPUBLIC   tID   opt_initializer                            { $$ = new fir::variable_declaration_node(LINE, tPUBLIC,  $1, *$3, $4); }
             |          data_type  tID   opt_initializer                            { $$ = new fir::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
             ;

vardecs      : vardec ';'                                                           { $$ = new cdk::sequence_node(LINE, $1);     }
             | vardecs vardec ';'                                                   { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

opt_vardecs  : /* empty */                                                          { $$ = NULL; }
             | vardecs                                                              { $$ = $1; }
             ;

data_type    : tTYPE_STRING                                                         { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
             | tTYPE_INT                                                            { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
             | tTYPE_FLOAT                                                          { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
             | tTYPE_POINTER '<' data_type '>'                                      { $$ = cdk::reference_type::create(4, $3); }
             | tTYPE_VOID                                                           { $$ = cdk::primitive_type::create(8, cdk::TYPE_VOID); }
             ;

opt_initializer  : /* empty */                                                      { $$ = nullptr; /* must be nullptr, not NIL */ }
                 | '=' expression                                                   { $$ = $2; }
                 ;


fundec       : data_type           tID '(' argdecs ')' tLITERAL tINTEGER body           { $$ = new fir::function_declaration_node(LINE, $1, tPRIVATE, *$2, $4, $8); }
             | data_type tREQUIRE  tID '(' argdecs ')' tLITERAL tINTEGER body           { $$ = new fir::function_declaration_node(LINE, $1, tREQUIRE, *$3, $5, $9); }
             | data_type tPUBLIC   tID '(' argdecs ')' tLITERAL tINTEGER body           { $$ = new fir::function_declaration_node(LINE, $1, tPUBLIC, *$3, $5, $9); }
             ;

argdecs  : /* empty */                                                              { $$ = new cdk::sequence_node(LINE);  }
         |             argdec                                                       { $$ = new cdk::sequence_node(LINE, $1); }
         | argdecs ',' argdec                                                       { $$ = new cdk::sequence_node(LINE, $3, $1); }
         ;

argdec   : data_type tID                                                            { $$ = new fir::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
         ;

body         : '@' block block tEPILOGUE block                                      { $$ = new fir::body_node(LINE, $2, $3, $5); }
             | '@' block                                                            { $$ = new fir::body_node(LINE, $2, nullptr, nullptr);}
             | '@' block block                                                      { $$ = new fir::body_node(LINE, $2, $3, nullptr);}
             | block                                                                { $$ = new fir::body_node(LINE, nullptr, $1, nullptr);}
             | block tEPILOGUE block                                                { $$ = new fir::body_node(LINE, nullptr, $1, $3);}
             | tEPILOGUE block                                                      { $$ = new fir::body_node(LINE, nullptr, nullptr, $2);}
             | '@' block tEPILOGUE block                                            { $$ = new fir::body_node(LINE, $2, nullptr, $4);}
             ;

block        : '{' opt_vardecs opt_instructions '}'                                 { $$ = new fir::block_node(LINE, $2, $3); }
             ;

instructions    : instruction                                                       { $$ = new cdk::sequence_node(LINE, $1);     }
                | instructions instruction                                          { $$ = new cdk::sequence_node(LINE, $2, $1); }
                ;

opt_instructions : /* empty */                                                      { $$ = new cdk::sequence_node(LINE); }
                | instructions                                                      { $$ = $1; }

return          : tRETURN ';'                                                       { $$ = new fir::return_node(LINE, nullptr); }
                ;

instruction     : tIF expression tTHEN instruction                                  { $$ = new fir::if_node(LINE, $2, $4); }
                | tIF expression tTHEN instruction tELSE instruction                { $$ = new fir::if_else_node(LINE, $2, $4, $6); }
                | tWHILE expression tDO instruction                                 { $$ = new fir::while_node(LINE, $2, $4); }
                | tWHILE expression tDO instruction tFINALLY instruction            { $$ = new fir::while_node(LINE, $2, $4, $6); }
                | expression ';'                                                    { $$ = new fir::evaluation_node(LINE, $1); }
                | tWRITE   expressions ';'                                          { $$ = new fir::print_node(LINE, $2, false); }
                | tWRITELN expressions ';'                                          { $$ = new fir::print_node(LINE, $2, true); }
                | tLEAVE ';'                                                        { $$ = new fir::leave_node(LINE);  }
                | tLEAVE tINTEGER ';'                                               { $$ = new fir::leave_node(LINE, $2);  }
                | tRESTART ';'                                                      { $$ = new fir::restart_node(LINE); }
                | tRESTART tINTEGER ';'                                             { $$ = new fir::restart_node(LINE, $2); }
                | return                                                            { $$ = $1; }
                | block                                                             { $$ = $1; }
                ;

lvalue          : tID                                                               { $$ = new cdk::variable_node(LINE, *$1); delete $1; }
                | lvalue             '[' expression ']'                             { $$ = new fir::index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
                | '(' expression ')' '[' expression ']'                             { $$ = new fir::index_node(LINE, $2, $5); }
                | tID '(' opt_expressions ')' '[' expression ']'                    { $$ = new fir::index_node(LINE, new fir::function_call_node(LINE, *$1, $3), $6); }
                ;

expression      : integer                                                           { $$ = $1; }
                | float                                                             { $$ = $1; }
                | string                                                            { $$ = new cdk::string_node(LINE, $1); }
                | tNULL                                                             { $$ = new fir::null_node(LINE); }
                /* LEFT VALUES */
                | lvalue                                                            { $$ = new cdk::rvalue_node(LINE, $1); }
                /* ASSIGNMENTS */
                | lvalue '=' expression                                             { $$ = new cdk::assignment_node(LINE, $1, $3); }
                /* ARITHMETIC EXPRESSIONS */
                | expression '+' expression                                         { $$ = new cdk::add_node(LINE, $1, $3); }
                | expression '-' expression                                         { $$ = new cdk::sub_node(LINE, $1, $3); }
                | expression '*' expression                                         { $$ = new cdk::mul_node(LINE, $1, $3); }
                | expression '/' expression                                         { $$ = new cdk::div_node(LINE, $1, $3); }
                | expression '%' expression                                         { $$ = new cdk::mod_node(LINE, $1, $3); }
                /* LOGICAL EXPRESSIONS */
                | expression  '<' expression                                        { $$ = new cdk::lt_node(LINE, $1, $3); }
                | expression tLE  expression                                        { $$ = new cdk::le_node(LINE, $1, $3); }
                | expression tEQ  expression                                        { $$ = new cdk::eq_node(LINE, $1, $3); }
                | expression tGE  expression                                        { $$ = new cdk::ge_node(LINE, $1, $3); }
                | expression  '>' expression                                        { $$ = new cdk::gt_node(LINE, $1, $3); }
                | expression tNE  expression                                        { $$ = new cdk::ne_node(LINE, $1, $3); }
                /* LOGICAL EXPRESSIONS */
                | expression tAND  expression                                       { $$ = new cdk::and_node(LINE, $1, $3); }
                | expression tOR   expression                                       { $$ = new cdk::or_node (LINE, $1, $3); }
                /* UNARY EXPRESSION */
                | '-' expression %prec tUMINUS                                      { $$ = new cdk::neg_node(LINE, $2); }
                | '+' expression %prec tUMINUS                                      { $$ = new fir::identity_node(LINE, $2); }
                | '~' expression                                                    { $$ = new cdk::not_node(LINE, $2); }
                /* OTHER EXPRESSION */
                | '@'                                                               { $$ = new fir::read_node(LINE); }
                /* OTHER EXPRESSION */
                | tID '(' opt_expressions ')'                                       { $$ = new fir::function_call_node(LINE, *$1, $3); delete $1; }
                | tSIZEOF '(' expression ')'                                        { $$ = new fir::sizeof_node(LINE, $3); }
                /* OTHER EXPRESSION */
                | '(' expression ')'                                                { $$ = $2; }
                | '[' expression ']'                                                { $$ = new fir::stack_alloc_node(LINE, $2); }
                | lvalue '?'                                                        { $$ = new fir::address_of_node(LINE, $1); }
                ;

expressions     : expression                                                        { $$ = new cdk::sequence_node(LINE, $1);     }
                | expressions ',' expression                                        { $$ = new cdk::sequence_node(LINE, $3, $1); }
                ;

opt_expressions : /* empty */                                                       { $$ = new cdk::sequence_node(LINE); }
                | expressions                                                       { $$ = $1; }
                ;

integer         : tINTEGER                                                          { $$ = new cdk::integer_node(LINE, $1); };
float           : tFLOAT                                                            { $$ = new cdk::double_node(LINE, $1); };
string          : tSTRING                                                           { $$ = $1; }
                | string tSTRING                                                    { $$ = $1; $$->append(*$2); delete $2; }
                ;

%%