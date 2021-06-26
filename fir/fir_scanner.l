%option c++ prefix="fir_scanner_" outfile="fir_scanner.cpp" debug
%option stack noyywrap yylineno 8bit
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "fir_parser.tab.h"

// output stream for building string literals
static std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}

INT       [[:digit:]]+

LETTER  [A-Za-z_]
IDENTIFIER   {LETTER}({LETTER}|[[:digit:]])*

%x X_COMMENT
%x X_STRING X_TIL
%x X_OCT_INT


%%

    {  yydebug = 1; set_debug(true); }

  /* ====================================================================== */
  /* ====[                      2.2 - COMMENTS                        ]==== */
  /* ====================================================================== */

"!!".*$                /* match the whole line (it is ignored) */

"(*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*)"        yy_pop_state();
<X_COMMENT>.|"\n"      ;  /* ignore inside comments */



  /* ====================================================================== */
  /* ====[                  3.4   - Tipos de dados                    ]==== */
  /* ====[                  3.4   - Null                              ]==== */
  /* ====[                  3.4   - Sizeoff                           ]==== */
  /* ====================================================================== */

"->"                     return tLITERAL;
"int"                    return tTYPE_INT;
"float"                  return tTYPE_FLOAT;
"string"                 return tTYPE_STRING;
"void"                   return tTYPE_VOID;
[<>]                     return tTYPE_POINTER;

"null"                   return tNULL;
"sizeof"                 return tSIZEOF;

  /* ====================================================================== */
  /* ====[                  4.5 - Símbolos globais                  ]==== */
  /* ====================================================================== */

"*"                      return tPUBLIC;
"?"                      return tREQUIRE;

  /* ====================================================================== */
  /* ====[                 6.2 - Instrução condicional                ]==== */
  /* ====================================================================== */

"if"                   return tIF;
"then"                 return tTHEN;
"elif"                 return tELIF;
"else"                 return tELSE;

  /* ====================================================================== */
  /* ====[                 6.3 - Instrução de iteração                ]==== */
  /* ====[                 6.4 - Instrução de terminação              ]==== */
  /* ====[                 6.5 - Instrução de reinício                ]==== */
  /* ====================================================================== */

"while"                 return tWHILE;
"do"                    return tDO;
"leave"                 return tLEAVE;
"restart"               return tRESTART;
"finally"               return tFINALLY;

  /* ====================================================================== */
  /* ====[                 7 - Operadores de expressões               ]==== */
  /* ====================================================================== */

[\[\](),?]             return *yytext;
[-+*/%~<>@?=]          return *yytext;
"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"&&"                   return tAND;
"||"                   return tOR;



  /* ====================================================================== */
  /* ====[              3   - Delimitadores e separadores             ]==== */
  /* ====[              5.3 - Corpo (de função)                       ]==== */
  /* ====[                 5.3 - Instrução de retorno                 ]==== */
  /* ====================================================================== */

[#\[\](){};:!,]         return *yytext;
"return"                return tRETURN;
"write"                 return tWRITE;
"writeln"               return tWRITELN;

  /* ====================================================================== */
  /* ====[                   3.8.1 - inteiros                         ]==== */
  /* ====================================================================== */

{INT}                  yylval.i = std::stoi(yytext, NULL, 10); return tINTEGER;

"0"                     yy_push_state(X_OCT_INT);
<X_OCT_INT>[0-7]+       { yylval.i = std::stoi(yytext, NULL, 8); yy_pop_state(); return tINTEGER; } 
<X_OCT_INT>.|\n         std::cerr << "WARNING: line " << lineno() << ": bad octal data!" << std::endl; exit(1);



  /* ====================================================================== */
  /* ====[                   3.8.2 - reais                            ]==== */
  /* ====================================================================== */

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { yylval.d = std::stod(yytext, NULL); return tFLOAT; }


  /* ====================================================================== */
  /* ====[                3.7 - Identificadores (nomes)               ]==== */
  /* ====================================================================== */

{IDENTIFIER}  yylval.s = new std::string(yytext); return tID;



  /* ====================================================================== */
  /* ====[              3.8.3 - Cadeias de caracteres                 ]==== */
  /* ====================================================================== */

"'"                            yy_push_state(X_STRING);
<X_STRING>~                    yy_push_state(X_TIL);
<X_STRING>"'"                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_STRING>.                    strlit << *yytext;

<X_TIL>n                       strlit << "~n"; yy_pop_state();
<X_TIL>r                       strlit << "~r"; yy_pop_state();
<X_TIL>t                       strlit << "~t"; yy_pop_state();
<X_TIL>~                       strlit << "~~"; yy_pop_state();

<X_TIL>[[:xdigit:]]{1,2} {
                                 strlit << (char)(unsigned char)std::stoi(yytext, NULL, 16);
                                 yy_pop_state();
                               }
<X_TIL>.                       strlit << *yytext; yy_pop_state();


  /* ====================================================================== */
  /* ====[                    (other characters)                      ]==== */
  /* ====================================================================== */

">>"                          return tEPILOGUE;

[[:space:]]         /* ignore remaining white space inside the program */
.                      yyerror(yytext);    /* all other chars: error! */

  /* ====================================================================== */
  /* ====[                           THE END                          ]==== */
  /* ====================================================================== */

%%