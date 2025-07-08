%{
#include "default-defs.h"
#include "decafexpr.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

int lineno = 1;
int tokenpos = 1;
%}

%%

"package"                  { return T_PACKAGE; }
"var"                      { return T_VAR; }
"int"                      { return T_INT; }
"bool"                     { return T_BOOL; }
"void"                     { return T_VOID; }
"return"                   { return T_RETURN; }
"if"                       { return T_IF; }
"else"                     { return T_ELSE; }
"while"                    { return T_WHILE; }
"for"                      { return T_FOR; }
"break"                    { return T_BREAK; }
"continue"                 { return T_CONTINUE; }
"extern"                   { return T_EXTERN; }
"func"                     { return T_FUNC; }
"true"                     { return T_TRUE; }
"false"                    { return T_FALSE; }

"=="                       { return T_EQEQ; }
"!="                       { return T_NEQ; }
"<="                       { return T_LEQ; }
">="                       { return T_GEQ; }
"&&"                       { return T_AND; }
"||"                       { return T_OR; }
"!"                        { return T_NOT; }

"<"                        { return T_LT; }
">"                        { return T_GT; }
"="                        { return T_ASSIGN; }
"+"                        { return T_PLUS; }
"-"                        { return T_MINUS; }
"*"                        { return T_MULT; }
"/"                        { return T_DIV; }
"%"                        { return T_MOD; }

"("                        { return T_LPAREN; }
")"                        { return T_RPAREN; }
"{"                        { return T_LCB; }
"}"                        { return T_RCB; }
";"                        { return T_SEMICOLON; }
","                        { return T_COMMA; }

[0-9]+                     { yylval.ival = atoi(yytext); return T_NUMBER; }
[a-zA-Z_][a-zA-Z0-9_]*     { yylval.sval = new string(yytext); return T_ID; }
\"([^\\\"]|\\.)*\"         { yylval.sval = new string(yytext); return T_STRING; }

[\n]                       { lineno++; tokenpos = 1; }
[\t\r\a\v\b ]+             { tokenpos += yyleng; /* skip whitespace */ }
.                          { tokenpos += yyleng; return yytext[0]; }

%%

int yyerror(const char *s) {
  cerr << lineno << ": " << s << " at char " << tokenpos << endl;
  return 1;
}
