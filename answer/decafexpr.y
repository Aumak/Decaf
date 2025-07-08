%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include "default-defs.h"

int yylex(void);
int yyerror(char *);

// print AST?
bool printAST = false;

using namespace std;

// this global variable contains all the generated code
static llvm::Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);

static llvm::Function *TheFunction = 0;

llvm::Function *gen_main_def() {
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::IntegerType::get(TheContext, 32), false);
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule);
  if (TheFunction == 0) {
    throw runtime_error("empty function block");
  }
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);
  return TheFunction;
}

#include "default.cc"
%}

// ==== Precedence and Associativity ====
%left T_OR
%left T_AND
%left T_EQEQ T_NEQ
%left T_LT T_GT T_LEQ T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD
%right T_NOT

// ==== Union for Bison values ====
%union {
    class decafAST *ast;
    std::string *sval;
    int ival;
    std::list<decafAST*> *astlist;
}

%token T_PACKAGE T_VAR T_LCB T_RCB
%token T_INT T_BOOL T_VOID
%token T_RETURN T_IF T_ELSE T_WHILE T_FOR T_BREAK T_CONTINUE
%token T_EXTERN T_FUNC
%token T_TRUE T_FALSE

%token T_ASSIGN T_PLUS T_MINUS T_MULT T_DIV T_MOD
%token T_EQEQ T_NEQ T_LT T_GT T_LEQ T_GEQ
%token T_AND T_OR T_NOT

%token T_LPAREN T_RPAREN T_SEMICOLON T_COMMA

%token <sval> T_ID T_STRING
%token <ival> T_NUMBER

// ==== Non-terminal types ====
%type <ast> program extern_decl extern_list decafpackage
%type <ast> method_decl method_list method_block var_decl_list var_decl statement_list statement
%type <ast> expr assign_stmt return_stmt
%type <sval> binop
%type <ast> number bool_val identifier
%type <astlist> expr_list

%%

start:
    program
;

program:
    extern_list decafpackage
    {
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2);
        if (printAST) cout << getString(prog) << endl;
        try { prog->Codegen(); }
        catch (std::runtime_error &e) {
            cout << "semantic error: " << e.what() << endl;
            exit(EXIT_FAILURE);
        }
        delete prog;
    }
;

extern_list:
      /* empty */                   { $$ = new decafStmtList(); }
    | extern_decl extern_list       { ((decafStmtList *)$2)->push_front($1); $$ = $2; }
;

extern_decl:
    T_EXTERN T_FUNC T_VOID T_ID T_LPAREN T_INT T_RPAREN T_SEMICOLON {
        std::vector<std::string> args;
        args.push_back("int");
        $$ = new ExternFunctionAST(*$4, args, "void");
        delete $4;
    }
;

decafpackage:
      T_PACKAGE T_ID T_LCB T_RCB {
        $$ = new PackageAST(*$2, new decafStmtList(), new decafStmtList()); delete $2;
    }
    | T_PACKAGE T_ID T_LCB method_list T_RCB {
        $$ = new PackageAST(*$2, new decafStmtList(), (decafStmtList *)$4); delete $2;
    }
;

method_list:
      method_decl                  { decafStmtList *l = new decafStmtList(); l->push_back($1); $$ = l; }
    | method_decl method_list     { ((decafStmtList *)$2)->push_front($1); $$ = $2; }
;

method_decl:
    T_FUNC T_ID T_LPAREN T_RPAREN T_LCB method_block T_RCB {
        $$ = new MethodDeclAST(*$2, "int", {}, (decafStmtList *)$6); delete $2;
    }
;

method_block:
    var_decl_list statement_list {
        decafStmtList *combined = (decafStmtList *)$1;
        ((decafStmtList *)$2)->push_front(combined);
        $$ = $2;
    }
;

var_decl_list:
      var_decl var_decl_list {
        ((decafStmtList *)$2)->push_front($1); $$ = $2;
      }
    | /* empty */ {
        $$ = new decafStmtList();
      }
;

var_decl:
    T_INT T_ID T_SEMICOLON {
        $$ = new VarDeclAST(*$2, "int"); delete $2;
    }
;

statement_list:
      statement                 { decafStmtList *l = new decafStmtList(); l->push_back($1); $$ = l; }
    | statement statement_list { ((decafStmtList *)$2)->push_front($1); $$ = $2; }
;

statement:
      assign_stmt
    | return_stmt
    | expr T_SEMICOLON       { $$ = $1; }  // allows function calls like print_int(x);
;

assign_stmt:
    T_ID T_ASSIGN expr T_SEMICOLON {
        $$ = new AssignStmtAST(*$1, $3); delete $1;
    }
;

return_stmt:
    T_RETURN expr T_SEMICOLON {
        $$ = new ReturnStmtAST($2);
    }
;

expr:
      number
    | bool_val
    | identifier
    | expr binop expr              { $$ = new BinaryExprAST(*$2, $1, $3); delete $2; }
    | T_MINUS expr %prec T_MINUS  { $$ = new UnaryExprAST("-", $2); }
    | T_NOT expr %prec T_NOT      { $$ = new UnaryExprAST("!", $2); }
    | T_ID T_LPAREN T_RPAREN      { $$ = new MethodCallAST(*$1, {}); delete $1; }
    | T_ID T_LPAREN expr_list T_RPAREN {
        $$ = new MethodCallAST(*$1, *$3); delete $3; delete $1;
    }
;

expr_list:
      expr { $$ = new list<decafAST*>(); $$->push_back($1); }
    | expr T_COMMA expr_list { $3->push_front($1); $$ = $3; }
;

binop:
      T_PLUS   { $$ = new string("+"); }
    | T_MINUS  { $$ = new string("-"); }
    | T_MULT   { $$ = new string("*"); }
    | T_DIV    { $$ = new string("/"); }
    | T_MOD    { $$ = new string("%"); }
    | T_EQEQ   { $$ = new string("=="); }
    | T_NEQ    { $$ = new string("!="); }
    | T_LT     { $$ = new string("<"); }
    | T_GT     { $$ = new string(">"); }
    | T_LEQ    { $$ = new string("<="); }
    | T_GEQ    { $$ = new string(">="); }
    | T_AND    { $$ = new string("&&"); }
    | T_OR     { $$ = new string("||"); }
;

number:
    T_NUMBER { $$ = new NumberExprAST($1); }
;

bool_val:
      T_TRUE  { $$ = new BoolExprAST(true); }
    | T_FALSE { $$ = new BoolExprAST(false); }
;

identifier:
    T_ID { $$ = new VariableExprAST(*$1); delete $1; }
;

%%

int main() {
  llvm::LLVMContext &Context = TheContext;
  TheModule = new llvm::Module("Test", Context);
  TheFunction = gen_main_def();

  int retval = yyparse();

  Builder.CreateRet(llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)));
  verifyFunction(*TheFunction);
  TheModule->print(llvm::errs(), nullptr);

  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}
