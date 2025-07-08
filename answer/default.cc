#include "default-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>
#include "SymbolTable.h"

SymbolTableManager symtbl;  // global declaration

#ifndef YYTOKENTYPE
#include "decafexpr.tab.h"
#endif

using namespace std;

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value *Codegen() = 0;
};

string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str(); 
    }   
    if (s.empty()) {
        s = string("None");
    }   
    return s;
}

template <class T>
llvm::Value *listCodegen(list<T> vec) {
	llvm::Value *val = NULL;
	for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
		llvm::Value *j = (*i)->Codegen();
		if (j != NULL) { val = j; }
	}	
	return val;
}

/// ExternFunctionAST - for extern function declarations like print_int(int)
class ExternFunctionAST : public decafAST {
    string Name;
    vector<string> ParamTypes;
    string RetType;
public:
    ExternFunctionAST(string name, vector<string> paramTypes, string retType)
        : Name(name), ParamTypes(paramTypes), RetType(retType) {}

    string str() override {
        string params = "";
        for (auto &t : ParamTypes) params += (params.empty() ? "" : ",") + t;
        return "ExternFunction(" + Name + "," + params + "," + RetType + ")";
    }

    llvm::Value *Codegen() override {
        vector<llvm::Type *> types;
        for (auto &t : ParamTypes) {
            types.push_back(t == "int" ? llvm::Type::getInt32Ty(TheContext)
                                       : llvm::Type::getInt1Ty(TheContext));
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(
            (RetType == "int") ? llvm::Type::getInt32Ty(TheContext)
                               : llvm::Type::getVoidTy(TheContext),
            types, false);
        llvm::Function *F = llvm::Function::Create(FT,
            llvm::Function::ExternalLinkage, Name, TheModule);
        return F;
    }
};

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
public:
	decafStmtList() {}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) { 
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	string str() { return commaList<class decafAST *>(stmts); }
	llvm::Value *Codegen() { 
		return listCodegen<decafAST *>(stmts); 
	}
};

class PackageAST : public decafAST {
	string Name;
	decafStmtList *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist) 
		: Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	~PackageAST() { 
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}
	string str() { 
		return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
	}
	llvm::Value *Codegen() { 
		llvm::Value *val = NULL;
		TheModule->setModuleIdentifier(llvm::StringRef(Name)); 
		if (NULL != FieldDeclList) {
			val = FieldDeclList->Codegen();
		}
		if (NULL != MethodDeclList) {
			val = MethodDeclList->Codegen();
		} 
		return val; 
	}
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
	llvm::Value *Codegen() { 
		llvm::Value *val = NULL;
		if (NULL != ExternList) {
			val = ExternList->Codegen();
		}
		if (NULL != PackageDef) {
			val = PackageDef->Codegen();
		} else {
			throw runtime_error("no package definition in decaf program");
		}
		return val; 
	}
};

class UnaryExprAST : public decafAST {
    string Op;
    decafAST *Operand;
public:
    UnaryExprAST(string op, decafAST *expr) : Op(op), Operand(expr) {}
    ~UnaryExprAST() { delete Operand; }
    string str() override {
        return "UnaryExpr(" + Op + "," + getString(Operand) + ")";
    }
    llvm::Value *Codegen() override {
        llvm::Value *V = Operand->Codegen();
        if (!V) return nullptr;

        if (Op == "-") return Builder.CreateNeg(V, "negtmp");
        else if (Op == "!") return Builder.CreateNot(V, "nottmp");
        else throw runtime_error("unknown unary operator: " + Op);
    }
};
/// NumberExprAST - integer constants
class NumberExprAST : public decafAST {
    int Val;
public:
    NumberExprAST(int v) : Val(v) {}
    string str() override {
        return "NumberExpr(" + to_string(Val) + ")";
    }
    llvm::Value *Codegen() override {
        return llvm::ConstantInt::get(TheContext, llvm::APInt(32, Val));
    }
};

/// BoolExprAST - boolean constants
class BoolExprAST : public decafAST {
    bool Val;
public:
    BoolExprAST(bool v) : Val(v) {}
    string str() override {
        return string("BoolExpr(") + (Val ? "true" : "false") + ")";
    }
    llvm::Value *Codegen() override {
        return llvm::ConstantInt::get(TheContext, llvm::APInt(1, Val));
    }
};

/// VariableExprAST - variable reference
class VariableExprAST : public decafAST {
    string Name;
public:
    VariableExprAST(string name) : Name(name) {}
    string str() override {
        return "VariableExpr(" + Name + ")";
    }
    llvm::Value *Codegen() override {
        llvm::AllocaInst *alloca = symtbl.getVar(Name);
        if (!alloca)
            throw runtime_error("undefined variable: " + Name);
        return Builder.CreateLoad(alloca->getAllocatedType(), alloca, Name);
    }
};

/// ReturnStmtAST
class ReturnStmtAST : public decafAST {
    decafAST *Expr;
public:
    ReturnStmtAST(decafAST *e) : Expr(e) {}
    ~ReturnStmtAST() { delete Expr; }
    string str() override {
        return string("ReturnStmt(") + getString(Expr) + ")";
    }
    llvm::Value *Codegen() override {
        llvm::Value *retVal = Expr ? Expr->Codegen() : llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
        return Builder.CreateRet(retVal);
    }
};

/// AssignStmtAST - assignment statement
class AssignStmtAST : public decafAST {
    string Name;
    decafAST *Expr;
public:
    AssignStmtAST(string name, decafAST *e) : Name(name), Expr(e) {}
    ~AssignStmtAST() { delete Expr; }
    string str() override {
        return "AssignStmt(" + Name + "," + getString(Expr) + ")";
    }
    llvm::Value *Codegen() override {
        llvm::AllocaInst *alloca = symtbl.getVar(Name);
        if (!alloca)
            throw runtime_error("undefined variable in assignment: " + Name);
        llvm::Value *val = Expr->Codegen();
        Builder.CreateStore(val, alloca);
        return val;
    }
};

/// BinaryExprAST - binary operations
class BinaryExprAST : public decafAST {
    string Op;
    decafAST *LHS, *RHS;
public:
    BinaryExprAST(string op, decafAST *lhs, decafAST *rhs) : Op(op), LHS(lhs), RHS(rhs) {}
    ~BinaryExprAST() { delete LHS; delete RHS; }
    string str() override {
        return "BinaryExpr(" + Op + "," + getString(LHS) + "," + getString(RHS) + ")";
    }
    llvm::Value *Codegen() override {
        llvm::Value *L = LHS->Codegen();
        llvm::Value *R = RHS->Codegen();
        if (!L || !R) return nullptr;

        if (Op == "+") return Builder.CreateAdd(L, R);
        else if (Op == "-") return Builder.CreateSub(L, R);
        else if (Op == "*") return Builder.CreateMul(L, R);
        else if (Op == "/") return Builder.CreateSDiv(L, R);
        else if (Op == "%") return Builder.CreateSRem(L, R);
        else if (Op == "<") return Builder.CreateICmpSLT(L, R);
        else if (Op == ">") return Builder.CreateICmpSGT(L, R);
        else if (Op == "<=") return Builder.CreateICmpSLE(L, R);
        else if (Op == ">=") return Builder.CreateICmpSGE(L, R);
        else if (Op == "==") return Builder.CreateICmpEQ(L, R);
        else if (Op == "!=") return Builder.CreateICmpNE(L, R);
        else if (Op == "&&") return Builder.CreateAnd(L, R);
        else if (Op == "||") return Builder.CreateOr(L, R);

        throw runtime_error("unknown binary operator: " + Op);
    }
};

/// MethodDeclAST - method definition
class MethodDeclAST : public decafAST {
    string Name;
    string RetType;
    list<pair<string, string>> Params;
    decafStmtList *Body;
public:
    MethodDeclAST(string name, string rettype, list<pair<string, string>> params, decafStmtList *body)
        : Name(name), RetType(rettype), Params(params), Body(body) {}
    ~MethodDeclAST() { delete Body; }
    string str() override {
        string p = "";
        for (auto& [n, t] : Params) p += (p.empty() ? "" : ",") + t + " " + n;
        return "Method(" + Name + "," + RetType + "," + p + "," + getString(Body) + ")";
    }
    llvm::Value *Codegen() override {
        vector<llvm::Type *> args;
        for (auto& p : Params) {
            args.push_back(p.second == "int" ? llvm::Type::getInt32Ty(TheContext) : llvm::Type::getInt1Ty(TheContext));
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(
            (RetType == "int") ? llvm::Type::getInt32Ty(TheContext) : llvm::Type::getVoidTy(TheContext),
            args, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule);

        symtbl.enterScope();
        unsigned idx = 0;
        for (auto &arg : F->args()) {
            auto param = next(Params.begin(), idx);
            llvm::AllocaInst *Alloca = Builder.CreateAlloca(arg.getType(), nullptr, param->first);
            Builder.CreateStore(&arg, Alloca);
            symtbl.defineVar(param->first, param->second, -1, Alloca);
            idx++;
        }

        llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
        Builder.SetInsertPoint(BB);

        Body->Codegen();
        if (RetType == "int") Builder.CreateRet(llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)));
        else Builder.CreateRetVoid();

        symtbl.exitScope();
        return F;
    }
};
class MethodCallAST : public decafAST {
    string Callee;
    list<decafAST*> Args;
public:
    MethodCallAST(string name, list<decafAST*> args) : Callee(name), Args(args) {}
    ~MethodCallAST() {
        for (auto *a : Args) delete a;
    }
    string str() override {
        string arglist = commaList<decafAST*>(Args);
        return "MethodCall(" + Callee + "," + arglist + ")";
    }
    llvm::Value *Codegen() override {
        llvm::Function *CalleeF = TheModule->getFunction(Callee);
        if (!CalleeF)
            throw runtime_error("Call to undefined function: " + Callee);

        std::vector<llvm::Value*> ArgsV;
        for (auto *arg : Args) {
            llvm::Value *argVal = arg->Codegen();
            if (!argVal) return nullptr;
            ArgsV.push_back(argVal);
        }

        return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
    }
};

class VarDeclAST : public decafAST {
    string Name;
    string Type;
public:
    VarDeclAST(string name, string type) : Name(name), Type(type) {}
    string str() override {
        return "VarDecl(" + Type + " " + Name + ")";
    }
    llvm::Value *Codegen() override {
        llvm::Type *Ty = Type == "int" ? llvm::Type::getInt32Ty(TheContext) : llvm::Type::getInt1Ty(TheContext);
        llvm::AllocaInst *Alloca = Builder.CreateAlloca(Ty, nullptr, Name);
        symtbl.defineVar(Name, Type, -1, Alloca);
        return Alloca;
    }
};


