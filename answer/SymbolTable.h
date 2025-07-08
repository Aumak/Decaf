//===----------------------------------------------------------------------===//
// SymbolTable.h
// Fully implemented Symbol Table for Step 1 & 2
// Supports variable scoping, shadowing, and LLVM AllocaInst linkage
//===----------------------------------------------------------------------===//

#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <string>
#include <map>
#include <list>
#include <iostream>
#include <llvm/IR/Instructions.h>  // For llvm::AllocaInst

using namespace std;

/// ============================
/// Descriptor - Metadata for each variable
/// ============================
struct Descriptor {
    string type;
    int decl_line;
    llvm::AllocaInst* alloca_inst;

    Descriptor(const string& t, int l, llvm::AllocaInst* alloc = nullptr)
        : type(t), decl_line(l), alloca_inst(alloc) {}
};

typedef map<string, Descriptor*> SymbolTable;
typedef list<SymbolTable> SymbolTableList;

/// ============================
/// SymbolTableManager - Scoped Symbol Table
/// ============================
class SymbolTableManager {
private:
    SymbolTableList symtbl;
public:
    ~SymbolTableManager() {
        for (auto& tbl : symtbl) {
            for (auto& entry : tbl) delete entry.second;
        }
    }

    void enterScope() {
        symtbl.push_front(SymbolTable());
    }

    void exitScope() {
        if (!symtbl.empty()) symtbl.pop_front();
    }

    void defineVar(const string& name, const string& type, int line) {
        if (symtbl.empty()) enterScope();

        if (symtbl.front().count(name)) {
            cerr << "semantic error: variable '" << name << "' already declared in this scope" << endl;
            exit(EXIT_FAILURE);
        }

        Descriptor* d = new Descriptor(type, line);
        symtbl.front()[name] = d;

        cerr << "defined variable: " << name
             << ", with type: " << type
             << ", on line number: " << line << endl;
    }

    void defineVar(const string& name, const string& type, int line, llvm::AllocaInst* alloc) {
        if (symtbl.empty()) enterScope();

        if (symtbl.front().count(name)) {
            cerr << "semantic error: variable '" << name << "' already declared in this scope" << endl;
            exit(EXIT_FAILURE);
        }

        Descriptor* d = new Descriptor(type, line, alloc);
        symtbl.front()[name] = d;
    }

    Descriptor* lookup(const string& name) {
        for (auto& scope : symtbl) {
            auto it = scope.find(name);
            if (it != scope.end()) {
                return it->second;
            }
        }
        return nullptr;
    }

    llvm::AllocaInst* getVar(const string& name) {
        Descriptor* d = lookup(name);
        return d ? d->alloca_inst : nullptr;
    }
};

#endif // SYMBOL_TABLE_H
