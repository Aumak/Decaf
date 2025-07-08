#ifndef _DECAF_DEFS_H_
#define _DECAF_DEFS_H_

// LLVM Core Includes
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"

// C Standard Libraries
#include <cstdio>
#include <cstdlib>
#include <cstring>

// C++ Standard Libraries
#include <string>
#include <stdexcept>
#include <vector>
#include <list>
#include <map>
#include <iostream>
#include <sstream>

// External Variables (shared between Flex/Bison)
extern int lineno;
extern int tokenpos;

// Forward declaration of AST base class
class decafAST;

// Flex/Bison Prototypes (C linkage)
extern "C" {
    int yyparse(void);
    int yylex(void);
    int yywrap(void);
    int yyerror(const char *msg);
}

#endif // _DECAF_DEFS_H_
