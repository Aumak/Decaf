Decaf Compiler (decafexpr)
------------------
A lightweight compiler for a simplified language Decaf, generates LLVM Intermediate Representation (IR) code.

Project Structure
decafexpr.y — Bison/Yacc grammar rules
decafexpr.lex — Flex/Lex lexer rules
default.cc — AST and LLVM Codegen logic
default-defs.h — Common definitions and includes
SymbolTable.h — Symbol table with scoping and shadowing support
decaf-stdlib.c — Decaf standard library functions
makefile.csil — Build system to generate the compiler
testcases/ — Test inputs
check.py — Auto-grader script to check correctness

How to Build
make -f makefile.csil decafexpr

How to Run
To compile and test a Decaf file:
python3 zipout.py- generate output.zip for check and comparison
To check your implementation:
python3 check.py
You should see a score summary based on correctness.

Tips
Edit decafexpr.lex to ensure all keywords and symbols are tokenized correctly.
You can peek at dev_llvm/ to understand what the LLVM output should roughly look like.
Each AST class in default.cc has a .str() and Codegen() to help with debugging and generation.

Run into an error? 
Try:
Checking for missing tokens in your lexer
Ensuring variable declarations exist in the symbol table
Using print statements in Codegen() for trace debugging

Make it so.

