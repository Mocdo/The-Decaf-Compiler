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
// the calls to TheContext in the init above and in the
// following code ensures that we are incrementally generating
// instructions in the right order

// dummy main function
// WARNING: this is not how you should implement code generation
// for the main function!
// You should write the codegen for the main method as 
// part of the codegen for method declarations (MethodDecl)
static llvm::Function *TheFunction = 0;

// we have to create a main function 
llvm::Function *gen_main_def() {
  // create the top-level definition for main
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::IntegerType::get(TheContext, 32), false);
  llvm::Function *TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule);
  if (TheFunction == 0) {
    throw runtime_error("empty function block"); 
  }
  // Create a new basic block which contains a sequence of LLVM instructions
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  // All subsequent calls to IRBuilder will place instructions in this location
  Builder.SetInsertPoint(BB);
  return TheFunction;
}

#include "decafcomp.cc"

%}

%union{
    class decafAST *ast;
    std::string *sval;
    int number;
    int decaftype;
    void* not_Ty;
 }

%token T_AND
%token T_ASSIGN
%token T_BOOLTYPE
%token T_BREAK
%token <number> T_CHARCONSTANT
%token T_COMMA
%token T_COMMENT
%token T_CONTINUE
%token T_DIV
%token T_DOT
%token T_ELSE
%token T_EQ
%token T_EXTERN
%token <number> T_FALSE
%token T_FOR
%token T_FUNC
%token T_GEQ
%token T_GT
%token <sval> T_ID
%token T_IF
%token <number> T_INTCONSTANT
%token T_INTTYPE
%token T_LCB
%token T_LEFTSHIFT
%token T_LEQ
%token T_LPAREN
%token T_LSB
%token T_LT
%token T_MINUS
%token T_MOD
%token T_MULT
%token T_NEQ
%token T_NOT
%token T_NULL
%token T_OR
%token T_PACKAGE
%token T_PLUS
%token T_RCB
%token T_RETURN
%token T_RIGHTSHIFT
%token T_RPAREN
%token T_RSB
%token T_SEMICOLON
%token <sval> T_STRINGCONSTANT
%token T_STRINGTYPE
%token <number> T_TRUE
%token T_VAR
%token T_VOID
%token T_WHILE
%token T_WHITESPACE

%type <decaftype> type method_type extern_type
%type <ast> rvalue expr constant bool_constant method_call method_arg method_arg_list assign assign_comma_list
%type <ast> block method_block statement statement_list var_decl_list var_decl var_list param_list param_comma_list 
%type <ast> method_decl method_decl_list field_decl_list field_decl field_list extern_type_list extern_defn
%type <ast> extern_list decafpackage

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GEQ T_GT
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_NOT
%right UMINUS

%type <not_Ty> begin_block end_block

%%


start: program

  /* Program = Externs package identifier "{" FieldDecls MethodDecls "}" . */
program: extern_list decafpackage
    { 
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
		if (printAST) {
			cout << getString(prog) << endl;
		}
        try {
            prog->Codegen();
        } 
        catch (std::runtime_error &e) {
            cout << "semantic error: " << e.what() << endl;
            //cout << prog->str() << endl; 
            exit(EXIT_FAILURE);
        }
        delete prog;
    }

  /* Externs    = { ExternDefn } . */
extern_list: extern_list extern_defn
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /* ExternDefn = extern func identifier "(" [ { ExternType }+, ] ")" MethodType ";" . */
extern_defn: T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    { $$ = new ExternAST((decafType)$7, *$3, (TypedSymbolListAST *)$5); delete $3; }
    | T_EXTERN T_FUNC T_ID T_LPAREN T_RPAREN method_type T_SEMICOLON
    { $$ = new ExternAST((decafType)$6, *$3, NULL); delete $3; }
    ;

extern_type_list: extern_type
    { $$ = new TypedSymbolListAST(string(""), (decafType)$1); }
    | extern_type T_COMMA extern_type_list
    { 
        TypedSymbolListAST *tlist = (TypedSymbolListAST *)$3; 
        tlist->push_front(string(""), (decafType)$1); 
        $$ = tlist;
    }
    ;

  /* ExternType = ( string | MethodType ) . */
extern_type: T_STRINGTYPE
    { $$ = stringTy; }
    | type
    { $$ = $1; }
    ;

  /* Program = Externs package identifier "{" FieldDecls MethodDecls "}" . */
decafpackage: T_PACKAGE T_ID begin_block field_decl_list method_decl_list end_block
    { $$ = new PackageAST(*$2, (decafStmtList *)$4, (decafStmtList *)$5); delete $2; }
    | T_PACKAGE T_ID begin_block field_decl_list end_block
    { $$ = new PackageAST(*$2, (decafStmtList *)$4, new decafStmtList()); delete $2; }
    ;

  /* FieldDecls = { FieldDecl } . */
field_decl_list: field_decl_list field_decl
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /*
  FieldDecl  = var { identifier }+, Type ";" .
  FieldDecl  = var { identifier }+, ArrayType ";" .
  FieldDecl  = var identifier Type "=" Constant ";" .
  */
field_decl: T_VAR field_list T_SEMICOLON
    { $$ = $2; }
    | T_VAR T_ID type T_ASSIGN constant T_SEMICOLON
    { $$ = new AssignGlobalVarAST((decafType)$3, *$2, $5); delete $2; }
    ;

field_list: T_ID T_COMMA field_list
    { FieldDeclListAST *flist = (FieldDeclListAST *)$3; flist->new_sym(*$1); $$ = flist; delete $1; }
    | T_ID type 
    { $$ = new FieldDeclListAST(*$1, (decafType)$2); delete $1; }
    | T_ID T_LSB T_INTCONSTANT T_RSB type
    { $$ = new FieldDeclListAST(*$1, (decafType)$5, $3); delete $1; }
    ;

  /* MethodDecls = { MethodDecl } . */
method_decl_list: method_decl_list method_decl 
    { decafStmtList *slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | method_decl
    { decafStmtList *slist = new decafStmtList(); slist->push_back($1); $$ = slist; }
    ;

  /* MethodDecl  = func identifier "(" [ { identifier Type }+, ] ")" MethodType Block . */
method_decl: T_FUNC T_ID T_LPAREN param_list T_RPAREN method_type method_block
    { $$ = new MethodDeclAST((decafType)$6, *$2, (TypedSymbolListAST *)$4, (MethodBlockAST *)$7); delete $2; }
    ;

  /* MethodType = ( void | Type ) . */
method_type: T_VOID
    { $$ = voidTy; }
    | type
    { $$ = $1; }
    ;

param_list: param_comma_list
    { $$ = $1; }
    | /* empty */
    { $$ = NULL; }
    ;

param_comma_list: T_ID type T_COMMA param_comma_list
    { 
        TypedSymbolListAST *tlist = (TypedSymbolListAST *)$4; 
        tlist->push_front(*$1, (decafType)$2); 
        $$ = tlist;
        delete $1;
    }
    | T_ID type
    { $$ = new TypedSymbolListAST(*$1, (decafType)$2); delete $1; }
    ;

  /* Type = ( int | bool ) . */
type: T_INTTYPE
    { $$ = intTy; }
    | T_BOOLTYPE
    { $$ = boolTy; }
    ;

  /* Block = "{" VarDecls Statements "}" . */
block: begin_block var_decl_list statement_list end_block
    { $$ = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3); }

method_block: begin_block var_decl_list statement_list end_block
    { $$ = new MethodBlockAST((decafStmtList *)$2, (decafStmtList *)$3); }


begin_block: T_LCB {$$ = NULL;}
end_block: T_RCB   {$$ =NULL;}

  /* VarDecls = { VarDecl } .  */
var_decl_list: var_decl var_decl_list
    { decafStmtList *slist = (decafStmtList *)$2; slist->push_front($1); $$ = slist; }
    | /* empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /* VarDecl  = var { identifier }+, Type ";" . */
var_decl: T_VAR var_list T_SEMICOLON
    { $$ = $2; }

var_list: T_ID T_COMMA var_list
    { 
        TypedSymbolListAST *tlist = (TypedSymbolListAST *)$3; 
        tlist->new_sym(*$1); 
        $$ = tlist;
        delete $1;
    }
    | T_ID type
    { $$ = new TypedSymbolListAST(*$1, (decafType)$2); delete $1; }
    ;

  /* Statements = { Statement } . */
statement_list: statement statement_list
    { decafStmtList *slist = (decafStmtList *)$2; slist->push_front($1); $$ = slist; }
    | /* empty */ 
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    ;

  /*
  Statement = Block .
  Statement  = MethodCall ";" .
  Statement = Assign ";" .
  Statement = if "(" Expr ")" Block [ else Block ] .
  Statement =  while "(" Expr ")" Block .
  Statement = for "(" { Assign }+, ";" Expr ";" { Assign }+, ")" Block .
  Statement = return [ "(" [ Expr ] ")" ] ";" .
  Statement = break ";" .
  Statement = continue ";" .
  */
statement: assign T_SEMICOLON
    { $$ = $1; }
    | method_call T_SEMICOLON
    { $$ = $1; }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    { $$ = new IfStmtAST($3, (BlockAST *)$5, (BlockAST *)$7); }
    | T_IF T_LPAREN expr T_RPAREN block 
    { $$ = new IfStmtAST($3, (BlockAST *)$5, NULL); }
    | T_WHILE T_LPAREN expr T_RPAREN block
    { $$ = new WhileStmtAST($3, (BlockAST *)$5); }
    | T_FOR T_LPAREN assign_comma_list T_SEMICOLON expr T_SEMICOLON assign_comma_list T_RPAREN block
    { $$ = new ForStmtAST((decafStmtList *)$3, $5, (decafStmtList *)$7, (BlockAST *)$9); }
    | T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON
    { $$ = new ReturnStmtAST($3); }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON
    { $$ = new ReturnStmtAST(NULL); }
    | T_RETURN T_SEMICOLON
    { $$ = new ReturnStmtAST(NULL); }
    | T_BREAK T_SEMICOLON
    { $$ = new BreakStmtAST(); }
    | T_CONTINUE T_SEMICOLON
    { $$ = new ContinueStmtAST(); }
    | block
    { $$ = $1; }
    ;

  /* 
  Assign    = Lvalue "=" Expr .
  Lvalue    = identifier | identifier "[" Expr "]" .
  */
assign: T_ID T_ASSIGN expr
    { $$ = new AssignVarAST(*$1, $3); delete $1; }
    | T_ID T_LSB expr T_RSB T_ASSIGN expr
    { $$ = new AssignArrayLocAST(*$1, $3, $6); delete $1; }
    ;

  /* MethodCall = identifier "(" [ { MethodArg }+, ] ")" . */
method_call: T_ID T_LPAREN method_arg_list T_RPAREN
    { $$ = new MethodCallAST(*$1, (decafStmtList *)$3); delete $1; }
    | T_ID T_LPAREN T_RPAREN
    { $$ = new MethodCallAST(*$1, (decafStmtList *)NULL); delete $1; }
    ;

method_arg_list: method_arg
    { decafStmtList *slist = new decafStmtList(); slist->push_front($1); $$ = slist; }
    | method_arg T_COMMA method_arg_list
    { decafStmtList *slist = (decafStmtList *)$3; slist->push_front($1); $$ = slist; }
    ;

  /* MethodArg  = Expr | string_lit . */
method_arg: expr
    { $$ = $1; }
    | T_STRINGCONSTANT
    { $$ = new StringConstAST(*$1); delete $1; }
    ;
   
assign_comma_list: assign
    { decafStmtList *slist = new decafStmtList(); slist->push_front($1); $$ = slist; }
    | assign T_COMMA assign_comma_list
    { decafStmtList *slist = (decafStmtList *)$3; slist->push_front($1); $$ = slist; }
    ;

  /*
  Expr = identifier .
  Expr = MethodCall .
  Expr = Constant .
  Expr = Expr BinaryOperator Expr .
  Expr = UnaryOperator Expr .
  Expr = "(" Expr ")" .
  Expr = identifier "[" Expr "]" .
  */
rvalue: T_ID
    { $$ = new VariableExprAST(*$1); delete $1; }
    | T_ID T_LSB expr T_RSB
    { $$ = new ArrayLocExprAST(*$1, $3); delete $1; }
    ;

expr: rvalue
    { $$ = $1; }
    | method_call
    { $$ = $1; }
    | constant
    { $$ = $1; }
    | expr T_PLUS expr
    { $$ = new BinaryExprAST(T_PLUS, $1, $3); }
    | expr T_MINUS expr
    { $$ = new BinaryExprAST(T_MINUS, $1, $3); }
    | expr T_MULT expr
    { $$ = new BinaryExprAST(T_MULT, $1, $3); }
    | expr T_DIV expr
    { $$ = new BinaryExprAST(T_DIV, $1, $3); }
    | expr T_LEFTSHIFT expr
    { $$ = new BinaryExprAST(T_LEFTSHIFT, $1, $3); }
    | expr T_RIGHTSHIFT expr
    { $$ = new BinaryExprAST(T_RIGHTSHIFT, $1, $3); }
    | expr T_MOD expr
    { $$ = new BinaryExprAST(T_MOD, $1, $3); }
    | expr T_LT expr
    { $$ = new BinaryExprAST(T_LT, $1, $3); }
    | expr T_GT expr
    { $$ = new BinaryExprAST(T_GT, $1, $3); }
    | expr T_LEQ expr
    { $$ = new BinaryExprAST(T_LEQ, $1, $3); }
    | expr T_GEQ expr
    { $$ = new BinaryExprAST(T_GEQ, $1, $3); }
    | expr T_EQ expr
    { $$ = new BinaryExprAST(T_EQ, $1, $3); }
    | expr T_NEQ expr
    { $$ = new BinaryExprAST(T_NEQ, $1, $3); }
    | expr T_AND expr
    { $$ = new BinaryExprAST(T_AND, $1, $3); }
    | expr T_OR expr
    { $$ = new BinaryExprAST(T_OR, $1, $3); }
    | T_MINUS expr %prec UMINUS 
    { $$ = new UnaryExprAST(T_MINUS, $2); }
    | T_NOT expr
    { $$ = new UnaryExprAST(T_NOT, $2); }
    | T_LPAREN expr T_RPAREN
    { $$ = $2; }
    ;

  /* Constant = ( int_lit | char_lit | BoolConstant ) . */
constant: T_INTCONSTANT
    { $$ = new NumberExprAST($1); }
    | T_CHARCONSTANT
    { $$ = new NumberExprAST($1); }
    | bool_constant
    { $$ = $1; }
    ;

  /* BoolConstant = ( true | false ) . */
bool_constant: T_TRUE
    { $$ = new BoolExprAST(true); }
    | T_FALSE 
    { $$ = new BoolExprAST(false); }
    ;

%%


int main() {
  // initialize LLVM
  llvm::LLVMContext &Context = TheContext;
  // Make the module, which holds all the code.
  TheModule = new llvm::Module("Test", Context);

  // set up symbol table
push_new_symbol_table();

  // set up dummy main function
  //TheFunction = gen_main_def();
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  // remove symbol table
pop_head_symbol_table();
  // Finish off the main function. (see the WARNING above)

  // return 0 from main, which is EXIT_SUCCESS
  //Builder.CreateRet(llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)));
  // Validate the generated code, checking for consistency.
  //verifyFunction(*TheFunction);
  // Print out all of the generated code to stderr
  TheModule->print(llvm::errs(), nullptr);
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
};

