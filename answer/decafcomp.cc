
#include "default-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>


#include <utility>      // std::pair, std::make_pair
#include <string>       // std::string
#include <map>          // std::map

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include <stdexcept>

#include <tuple>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

using namespace std;



///////////////symbol table
/*
struct descriptor{
    string type;      //int char bool string
    int i_v;
    char c_v;
    bool  b_v;
    string s_v;
    bool function;
    bool global;

}*/




//symbol table


//typedef std::tuple<void*, void*,void*> descriptor;		
//variable: <type, alloc, NULL>
//array:    <arrayType, GlobalVar, elementType>
//function: <returnType, Func, ArgTypeList>
class descriptor{
public:
  void* first;
  void* second;
  void* third;
  std::vector<llvm::Type *> fourth;
  descriptor(void* a, void* b){
    first=a;
    second=b;
    third=NULL;
  }
  descriptor(void* a, void* b, void* c){
    first=a;
    second=b;
    third=c;
  }
  descriptor(void* a, void* b, std::vector<llvm::Type *> c){
    first=a;
    second=b;
    fourth=c;
  }
};
typedef map<string, descriptor* > symbol_table;
typedef list<symbol_table > symbol_table_list;
symbol_table_list symtbl;
descriptor* access_symtbl(string ident) {
    for (auto i : symtbl) {
        auto find_ident = i.find(ident);
        if (find_ident != i.end()) {
            return find_ident->second;
        }
    }
    return NULL;
}

descriptor* access_block_symtbl(string ident) {
    symbol_table i = symtbl.front();
        auto find_ident = i.find(ident);
        if (find_ident != i.end()) {
            return find_ident->second;
        }
    return NULL;
}

void insert_symbol(string name, descriptor* D){
    symbol_table table = symtbl.front();
    symtbl.pop_front();
    table[name] = D;
    symtbl.push_front(table);
}


void push_new_symbol_table(){
    symbol_table new_one;
    symtbl.push_front(new_one);

}

void pop_head_symbol_table(){
    symtbl.pop_front();

}


list<llvm::BasicBlock *> contine_dest_list;
list<llvm::BasicBlock *> break_dest_list;






typedef enum { voidTy, intTy, boolTy, stringTy, } decafType;


llvm::Type* to_llvmTy(decafType x){
	switch (x) {
		case voidTy: return Builder.getVoidTy();
		case intTy: return Builder.getInt32Ty();
		case boolTy: return Builder.getInt1Ty();
		case stringTy: return Builder.getInt8PtrTy();
		default: throw runtime_error("unknown type in Ty to_llvmTy call");
	}
}

string TyString(decafType x) {
	switch (x) {
		case voidTy: return string("VoidType");
		case intTy: return string("IntType");
		case boolTy: return string("BoolType");
		case stringTy: return string("StringType");
		default: throw runtime_error("unknown type in TyString call");
	}
}

string BinaryOpString(int Op) {
	switch (Op) {
		case T_PLUS: return string("Plus");
  		case T_MINUS: return string("Minus");
  		case T_MULT: return string("Mult");
  		case T_DIV: return string("Div");
  		case T_LEFTSHIFT: return string("Leftshift");
  		case T_RIGHTSHIFT: return string("Rightshift");
  		case T_MOD: return string("Mod");
  		case T_LT: return string("Lt");
  		case T_GT: return string("Gt");
  		case T_LEQ: return string("Leq");
  		case T_GEQ: return string("Geq");
  		case T_EQ: return string("Eq");
  		case T_NEQ: return string("Neq");
  		case T_AND: return string("And");
  		case T_OR: return string("Or");
		default: throw runtime_error("unknown type in BinaryOpString call");
	}
}

string UnaryOpString(int Op) {
	switch (Op) {
  		case T_MINUS: return string("UnaryMinus");
  		case T_NOT: return string("Not");
		default: throw runtime_error("unknown type in UnaryOpString call");
	}
}

string convertInt(int number) {
	stringstream ss;
	ss << number;
	return ss.str();
}

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value* getllvm(){return NULL;}
  virtual void var_decl_to_each_var(){}
  virtual void method_block_llvm(){}
};


string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

string buildString1(const char *Name, decafAST *a) {
	return string(Name) + "(" + getString(a) + ")";
}

string buildString1(const char *Name, string a) {
	return string(Name) + "(" + a + ")";
}

string buildString2(const char *Name, decafAST *a, decafAST *b) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + ")";
}

string buildString2(const char *Name, string a, decafAST *b) {
	return string(Name) + "(" + a + "," + getString(b) + ")";
}

string buildString2(const char *Name, string a, string b) {
	return string(Name) + "(" + a + "," + b + ")";
}

string buildString3(const char *Name, decafAST *a, decafAST *b, decafAST *c) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, decafAST *b, decafAST *c) {
	return string(Name) + "(" + a + "," + getString(b) + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, string b, decafAST *c) {
	return string(Name) + "(" + a + "," + b + "," + getString(c) + ")";
}

string buildString3(const char *Name, string a, string b, string c) {
	return string(Name) + "(" + a + "," + b + "," + c + ")";
}

string buildString4(const char *Name, string a, decafAST *b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + a + "," + getString(b) + "," + getString(c) + "," + getString(d) + ")";
}

string buildString4(const char *Name, string a, string b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + a + "," + b + "," + getString(c) + "," + getString(d) + ")";
}

string buildString4(const char *Name, decafAST *a, decafAST *b, decafAST *c, decafAST *d) {
	return string(Name) + "(" + getString(a) + "," + getString(b) + "," + getString(c) + "," + getString(d) + ")";
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





class TypedSymbol {
	string Sym;
	decafType Ty;
	int line;
	int charno;
public:
	TypedSymbol(string s, decafType t) : Sym(s), Ty(t) {
		line = lineno;
		charno = tokenpos;
	}
	string str() { 
		if (Sym.empty()) { 
			return "VarDef(" + TyString(Ty) + ")"; 
		} else { 
			return "VarDef(" + Sym + "," + TyString(Ty) + ")";
		}
	}

        decafType getTy(){return Ty;}
        string getSym(){return Sym;}
        void declVar(){

             if(access_block_symtbl(Sym)!=NULL){
                 cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                 throw runtime_error("variable has already been declared in this block\n");
             }

             llvm::AllocaInst *Alloca;
             llvm::Constant *zeroInit = llvm::Constant::getNullValue(to_llvmTy(Ty));
             Alloca =  Builder.CreateAlloca(to_llvmTy(Ty), nullptr, Sym);
             Builder.CreateStore(zeroInit, Alloca);						//zero Init
             insert_symbol(Sym, new descriptor(to_llvmTy(Ty),Alloca));

        }
};


//cout<< "decl Var 1ce"<<endl;


class TypedSymbolListAST : public decafAST {
	list<class TypedSymbol *> arglist;
	decafType listType; // this variable is used if all the symbols in the list share the same type
public:
	TypedSymbolListAST() {}
	TypedSymbolListAST(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_front(s);
		listType = ty;
	}
	~TypedSymbolListAST() {
		for (list<class TypedSymbol *>::iterator i = arglist.begin(); i != arglist.end(); i++) { 
			delete *i;
		}
	}
	void push_front(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_front(s); 
	}	
	void push_back(string sym, decafType ty) {
		TypedSymbol *s = new TypedSymbol(sym, ty);
		arglist.push_back(s); 
	}
	void new_sym(string sym) {
		if (arglist.empty()) { 
			throw runtime_error("Error in AST creation: insertion into empty typed symbol list\n");
		}
		TypedSymbol *s = new TypedSymbol(sym, listType);
		arglist.push_front(s);
	}
        
        list<class TypedSymbol *> getlist(){return arglist;}
	string str() { return commaList<class TypedSymbol *>(arglist); }
        
        std::vector<llvm::Type *> to_arg_type_vector(){
            std::vector<llvm::Type *> r;
            for (list<class TypedSymbol *>::iterator i = arglist.begin(); i != arglist.end(); i++) {
                r.push_back(to_llvmTy((*i)->getTy()));
            }
            return r;
        }


        void var_decl_to_each_var(){
            for (list<class TypedSymbol *>::iterator i = arglist.begin(); i != arglist.end(); i++) { 
                (*i)->declVar();
            }
        }

};



llvm::Value* convertType(llvm::Value* val, llvm::Type* Ty){

	llvm::Value* v;
	if(v->getType()!=Ty){
		v = Builder.CreateZExt(val, Ty, "zexttmp");
	}else{
		v = val;
	}
	return v;
}




/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
	int line;
	int charno;
public:
	decafStmtList() {
		line = lineno;
		charno = tokenpos;
	}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) { 
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	string str() { return commaList<class decafAST *>(stmts); }

        std::vector<llvm::Value *> to_arg_vector(std::list<llvm::Type *> type){
            std::vector<llvm::Value *> r;

            list<llvm::Type *>::iterator i_t = type.begin();
            for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++){
                llvm:: Value* v = (*i)->getllvm();
                llvm::Type *Ty = (*i_t);

                if(Ty==Builder.getInt32Ty() || Ty==Builder.getInt1Ty()){
                    v = convertType(v,Ty);
                }
                if(Ty != v->getType()){
                    cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                    throw runtime_error("function parameter type mismatch\n");
                }
                r.push_back(v);
                i_t++;
            }
            return r;
        }

        std::vector<llvm::Value *> bool_to_int_arg_vector(){
            std::vector<llvm::Value *> r;
            for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++){
                llvm:: Value* v = (*i)->getllvm();
                v = convertType(v, Builder.getInt32Ty());
                r.push_back(v);
            }
            return r;
        }

        void var_decl_list_to_var_decl(){
            for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++)  
                {(*i)->var_decl_to_each_var();}
        }

        void run_each_statement(){

            for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++)  
                {(*i)->getllvm();}
        }
 
        void run_each_methodBlock(){

           for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++)  
                {(*i)->method_block_llvm();}


        }
        
};


class expr :decafAST{
public:
      virtual llvm::Value* getllvm(){return NULL;}
};


/// NumberExprAST - Expression class for integer numeric literals like "12".
class NumberExprAST : public decafAST {
	int Val;

public:
	NumberExprAST(int val) : Val(val) {}
	string str() { return buildString1("NumberExpr", convertInt(Val)); }
        llvm::Value* getllvm(){ return Builder.getInt32(Val);}
};




string for_esap(string s, int size){
    string news = s;
    for(int i = 0; i < size;i++){
        if(news[i]=='\\'){
            string to_r;
            switch(news[i+1]){
		case '\\': to_r = "\\";break;
		case '\'': to_r = "\'";break;
		case '\"': to_r = "\"";break;
		case 'a': to_r = "\a";break;
		case 'b': to_r = "\b";break;
		case 'f': to_r = "\f";break;
		case 'n': to_r = "\n";break;
		case 'r': to_r = "\r";break;
		case 't': to_r = "\t";break;
		case 'v': to_r = "\v";break;
            }
            news.replace(i,2,to_r);
            size--;
        }

    }
    return news;
    
}





/// StringConstAST - string constant
class StringConstAST : public decafAST {
	string StringConst;
public:
	StringConstAST(string s) : StringConst(s) {}
	string str() { return buildString1("StringConstant", "\"" + StringConst + "\""); }
        llvm::Value* getllvm(){

            int size = StringConst.length();
            string s = for_esap(StringConst,size);

llvm::GlobalVariable *GS = Builder.CreateGlobalString(s, "globalstring");
//llvm::Type *ty = GS->getType();
//llvm::Type *eltTy = llvm::cast<llvm::PointerType>(ty)->getElementType();

llvm::Value *stringConst = Builder.CreateConstGEP2_32(GS->getValueType(), GS, 0, 0, "cast");
            return stringConst;
        }
};





/// BoolExprAST - Expression class for boolean literals: "true" and "false".
class BoolExprAST : public decafAST {
	bool Val;
public:
	BoolExprAST(bool val) : Val(val) {}
	string str() { return buildString1("BoolExpr", Val ? string("True") : string("False")); }
        llvm::Value* getllvm(){
            if(Val){return Builder.getInt1(1);}
            else{return Builder.getInt1(0);}
        }
};





/// VariableExprAST - Expression class for variables like "a".
class VariableExprAST : public decafAST {
	string Name;
        llvm::Value *valpos;
	int line;
	int charno;
public:
	VariableExprAST(string name) : Name(name) {
		line = lineno;
		charno = tokenpos;
	}
	string str() { return buildString1("VariableExpr", Name); }
	//const std::string &getName() const { return Name; }

        llvm::Value* getllvm(){
            llvm::Value *valpos;
            llvm::AllocaInst *Allo;
            descriptor* D = access_symtbl(Name);
            if(D==NULL){
                cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("Error unknown variable in expr\n");
            }
            Allo = (llvm::AllocaInst *)(D->second);
            valpos = Builder.CreateLoad(Allo, "loadtmp");
            return valpos;
        }
};





/// MethodCallAST - call a function with some arguments
class MethodCallAST : public decafAST {
	string Name;
	decafStmtList *Args;
	int line;
	int charno;

public:
	MethodCallAST(string name, decafStmtList *args) : Name(name), Args(args) {
		line = lineno;
		charno = tokenpos;
	}
	~MethodCallAST() { delete Args; }
	string str() { return buildString2("MethodCall", Name, Args); }

        llvm::Value* getllvm(){

            llvm::Function *call;
            descriptor* D = access_symtbl(Name);
            if(D==NULL){
                cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("Error unknown functoin call\n");
            }
            call = (llvm::Function *)(D->second);



            std::vector<llvm::Value *> args;
            if(((D->fourth).size()==0) && (Args!=NULL)){
                cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("method call wrong arguements number\n");
            }
            if(Args!=NULL){

                if(Args->size()!=(D->fourth).size()){
                    cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                    throw runtime_error("method call wrong arguements number\n");
                }

                if(Name.compare("print_int")==0){
                    args = Args->bool_to_int_arg_vector();
                }else{
                    std::vector<llvm::Type *> arg_type = (D->fourth);
                    std::list<llvm::Type *> args_list;
                    std::copy(arg_type.begin(), arg_type.end(), std::back_inserter(args_list));
                    args = Args->to_arg_vector(args_list);
                }
            }



            bool isVoid = call->getReturnType()->isVoidTy();
            llvm::Value *val;
//cout<< "create call"<<endl;
            if(isVoid){
                Builder.CreateCall(call,args);
                val = NULL;
            }
            else{val = Builder.CreateCall(call,args,"calltmp");}

            return val;
        }
};


int expr_line = 0;
int expr_char = 0;

void checkInt(llvm::Value* v){
    if(v->getType() != Builder.getInt32Ty()){
        cout<<"in line "<<expr_line<<", before char "<<expr_char<<", error: \n";
        throw runtime_error("operator only works on Integer expressions\n");
    }
}
void checkBool(llvm::Value* v){
    if(v->getType() != Builder.getInt1Ty()){
        cout<<"in line "<<expr_line<<", before char "<<expr_char<<", error: \n";
        throw runtime_error("operator only works on Boolean expressions\n");
    }
}

void checkSameType(llvm::Value* v1, llvm::Value* v2){
    if(v1->getType() != v2->getType()){
        cout<<"in line "<<expr_line<<", before char "<<expr_char<<", error: \n";
        throw runtime_error("two expressions are not in same type\n");
    }
}

void checkIsNULL(llvm::Value* v){	//is NULL when it is a void method call
    if(v==NULL){
        cout<<"in line "<<expr_line<<", before char "<<expr_char<<", error: \n";
        throw runtime_error("void function call in an expression\n");
    }
}


llvm::Value* skct_and(decafAST *LHS, decafAST *RHS){
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *ARB = llvm::BasicBlock::Create(TheContext, "andright",func);
    llvm::BasicBlock *AEB = llvm::BasicBlock::Create(TheContext, "andend",func);

    llvm::Value *L = LHS->getllvm();
    checkBool(L);checkIsNULL(L);
    llvm::BasicBlock *CurBB = Builder.GetInsertBlock();
    Builder.CreateCondBr(L, ARB, AEB);

    Builder.SetInsertPoint(ARB);
    llvm::Value *R = RHS->getllvm();
    checkBool(R);checkIsNULL(R);
    llvm::Value *Res = Builder.CreateAnd(L,R,"andtemp");
    llvm::BasicBlock *After = Builder.GetInsertBlock();
    Builder.CreateBr(AEB);

    Builder.SetInsertPoint(AEB);
    llvm::PHINode *val = Builder.CreatePHI(Builder.getInt1Ty(), 2, "phival");
    val->addIncoming(L, CurBB);
    val->addIncoming(Res, After);

    return val;
}

llvm::Value* skct_or(decafAST *LHS, decafAST *RHS){
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *ORB = llvm::BasicBlock::Create(TheContext, "orright",func);
    llvm::BasicBlock *OEB = llvm::BasicBlock::Create(TheContext, "orend",func);

    llvm::Value *L = LHS->getllvm();
    checkBool(L);checkIsNULL(L);
    llvm::BasicBlock *CurBB = Builder.GetInsertBlock();
    Builder.CreateCondBr(L, OEB, ORB);

    Builder.SetInsertPoint(ORB);
    llvm::Value *R = RHS->getllvm();
    checkBool(R);checkIsNULL(R);
    llvm::Value *Res = Builder.CreateOr(L,R,"ortemp");
    llvm::BasicBlock *After = Builder.GetInsertBlock();
    Builder.CreateBr(OEB);

    Builder.SetInsertPoint(OEB);
    llvm::PHINode *val = Builder.CreatePHI(Builder.getInt1Ty(), 2, "phival");
    val->addIncoming(L, CurBB);
    val->addIncoming(Res, After);

    return val;
}

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public decafAST {
	int Op; // use the token value of the operator
	decafAST *LHS, *RHS;
	int line;
	int charno;
public:
	BinaryExprAST(int op, decafAST *lhs, decafAST *rhs) : Op(op), LHS(lhs), RHS(rhs) {
		line = lineno;
		charno = tokenpos;
	}
	~BinaryExprAST() { delete LHS; delete RHS; }
	string str() { return buildString3("BinaryExpr", BinaryOpString(Op), LHS, RHS); }
        
        llvm::Value* getllvm(){

            expr_line=line;expr_char=charno;

            if(Op==T_AND){
                return skct_and(LHS,RHS);
            }
            if(Op==T_OR){
                return skct_or(LHS,RHS);
            }
            

            llvm::Value *L = LHS->getllvm();
            llvm::Value *R = RHS->getllvm();
            checkIsNULL(L);checkIsNULL(R);
            switch (Op) {
		case T_PLUS: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateAdd(L, R, "addtmp");
  		case T_MINUS: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateSub(L,R, "subtmp");
  		case T_MULT: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateMul(L,R, "multmp");
  		case T_DIV: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateSDiv(L,R, "divtmp");
  		case T_LEFTSHIFT: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateShl(L,R, "shltmp");
  		case T_RIGHTSHIFT: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateLShr(L,R, "shrtmp");
  		case T_MOD: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateSRem(L,R, "remtmp");
  		case T_LT: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateICmpSLT(L,R, "SLTtmp");
  		case T_GT: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateICmpSGT(L,R, "SGTtmp");
  		case T_LEQ: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateICmpSLE(L,R, "SLEtmp");
  		case T_GEQ: 
                    checkInt(L);checkInt(R);
                    return Builder.CreateICmpSGE(L,R, "SGEtmp");
  		case T_EQ: 
                    return Builder.CreateICmpEQ(L,R, "Eqtmp");
  		case T_NEQ: 
                    return Builder.CreateICmpNE(L,R, "Neqtmp");
  			
		default: throw runtime_error("unknown type in binary operation\n");
	    }

            return NULL;
        }
};




/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public decafAST {
	int Op; // use the token value of the operator
	decafAST *Expr;
	int line;
	int charno;
public:
	UnaryExprAST(int op, decafAST *expr) : Op(op), Expr(expr) {
		line = lineno;
		charno = tokenpos;
	}
	~UnaryExprAST() { delete Expr; }
	string str() { return buildString2("UnaryExpr", UnaryOpString(Op), Expr); }

        llvm::Value* getllvm(){
            expr_line=line;expr_char=charno;
            llvm::Value *E = Expr->getllvm();
            checkIsNULL(E);
	    switch (Op) {
  		case T_MINUS: 
                    checkInt(E);
                    return Builder.CreateNeg(E, "UnaryMinustmp");
  		case T_NOT:
                    checkBool(E); 
                    return Builder.CreateNot(E, "Nottmp");
		default: throw runtime_error("unknown type in UnaryOpString call\n");
	    }

        }
};





/// AssignVarAST - assign value to a variable
class AssignVarAST : public decafAST {
	string Name; // location to assign value
	decafAST *Value;
	int line;
	int charno;
public:
	AssignVarAST(string name, decafAST *value) : Name(name), Value(value) {
		line = lineno;
		charno = tokenpos;
        }
	~AssignVarAST() { 
		if (Value != NULL) { delete Value; }
	}
	string str() { return buildString2("AssignVar", Name, Value); }

        llvm::Value* getllvm(){
            llvm::Value *right = Value->getllvm();
	    descriptor* D = access_symtbl(Name);
            if(D==NULL){
                cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("unknown variable in Assigning Variable\n");
            }

            if((D->first)!=(right->getType())){
                cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("type mismatch in Assigning Variable\n");
            }
            llvm::AllocaInst *Alloca = (llvm::AllocaInst *)(D->second);
            const llvm::PointerType *ptrTy = right->getType()->getPointerTo();
            llvm::Value *val = NULL;
            if(ptrTy == Alloca->getType()){
                llvm::Value *val = Builder.CreateStore(right, Alloca);
            }
            return val;
        }
};





/// AssignArrayLocAST - assign value to a variable
class AssignArrayLocAST : public decafAST {
	string Name; // name of array variable
    decafAST *Index;  // index for assignment of value
	decafAST *Value;
	int line;
	int charno;
public:
	AssignArrayLocAST(string name, decafAST *index, decafAST *value) : Name(name), Index(index), Value(value) {
		line = lineno;
		charno = tokenpos;
	}
	~AssignArrayLocAST() { delete Index; delete Value; }
	string str() { return buildString3("AssignArrayLoc", Name, Index, Value); }

        llvm::Value* getllvm(){

		llvm::Value *ix = Index->getllvm();
		if(ix->getType() != Builder.getInt32Ty()){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("Error in Array Index is not an int\n");
		}
		llvm::Value *right = Value->getllvm();
		descriptor* D = access_symtbl(Name);
		if(D==NULL){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("Error in Assign Array: unknown variable\n");
		}
                llvm::Type *tempTy = (llvm::Type*)(D->first);
		if(!(tempTy->isArrayTy())){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("variable is not an array type\n");
		}
		if((llvm::Type *)(D->third)!=(right->getType())){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("type mismatch in Assigning Array element\n");
		}

		llvm::ArrayType *arrayTy = (llvm::ArrayType*)(D->first);
		llvm::GlobalVariable *Allo;
		Allo = (llvm::GlobalVariable *)(D->second);	//Gloabl Var
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(arrayTy, Allo, 0, "arrayloc");
		llvm::Value *ArrayIndex = Builder.CreateGEP((llvm::Type*)(D->third), ArrayLoc, ix, "arrayindex");//
		llvm::Value *ArrayStore = Builder.CreateStore(right, ArrayIndex); 
		return ArrayStore;
        }


};





/// ArrayLocExprAST - access an array location
class ArrayLocExprAST : public decafAST {
	string Name;
	decafAST *Expr;
	int line;
	int charno;

public:
	ArrayLocExprAST(string name, decafAST *expr) : Name(name), Expr(expr) {
		line = lineno;
		charno = tokenpos;
	}
	~ArrayLocExprAST() {
		if (Expr != NULL) { delete Expr; }
	}
	string str() { return buildString2("ArrayLocExpr", Name, Expr); }

	llvm::Value* getllvm(){
		llvm::Value *Index = Expr->getllvm();
		if(Index->getType() != Builder.getInt32Ty()){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("Error in Array Index is not an int\n");
		}
		llvm::Value *valpos;
		descriptor* D = access_symtbl(Name);
		if(D==NULL){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("Error in unknown Array variable\n");
		}
		llvm::Type *tempTy = (llvm::Type*)(D->first);
		if(!(tempTy->isArrayTy())){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("variable is not an array type\n");
		}

		llvm::ArrayType *arrayTy = (llvm::ArrayType*)(D->first);
		llvm::GlobalVariable *Allo;
		Allo = (llvm::GlobalVariable *)(D->second);	//Gloabl Var
		llvm::Value *ArrayLoc = Builder.CreateStructGEP(arrayTy, Allo, 0, "arrayloc");
		llvm::Value *ArrayIndex = Builder.CreateGEP((llvm::Type*)(D->third), ArrayLoc, Index, "arrayindex");//
		valpos = Builder.CreateLoad(ArrayIndex, "loadtmp");
		return valpos;
        }
};





/// BlockAST - block
class BlockAST : public decafAST {
	decafStmtList *Vars;
	decafStmtList *Statements;
public:
	BlockAST(decafStmtList *vars, decafStmtList *s) : Vars(vars), Statements(s) {}
	~BlockAST() { 
		if (Vars != NULL) { delete Vars; }
		if (Statements != NULL) { delete Statements; }
	}
	decafStmtList *getVars() { return Vars; }
	decafStmtList *getStatements() { return Statements; }
	string str() { return buildString2("Block", Vars, Statements); }

        llvm::Value* getllvm(){
push_new_symbol_table();
            Vars->var_decl_list_to_var_decl();
            Statements->run_each_statement();
pop_head_symbol_table();
            return NULL;
        }
        
};





/// MethodBlockAST - block for methods
class MethodBlockAST : public decafAST {
	decafStmtList *Vars;
	decafStmtList *Statements;
public:
	MethodBlockAST(decafStmtList *vars, decafStmtList *s) : Vars(vars), Statements(s) {}
	~MethodBlockAST() { 
		if (Vars != NULL) { delete Vars; }
		if (Statements != NULL) { delete Statements; }
	}
	string str() { return buildString2("MethodBlock", Vars, Statements); }

        llvm::Value* getllvm(){
push_new_symbol_table();
            Vars->var_decl_list_to_var_decl();

            Statements->run_each_statement();
pop_head_symbol_table();
            return NULL;
        }
};





/// IfStmtAST - if statement
class IfStmtAST : public decafAST {
	decafAST *Cond;
	BlockAST *IfTrueBlock;
	BlockAST *ElseBlock;
public:
	IfStmtAST(decafAST *cond, BlockAST *iftrue, BlockAST *elseblock) : Cond(cond), IfTrueBlock(iftrue), ElseBlock(elseblock) {}
	~IfStmtAST() { 
		delete Cond; 
		delete IfTrueBlock; 
		if (ElseBlock != NULL) { delete ElseBlock; }
	}
	string str() { return buildString3("IfStmt", Cond, IfTrueBlock, ElseBlock); }

	llvm::Value* getllvm(){

		llvm::Function *func = Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *ISB = llvm::BasicBlock::Create(TheContext, "ifstart",func);
		llvm::BasicBlock *IfTrueBB = llvm::BasicBlock::Create(TheContext, "iftrue",func);
		llvm::BasicBlock *IfElseBB;
		if(ElseBlock != NULL){
			IfElseBB = llvm::BasicBlock::Create(TheContext, "ifelse",func);
		}
		llvm::BasicBlock *EndBB = llvm::BasicBlock::Create(TheContext, "ifend",func);

		Builder.CreateBr(ISB);
		Builder.SetInsertPoint(ISB);
		llvm::Value* cond = Cond->getllvm();
		checkBool(cond);

		if(ElseBlock != NULL){
			Builder.CreateCondBr(cond, IfTrueBB, IfElseBB);
		}else{
			Builder.CreateCondBr(cond, IfTrueBB, EndBB);
		}
		Builder.SetInsertPoint(IfTrueBB);
		IfTrueBlock->getllvm();
		Builder.CreateBr(EndBB);

		if(ElseBlock != NULL){
			Builder.SetInsertPoint(IfElseBB);
			ElseBlock->getllvm();
			Builder.CreateBr(EndBB);
		}
		
		Builder.SetInsertPoint(EndBB);

		return NULL;
	}

};





/// WhileStmtAST - while statement
class WhileStmtAST : public decafAST {
	decafAST *Cond;
	BlockAST *Body;
public:
	WhileStmtAST(decafAST *cond, BlockAST *body) : Cond(cond), Body(body) {}
	~WhileStmtAST() { delete Cond; delete Body; }
	string str() { return buildString2("WhileStmt", Cond, Body); }

	llvm::Value* getllvm(){
//can be used after slove while(true)
		
		
		llvm::Function *func = Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *WSB = llvm::BasicBlock::Create(TheContext, "whilestart",func);
		llvm::BasicBlock *WMB = llvm::BasicBlock::Create(TheContext, "Whilemain",func);
		llvm::BasicBlock *WEB = llvm::BasicBlock::Create(TheContext, "Whileend",func);
		
		contine_dest_list.push_front(WSB);
		break_dest_list.push_front(WEB);
		
		Builder.CreateBr(WSB);
		Builder.SetInsertPoint(WSB);
		llvm::Value* cond = Cond->getllvm();
		checkBool(cond);

		Builder.CreateCondBr(cond, WMB, WEB);
		Builder.SetInsertPoint(WMB);
		Body->getllvm();
		Builder.CreateBr(WSB);

		Builder.SetInsertPoint(WEB);

		contine_dest_list.pop_front();
		break_dest_list.pop_front();
		return NULL;

	}

};



void test1(){cout<< "1111111111111" <<endl;}
void test2(){cout<< "2222222222222" <<endl;}

/// ForStmtAST - for statement
class ForStmtAST : public decafAST {
	decafStmtList *InitList;
	decafAST *Cond;
	decafStmtList *LoopEndList;
	BlockAST *Body;
public:
	ForStmtAST(decafStmtList *init, decafAST *cond, decafStmtList *end, BlockAST *body) :
		InitList(init), Cond(cond), LoopEndList(end), Body(body) {}
	~ForStmtAST() {
		delete InitList;
		delete Cond;
		delete LoopEndList;
		delete Body;
	}
	string str() { return buildString4("ForStmt", InitList, Cond, LoopEndList, Body); }

	llvm::Value* getllvm(){
//cout<< "for start"<<endl;
		llvm::Function *func = Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *FSB = llvm::BasicBlock::Create(TheContext, "forstart",func);
		llvm::BasicBlock *FMB = llvm::BasicBlock::Create(TheContext, "formain",func);
		llvm::BasicBlock *FAB = llvm::BasicBlock::Create(TheContext, "forafter",func);
		llvm::BasicBlock *FEB = llvm::BasicBlock::Create(TheContext, "forend",func);

		contine_dest_list.push_front(FAB);
		break_dest_list.push_front(FEB);

//cout<< "for block done"<<endl;	
		Builder.CreateBr(FSB);
		Builder.SetInsertPoint(FSB);

		InitList->run_each_statement();
//cout<< "for init done"<<endl;	
		llvm::Value* cond = Cond->getllvm();
		checkBool(cond);

		Builder.CreateCondBr(cond, FMB, FEB);
		Builder.SetInsertPoint(FMB);
		Body->getllvm();
		Builder.CreateBr(FAB);

		Builder.SetInsertPoint(FAB);
		LoopEndList->run_each_statement();
		cond = Cond->getllvm();
		Builder.CreateCondBr(cond, FMB, FEB);

		Builder.SetInsertPoint(FEB);

		contine_dest_list.pop_front();
		break_dest_list.pop_front();
		return NULL;

	}


};




/// ReturnStmtAST - return statement
class ReturnStmtAST : public decafAST {
	decafAST *Value;
	int line;
	int charno;
public:
	ReturnStmtAST(decafAST *value) : Value(value) {
		line = lineno;
		charno = tokenpos;
	}
	~ReturnStmtAST() { 
		if (Value != NULL) { delete Value; }
	}
	string str() { return buildString1("ReturnStmt", Value); }
        
        llvm::Value* getllvm(){
            llvm::Function *F = Builder.GetInsertBlock()->getParent();
            llvm::Type* retTy = F->getReturnType();

            if(retTy->isVoidTy() && Value!=NULL){
		cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("trying to return a value in a void function\n");
            }
            


            if(retTy->isVoidTy()){Builder.CreateRetVoid();}
            else{
                llvm::Value* v;
                if(Value==NULL){v = llvm::Constant::getNullValue(retTy);}
                else{v = Value->getllvm();}
                if(v->getType() != retTy){
		    cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                    throw runtime_error("return Type is not vaild\n");
                }
                Builder.CreateRet(v);
            }



            llvm::verifyFunction(*F);
            return NULL;
        }
};




/// BreakStmtAST - break statement
class BreakStmtAST : public decafAST {
	int line;
	int charno;
public:
	BreakStmtAST() {
		line = lineno;
		charno = tokenpos;
	}
	string str() { return string("BreakStmt"); }

	llvm::Value* getllvm(){

		if(contine_dest_list.empty()){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("break statement cannot be made\n");
		}
		llvm::BasicBlock *BreakB = break_dest_list.front();
		Builder.CreateBr(BreakB);
		
		return NULL;
	}

};




/// ContinueStmtAST - continue statement
class ContinueStmtAST : public decafAST {
	int line;
	int charno;
public:
	ContinueStmtAST() {
		line = lineno;
		charno = tokenpos;
	}
	string str() { return string("ContinueStmt"); }

	llvm::Value* getllvm(){

		if(contine_dest_list.empty()){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("continue statement cannot be made\n");
		}
		llvm::BasicBlock *ContinueB = contine_dest_list.front(); 
		Builder.CreateBr(ContinueB);
		
		return NULL;
	}
};





/// MethodDeclAST - function definition
class MethodDeclAST : public decafAST {
	decafType ReturnType;
	string Name;
	TypedSymbolListAST *FunctionArgs;
	MethodBlockAST *Block;
        llvm::Function *func;
	int line;
	int charno;
public:
	MethodDeclAST(decafType rtype, string name, TypedSymbolListAST *fargs, MethodBlockAST *block) 
		: ReturnType(rtype), Name(name), FunctionArgs(fargs), Block(block) {
		line = lineno;
		charno = tokenpos;
	}
	~MethodDeclAST() { 
		delete FunctionArgs;
		delete Block; 
	}
	string str() { return buildString4("Method", Name, TyString(ReturnType), FunctionArgs, Block); }

        llvm::Value*  getllvm(){

            if(access_symtbl(Name)!=NULL){
		cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
                throw runtime_error("the method name has already been declared\n");
            }


            llvm::Type *returnTy;	
            switch (ReturnType){
                case voidTy:
                    returnTy = Builder.getVoidTy();break;
                case intTy:
                    returnTy = Builder.getInt32Ty();break;
                case boolTy:
                    returnTy = Builder.getInt1Ty();break;
                case stringTy:
                    returnTy = Builder.getInt8PtrTy();break;
            }

            std::vector<llvm::Type *> args;

            if(FunctionArgs!=NULL){
                args = FunctionArgs->to_arg_type_vector();

            }

            func = llvm::Function::Create(
              llvm::FunctionType::get(returnTy, args, false),
              llvm::Function::ExternalLinkage,
              Name,
              TheModule
            );

            
            if(FunctionArgs==NULL){
                insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func,args));
            }else{
                insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func,args));
            }


            llvm::verifyFunction(*func);


            return func;
        }

        void method_block_llvm(){
///////////////

//cout<< Name;
//cout<< "    method block start" <<endl;

            llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", func);

            Builder.SetInsertPoint(BB);


          if(FunctionArgs!=NULL){
            list<class TypedSymbol *> Arglist = FunctionArgs->getlist();

            for(auto argindex = func->arg_begin(); argindex!=func->arg_end(); argindex = std::next(argindex)){
                llvm::Value* arg1 = &(*argindex);
                TypedSymbol * var = Arglist.front();
                Arglist.pop_front();
                string Sym = var->getSym();
                llvm::Type* type = to_llvmTy(var->getTy());
                llvm::AllocaInst *Alloca =  Builder.CreateAlloca(to_llvmTy(var->getTy()), nullptr, Sym);
                llvm::Value *val = Builder.CreateStore(arg1, Alloca);
                insert_symbol(Sym, new descriptor(type,Alloca));
            }
          }



            Block->getllvm();

            if(Name.compare("main")==0){
                if(ReturnType==voidTy){
                    Builder.CreateRetVoid();
                }
                if(ReturnType==intTy){
                    Builder.CreateRet(Builder.getInt32(0));
                }
            }else{
                Builder.CreateRetVoid();
            }



//cout<< Name;
//cout<< "    method block done" <<endl;

//////////////

        }
};





/// AssignGlobalVarAST - assign value to a global variable
class AssignGlobalVarAST : public decafAST {
	decafType Ty;
	string Name; // location to assign value
	decafAST *Value;
	int line;
	int charno;
public:
	AssignGlobalVarAST(decafType ty, string name, decafAST *value) : Ty(ty), Name(name), Value(value) {
		line = lineno;
		charno = tokenpos;
	}
	~AssignGlobalVarAST() { 
		if (Value != NULL) { delete Value; }
	}
	string str() { return buildString3("AssignGlobalVar", Name, TyString(Ty), Value); }

	llvm::Value* getllvm(){

		if(access_symtbl(Name)!=NULL){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("field name has already been declared\n");
		}

		llvm::GlobalVariable *GV = new llvm::GlobalVariable(
			*TheModule, 
			to_llvmTy(Ty), 
			false,  // variable is mutable
			llvm::GlobalValue::InternalLinkage, 
			(llvm::Constant *)(Value->getllvm()), 
			Name
		);
		//Builder.CreateStore(Value->getllvm(), GV);
		insert_symbol(Name, new descriptor(to_llvmTy(Ty),GV));
		
//cout<< Name;
//cout<< "    GlobalVar Assigned" <<endl;

		return NULL;
		
	}
};





/// FieldDecl - field declaration aka Decaf global variable
class FieldDecl : public decafAST {
	string Name;
	decafType Ty;
	int Size; // -1 for scalars and size value for arrays, size 0 array is an error
	int line;
	int charno;
public:
	FieldDecl(string name, decafType ty, int size){
            Name = name;
            Ty = ty;
            if(size<=0){
                Size = 0;
            }else{
                Size = size;
            }
	    line = lineno;
	    charno = tokenpos;
	
        }
	FieldDecl(string name, decafType ty){
            Name = name;
            Ty = ty;
            Size = -1;

	    line = lineno;
	    charno = tokenpos;

        }
	string str() { return buildString3("FieldDecl", Name, TyString(Ty), (Size == -1) ? "Scalar" : "Array(" + convertInt(Size) + ")"); }

	llvm::Value* getllvm(){

		if(access_symtbl(Name)!=NULL){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("field name has already been declared\n");
		}
                if(Size==0){
			cout<<"in line "<<line<<", before char "<<charno<<", error: \n";
			throw runtime_error("declaring array size less than or equal to 0\n");
                }


		if(Size == -1){
			llvm::GlobalVariable *GV = new llvm::GlobalVariable(
				*TheModule, 
				to_llvmTy(Ty), 
				false,  // variable is mutable
				llvm::GlobalValue::InternalLinkage, 
				Builder.getInt32(0), 
				Name
			);
			insert_symbol(Name, new descriptor(to_llvmTy(Ty),GV));

		}else{		//array

			llvm::ArrayType *arrayTy = llvm::ArrayType::get(to_llvmTy(Ty), Size);
			llvm::Constant *zeroInit = llvm::Constant::getNullValue(arrayTy);
			llvm::GlobalVariable *GV = new llvm::GlobalVariable(*TheModule, arrayTy, false,llvm::GlobalValue::ExternalLinkage, zeroInit, Name);
			insert_symbol(Name, new descriptor(arrayTy,GV,to_llvmTy(Ty)));
		}

		return NULL;

	}

};

class FieldDeclListAST : public decafAST {
	list<class decafAST *> arglist;
	decafType listType; // this variable is used if all the symbols in the list share the same type
	int size; // this variable is used if all the symbols in the list share the same type, -1 for scalar, array size otherwise
public:
	FieldDeclListAST() {}
	FieldDeclListAST(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_front(s);
		listType = ty;
		size = sz;
	}
	FieldDeclListAST(string sym, decafType ty) {
		FieldDecl *s = new FieldDecl(sym, ty);
		arglist.push_front(s);
		listType = ty;
		size = -1;
	}
	~FieldDeclListAST() {
		for (list<class decafAST *>::iterator i = arglist.begin(); i != arglist.end(); i++) { 
			delete *i;
		}
	}
	void push_front(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_front(s); 
	}	
	void push_back(string sym, decafType ty, int sz) {
		FieldDecl *s = new FieldDecl(sym, ty, sz);
		arglist.push_back(s); 
	}
	void new_sym(string sym) {
		if (arglist.empty()) { 
			throw runtime_error("Error in AST creation: insertion into empty field list\n");
		}
		FieldDecl *s = new FieldDecl(sym, listType, size);
		arglist.push_front(s);
	}
	void new_sym(string sym, int sz) {
		if (arglist.empty()) { 
			throw runtime_error("Error in AST creation: insertion into empty field list\n");
		}
		FieldDecl *s = new FieldDecl(sym, listType, sz);
		arglist.push_back(s);
	}
	string str() { return commaList<class decafAST *>(arglist); }

	llvm::Value* getllvm(){
//cout<< " start field list" <<endl;
		for (list<class decafAST *>::iterator i = arglist.begin(); i != arglist.end(); i++) {
//cout<< "  dealing with fields" <<endl;
			(*i)->getllvm();
		}
//cout<< " end field list" <<endl;
		return NULL;
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
	string str() { return buildString3("Package", Name, FieldDeclList, MethodDeclList); }

        void Codegen(){
            push_new_symbol_table();
//cout<< " 222222222222" <<endl;
            FieldDeclList->run_each_statement();
//cout<< " 333333333333" <<endl;

            MethodDeclList->run_each_statement();	//decl

            MethodDeclList->run_each_methodBlock();	//run
            pop_head_symbol_table();
        }
};








//extern function
llvm::Function *genPrintIntDef() {
  // create a extern definition for print_int
  std::vector<llvm::Type*> args;
  args.push_back(Builder.getInt32Ty()); // print_int takes one i32 argument
  return llvm::Function::Create(llvm::FunctionType::get(Builder.getVoidTy(), args, false), llvm::Function::ExternalLinkage, "print_int", TheModule);
}

llvm::Function *genPrintStringDef() {
  // create a extern definition for print_string
  std::vector<llvm::Type*> args;
  args.push_back(Builder.getInt8PtrTy()); // print_string takes one string argument
  return llvm::Function::Create(llvm::FunctionType::get(Builder.getVoidTy(), args, false), llvm::Function::ExternalLinkage, "print_string", TheModule);
}

llvm::Function *genReadIntDef() {
  // create a extern definition for print_string
  std::vector<llvm::Type*> args; // read_int takes no argument
  return llvm::Function::Create(llvm::FunctionType::get(Builder.getInt32Ty(), args, false), llvm::Function::ExternalLinkage, "read_int", TheModule);
}



/// ExternAST - extern function definition
class ExternAST : public decafAST {
	decafType ReturnType;
	string Name;
	TypedSymbolListAST *FunctionArgs;
public:
	ExternAST(decafType r, string n, TypedSymbolListAST *fargs) : ReturnType(r), Name(n), FunctionArgs(fargs) {}
	~ExternAST() {
		if (FunctionArgs != NULL) { delete FunctionArgs; }
	}
	string str() { return buildString3("ExternFunction", Name, TyString(ReturnType), FunctionArgs); }

        llvm::Value*  getllvm(){
            llvm::Function *func;
            std::vector<llvm::Type *> args;
            if(FunctionArgs!=NULL){
                args = FunctionArgs->to_arg_type_vector();
            }
            func = llvm::Function::Create(
                llvm::FunctionType::get(to_llvmTy(ReturnType), args, false),
                llvm::Function::ExternalLinkage, 
                Name, 
                TheModule);

            insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func,args));

/*
            if( Name.compare("print_int")==0){
                func = genPrintIntDef();
                insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func));
            }
            if( Name.compare("print_string")==0){
                func = genPrintStringDef();
                insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func));
            }
            if( Name.compare("read_int")==0){
                func = genReadIntDef();
                insert_symbol(Name, new descriptor(to_llvmTy(ReturnType),func));
            }
*/
            return func;
        }


};





/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *ClassDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), ClassDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (ClassDef != NULL) { delete ClassDef; }
	}
	string str() { return buildString2("Program", ExternList, ClassDef); }

        void Codegen(){

            ExternList -> run_each_statement();

            ClassDef -> Codegen();

        }
};













//cout<< "decl Method done"<<endl;



/*
{
    symbol_table outer;
    outer["x"] = new descriptor(1,5);
    outer["y"] = new descriptor(1,7);
    symtbl.push_front(outer);
    descriptor* d = access_symtbl("y");
    if (d != NULL) {
        cout << d->first << '\t' << d->second << endl;
    }
}

*/








