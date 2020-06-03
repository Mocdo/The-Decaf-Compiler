# The-Decaf-Compiler
a fully working compiler for Decaf programming language using LLVM for code generation</br>
</br>
This project followed instruction below</br>
[The Decaf Compiler](http://anoopsarkar.github.io/compilers-class/hw4.html)</br>
</br></br>
All definations below associated with Decaf language follows the defination: [Decaf Programming Language Specification](http://anoopsarkar.github.io/compilers-class/decafspec.html)</br>
</br>
This project has accomplished:
* Implement a lex program that is a lexical analyzer for the Decaf language.
* Porgrammed a parser for the Decaf language that produces an abstract syntax tree for valid Decaf programs
* Implement a Symbol Table that can keep track of Variables and Methods in Decaf
* Provide Code Generation for Decaf Expressions for following
  * Arithmetic and Boolean expressions
  * Function calls
  * Function definitions (including recursive functions)
  * Declaration of extern functions
* Add support for global variables
* Zero initialize all variables
* Add support for Control Flow and Loops
* Implement short-circuit evaluation for boolean expressions
* Implement all the semantic checks and raise a semantic error if the input Decaf program does not pass any of semantic checks
* Implement Error reporting that provide a helpful error message
