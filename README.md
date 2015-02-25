Scheme Parser and Interpreter


My Code: Scheme.ml

The goal of this project was to create a scheme parser and interperter using Ocaml.  The parser translates a plain text scheme program into an abstract syntax tree (AST) and an evaluator that executes the code in the AST.  The rest of the project description is show below.





Part 1: Simple Scheme Programming
Put your solution to this part in the file schemeTest.txt.

Implement the following functions in regular Scheme. All of these functions should operate over integers and/or lists of integers. The purpose of this part is just to make sure you understand Scheme before you try to start writing an interpreter for it.

    Write a function double x that returns two times x.
    Write a function powof2 x that returns true (which is written #t in Scheme) if and only if x is a power of 2. (Hint: Use the functions / and mod.)
    Write a function sum l that returns the sum of the integer list l, using recursion. Hint: You can use the function pair? to determine whether a value is a cons cell or not; and in this problem, you can assume if something is not a cons cell, then it's '().
    Write a function applyToList f l that returns a new list containing all the elements of l, in the same order, but with f applied to them. Your implementation should use recursion. You may not use Scheme's built-in map or for-each functions in writing your map function. Note applyToList directly takes 2 arguments (i.e., it is not using currying). 

Part 2: Scheme Interpreter
For part 2 of this project, your task is to write a evaluator for Scheme expressions. This evaluator will form the basis of your Scheme interpreter. The code for the Scheme interpreter is in scheme.ml.
Scheme Grammar

The grammar for Scheme expressions you need to support is particularly simple:

    S -> id | n | str | #t | #f | ( L )
    L -> S L | epsilon 

where

    id are Scheme identifiers. Identifiers may contain upper- and lower-case letters, digits, =, *, +, /, < >, !, ?, and -. For example, valid identifiers are Foo, set!, <three, and +=. Identifiers may not begin with a digit.
    n are Integers (made up of digits).
    str are Strings (beginning and ending with quotes). For purposes of this project, only alphanumeric and whitespace characters can appear within a string.
    #t and #f are tokens representing true and false.
    ( and ) are tokens (marking the beginning and end of a list). 

Integers and strings are distinguished from identifiers by their first character.
Scheme AST and Values

For this project, Scheme expressions are represented using an AST (abstract syntax tree) defined using the following OCaml data type:

type ast =
  | Id of string
  | Num of int
  | Bool of bool
  | String of string
  | List of ast list

For example, the Scheme s-expression (foo (bar 3) 4 "baz") represented by the AST List [Id "foo"; (List [Id "bar"; Num 3]); Num 4; String "baz"]. Your AST nodes for strings should not include quotes. For example, parse ["\"a\""] should return String "a", i.e., a String constructed from a string with one character, a.

For this project, your Scheme interpreter should represent the value of Scheme expressions using the following value user-variant data type:

type value =
  | Val_Num of int
  | Val_Bool of bool
  | Val_String of string
  | Val_Null
  | Val_Cons of value * value
  | Val_Define of (string * value) list
  | Val_Closure of ...

where the part labeled ... is for you to fill in. However, you must not change the part of value we have given you, because our grading scripts will look for exactly those constructors, with exactly those arguments as given, to test your interpreter.

Scheme Environment and Expression Evaluation
Your interpreter will need to maintain an environment to store bindings of values to symbols. The interpreter will represent the environment as an associative list of type (string * value) list. How the associative list is organized is up to you. The interpreter will need to maintain a top-level environment of all definitions encountered so far in the input program. You will need to write a function eval that, given an environment and an AST, evaluates the expression corresponding to that AST in the given environment. The type of the function is (string * value) list -> ast -> value.

For example, eval [] (Num 3) should return Val_Num 3, meaning that in an empty environment, an AST node containing the number 3 evaluates to the integer 3. Calling eval [("x", Val_Num 3)] (Id "x") should also return Val_Num 3, since x is bound to 3 in the environment used to evaluate x.

When doing this project, be sure to keep straight the difference between textual entities that the programmer has written down (like the text "3") with the resulting value that your interpreter produces (Val_Num 3).
Scheme Language Features
Here are the language features your Scheme interpreter needs to support:

    Basic Expressions
        Values. Your evaluation function should evaluate integers, booleans, and strings to the corresponding values.
        Null. For this project, we will vary slightly from Scheme and make null the built-in keyword for the empty list. Thus null should evaluate to Val_null. In actual Scheme, null is written '(). 

    Functions Calls
        In Scheme function calls appear as lists, where the name of the function is the first element in the list, and the remaining list arguments are the arguments passed to the function. E.g., (+ 1 2 3) calls the built-in + function and passes it three arguments, 1, 2, and 3.
        Your implementation must support call-by-value, so that arguments are evaluated before they are passed to a closure. For example, consider the following Scheme expression: (+ (- 1 2) 3). The arguments to the + function must be first evaluated before they are passed to +. In this case, it means the expression (- 1 2) must be evaluated first, and its resulting value passed as the first argument to +. 

    Built-in Functions
        Your interpreter should support a number of built-in Scheme functions. Some of these functions can take more than one argument, but you don't have to implement them with currying. You can just make these special cases inside your evaluator.
        You do not have to treat primitive functions as first-class values; they will only be invoked and applied to arguments (and not passed as arguments or used as return values).
        boolean?, number?, string?, pair?, and null?. These built-in functions return true if their single argument is a boolean, integer, string, cons cell, or null, respectively, and false otherwise.
        Integer operations. +, -, *, and = on integers.
            + sums its arguments, and should accept one or more arguments.
            - may take one or more of arguments. It subtracts its 2nd through last argument from its first argument; given only one argument, it compute unary negation. For example, (- 3) evaluates to Val_Num (-3), while (- 4 3) evaluates to Val_Num 1 and (- 4 3 1) evaluates to Val_Num 0.
            * multiplies its arguments, and should accept one or more arguments.
            = compares its two arguments and evaluates to either Val_Bool true or Val_Bool false. You may use the OCaml = operator. 
        If expressions. You should allow both (if cond tr fl), which evaluates to tr if cond is true and fl otherwise, and (if cond tr), which evaluates to Val_Null if cond is false.
        display. Prints its single argument, which is either a number or a string. Do not add a trailing newline. If a string, do not include any quotes around the string when it is printed. Display should return the value null. 

    Lists
        Your interpreter should support lists constructed from multiple calls to the cons function. The car and cdr functions may be used to extract the head and tail of a list.
            cons - Here (cons x y) should evaluate to the cons cell Val_Cons(a, b), where a and b are whatever x and y evaluate to, respectively.
            car - When applied to a cons cell Val_Cons(a, b), car returns a.
            cdr - When applied to a cons cell Val_Cons(a, b), cdr returns b. 

    Top-level Definitions
        Your evaluation function must maintain a top-level environment containing the values of variables that have been bound using the built-in define function. For example, (define x y) will bind the value of y to the symbol x.
        The top-level environment is passed to the eval function used to evaluate Scheme expressions. Scheme expressions containing unbound (free) variables may look in the top-level environment for values.
        You may assume define is used only at the top level, and not within the body of a function; we will not test your interpreter with any such examples. You also do not need to handle cases where we shadow primitive operators.
        The expression (define x ...) when evaluated returns a new top-level environment that contains the binding for x, as well as all bindings in the current top-level environment.
        Future calls to eval at the top level must be made with the new top-level environment. Your implementation of eval must use the most recent binding for x within the top-level environment, if x is a free variable.
        Remember that define may be used to bind different values (possibly with different type) to the same identifier, causing shadowing. When the value of a shadowed identifier is looked up, we return its latest definition. For instance:

          (define x 1)
          (+ x 2)  <-- produces 3
          (define x 4)
          (+ x 2)  <-- produces 6

        The actual top-level environment in Scheme supports dynamic scoping (where identifier bindings may be changed by define). For this project, your top-level environment will be like the OCaml top-level environment, where identifier bindings do not change, but may be shadowed by newer bindings to an identifier with the same name.
        You may assume that identifiers bound to user functions (lambda or dynamic) will not be shadowed (i.e., will only be defined once). 

    Identifiers
        When presented with an identifier, your interpreter needs to look it up. The first place it should look it up is in the "local" environment, consisting of parameters of enclosing user-defined functions.
        If an identifier is not in the local environment, the interpreter should look it up in the top-level environment (the one that define uses).
        If an identifier is in neither environment, then it is unbound, and trying to evaluate it should produce an error. Since we are testing your code with only valid Scheme expressions, there will not be any unbound variables in the submit server test cases.
        Scheme expressions use static lexical scoping to determine variable bindings in the local environment. For this project, user functions defined with the "dynamic" keyword will use dynamic scoping instead to determine where variables are bound in the local environment. 

    User Defined Functions
        You should support creating anonymous functions of one argument. We will only test user-defined functions called with a single argument (though functions may use currying). You need to support functions using both static (lexical) and dynamic scoping. Both types of functions produce a Val_Closure value that you must define.
        Lambda
            User-defined functions supporting static lexical scoping are defined using the lambda keyword. Function definitions must be of the form (lambda (x) body), where x is the formal parameter, and contains the code for the function. Note x must be in a list, (lambda x body) is invalid syntax. The body of the function is a single expression; it does not have to be a list. For instance, the user-defined function may be returning a simple expression (e.g., 1, x).
            To support static lexical scoping, your value for lambda must support closures. I.e., the function will need to be able to access values bound to identifiers not in the scope of the lambda expression, even if those values are bound to parameters of enclosing functions that have already returned.
            The following example shows how user-defined functions should evaluated:

              let t = List [Id "define"; Id "next";
                List [Id "lambda"; List [Id "x"]; List [Id "+"; Id "x"; Num 1]]];;
              let (Val_Define newEnv) = eval env t;;
              eval newEnv (List [Id "next"; Num 3]);;

            Here we define t to be the abstract syntax tree corresponding to the Scheme code (define next (lambda (x) (+ x 1))). In evaluating t, we first evaluate the lambda to produce a closure. Then we bind next to the closure. The last evaluation line corresponds to the Scheme code (next 3). Thus we look up next in the top-level environment to return the closure. We evaluate the argument 3, which produces Val_Num 3. Then we apply the closure to the argument to produce 4. 
        Dynamic
            User-defined functions supporting dynamic scoping are defined using the dynamic keyword. The syntax is otherwise identical to that of lambda.
            The value of a dynamic function is also stored as a Val_Closure for convenience, but it does not need to be able to access non-local variables. Instead, values for any free variables in the body of the dynamic expression should be looked up in the environment where the function is called, rather than where it is declared. (Hint, this behavior is similar to that of a closure with an empty environment).
            The following example shows the difference between functions defined with lambda and dynamic:

              (define x 1)
              (define foo (lambda (y) (+ x y)))
              (define bar (dynamic (y) (+ x y)))
              (foo 2)  <-- produces 3
              (bar 2)  <-- produces 3
              (define x 4)
              (foo 2)  <-- produces 3
              (bar 2)  <-- produces 6

        There is one case where a lambda user-defined function foo may use dynamic scoping. A free variable x in foo that is also not in the top-level environment will not be saved in the closure for foo. The function foo may still be valid, if a define expression is used to bind a value to x before foo is called. In this case, the binding for x behaves as if foo used dynamic scoping.

        This behavior is different from how OCaml's top-level environment works, and is a result of the requirement that a Scheme expression look up variable bindings in both the local and top-level environment (and that define can add bindings to the top-level environment). For OCaml, a free variable in foo that is not in the top-level environment will cause an Unbound Variable error immediately.

        The public tests include an example of this situation. In the body of the user-defined lambda function bound to fact is a recursive call to fact. Similar code in OCaml would fail unless a "let rec" was used instead of "let". This code works in Scheme by relying on the assumption fact will have been added to the top-level environment using define by the time the function is actually called. 

Part 3: Parsing Scheme

Put your solution to this part in the file scheme.ml.

Your next task is to write a parser for Scheme expressions, which in this case will be a function that turns a string into a Scheme abstract syntax tree (AST). Your parser will take as input a sequence of tokens, which are the terminals of the Scheme grammar, and output the Scheme AST.

To make the project a bit simpler, we've supplied you with a function tokenize : string -> token list that acts as a scanner/lexer, converting the string input into a list of tokens, represented by the following data type:

type token =
  | Tok_Id of string
  | Tok_Num of int
  | Tok_String of string
  | Tok_True
  | Tok_False
  | Tok_LParen
  | Tok_RParen
  | Tok_END

For example, when called as tokenize "(foo (bar 3) 4 \"baz\")", the return value is
[Tok_LParen; Tok_Id "foo"; Tok_LParen; Tok_Id "bar"; Tok_Num 3; Tok_RParen; Tok_Num 4; Tok_String "baz"; Tok_RParen]

You must write a function parse : token list -> ast that takes as input a list of tokens (returned from tokenize) and returns an AST. You should use the idea of a recursive descent parser, as we discussed in class. Thus we suggest you write two functions: parse_S, which parses the non-terminal S representing a single Scheme expression, and parse_L, which parses the non-terminal L representing a list of Scheme expressions.

You may assume that all input test cases are syntactically correct. If the input Scheme code is not legal you may perform any action (e.g., exit, throw an exception).

We will test your project by calling your parsing and evaluation functions directly, so be sure to give those functions the types we expect above. You can work on the interpreter and parser in any order, we will test each part independently. 


