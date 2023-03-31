## Implementing a language formatter with less work.

implementing a formatter for programming language always with a lot of work.

first of all a language AST has a lot of variant data structure.

like move expression.

~~~
    Value(Value),
    // move(x)
    Move(Var),
    // copy(x)
    Copy(Var),
    // [m::]n[<t1, .., tn>]
    Name(NameAccessChain, Option<Vec<Type>>),

    // f(earg,*)
    // f!(earg,*)
    Call(NameAccessChain, bool, Option<Vec<Type>>, Spanned<Vec<Exp>>),

    // tn {f1: e1, ... , f_n: e_n }
    Pack(NameAccessChain, Option<Vec<Type>>, Vec<(Field, Exp)>),

    // vector [ e1, ..., e_n ]
    // vector<t> [e1, ..., en ]
    Vector(
        /* name loc */ Loc,
        Option<Vec<Type>>,
        Spanned<Vec<Exp>>,
    ),

    // if (eb) et else ef
    IfElse(Box<Exp>, Box<Exp>, Option<Box<Exp>>),
    // while (eb) eloop
    While(Box<Exp>, Box<Exp>),
    // loop eloop
    Loop(Box<Exp>),

    // { seq }
    Block(Sequence),
    // fun (x1, ..., xn) e
    Lambda(BindList, Box<Exp>), // spec only
    // forall/exists x1 : e1, ..., xn [{ t1, .., tk } *] [where cond]: en.
    Quant(
        QuantKind,
        BindWithRangeList,
        Vec<Vec<Exp>>,
        Option<Box<Exp>>,
        Box<Exp>,
    ), // spec only
    // (e1, ..., en)
    ExpList(Vec<Exp>),
    // ()
    Unit,
~~~
This is just `Expression` variants.

There are also `Function`,`Module`,`Struct`,etc.

Implement a formatter you have to deal all the data structure.


Another complex thing about formatter is `Comments`.
Most programming language support two forms of comments.
* Line comment  like // This is comments.
* Block comments     /*  This is comments */

The tricky part is `Block comments`. 
`Block comments` can write anywhere in source code.

In order to keep user's comments you have to keep comments in AST like below.
~~~
    // f(earg,*)
    // f!(earg,*)
    Call(NameAccessChain,
        Vec<Comment> , // keep in AST.
        bool, Option<Vec<Type>>, Spanned<Vec<Exp>>),
~~~
This will make things below ugly.
* Ast Definition.
* parse AST.
* all routine that handle AST.


In general We need keep a `Comments` in every AST structure.


Is there a way to slove the puzzle.

