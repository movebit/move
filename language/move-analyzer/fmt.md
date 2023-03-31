## Implementing a language formatter with less work.


Implementing a formatter for programming language always with a lot of work.

A formatter just print a AST to a string.

first of all a language AST always has a lot of variant data structure.

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
* Line comment       // This a is comment.
* Block comment     /*  This a is comment. */

The tricky part is `Block comment`. 


`Block comment` can write anywhere in source code.

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
* all routine that accept AST.

In general We need keep a `Vec<Comment>` in every AST structure.


Is there a way to slove this puzzle.

The key idea about this post to simplfy `AST` to far more simpler tree type which I call it `TokenTree`.
~~~
function(a) {
    if (a) { 
        return 1
    } else {
        return 2
    }
}
~~~

`TokenTree` mainly contains two category.

* `SimpleToken` in previous code snippet,`if`,`return`,`a` are `SimpleToken`.
* `Nested` in previous code snippet,`()` and `{}` will form a `Nested` Token.

So a source program may represents like this.

~~~
pub enum TokenTree {
    SimpleToken {
        content: String,
        pos: u32,
    },
    Nested {
        elements: Vec<TokenTree>,
        kind: NestKind,
    },
}

pub type AST = Vec<TokenTree>;
~~~

Instead of dealing a lot data structure we simple the puzzle to dump `Vec<TokenTree>`.

`TokenTree` is just another very simple version of `AST`.

`TokenTree` is very easy to parse,simple as.
~~~
...
if(tok == Tok::LParent){ // current token is `(`
    parse_nested(Tok::LParent);    
}
...
~~~

Right now verything look fine.
But There are some token can be both `SimpleToken` or `Nested`.

typical for a language support type parameter like `Vec<X>`.

A Token `<` can be `type parameter sign` or `mathematic less than`.

This can be solved by consule the `AST` before parse `TokenTree`.

because we are writting a formatter for existed programming language.
It is always easy for us to get the real `AST`.
We can traval the `AST` the decode a token is either a `SimpleToken` or `Nested`.

## how to generate base on `TokenTree`.

`Vec<TokenTree>` is a tree type, It is very easy to decide how many ident,etc.




