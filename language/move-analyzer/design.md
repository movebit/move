## The design of move-analyzer2
move-analyzer2 is a new design for move lanugage IDE support.

move-analyzer2 have a build in semantic analyzer for move and MSL.

First let's look at some core structure of move-analyzer.

### `Item`
Item is some you can define in you program. and used it later somewhere.
variable,paramater,typeparameter,and ... are all items.

### `Scope`

Scope means some scope you can define variable,function,etc.

For instance module is scope, and function is a scope too.
~~~
// module is scope,So you can define a function in it.
0x1::some_module{
    // here you can define function,const,...
    fun some_fun() {
        // function is scope too, you can create variable and ... 
    }
}
~~~

### `Scopes`

`Scopes` composed with two important field(`scopes` and `addresses`) and someother addtional information.

`scopes` is a stack of `Scope`.

`scopes` works like function calls.
push a frame on stack when you want call a function.

pop out a frame when function returns.

For instance.
~~~
fun some_fun() {
    
    {   // here is a scope.
        // push a `Scope` when enter so we can hold 
        // current block declared variables.
        let _x = 100;

        // at end we pop out a `Scope`. 
    }

}
~~~
`addresses` are just Global `Scope` which contains global struct definition , function definition.

For instance.
~~~
0x1::some_module { 
    fun some_fun() {  // some_fun will saved in `addresses` and can be accessed somehow.

    }
}
~~~

### `ResolvedType`
`ResolvedType` is a type have semantic meanings.
It's a type resovled from user defined.
For instance.
~~~
struct XXX {}
fun some_fun() : XXX  // XXX will resovled to ResolvedType::Struct which will contains information of the structure too.
{
    
}
~~~


### `Access`
`Access` mean a access point to a Item.
When you define a variable ,you must be used it somehow.
For instance.
~~~
fun some_fun() {
    let x = 1;

    some_fun2(x); // When we dealing with access of x 
                  // We have a structure below 
                  Access::ExprAccessChain(
                    NameAccessChain,  // access point.
                    Option<AddrAndModuleName>,  // The item maybe locate at some module,So we can implement goto to definition,... for module.
                    Box<Item>, // The actual Item.
                ) 
    ), 
~~~

### `Project`
`Project` represents a loaded project from a `Move.toml`.
`Project` is the most complex part of move-analyzer2.

First let me introduce `ScopeVisitor`.
~~~

pub trait ScopeVisitor: std::fmt::Display {
    /// Handle this item_or_access.
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        scopes: &Scopes,
        item_or_access: &ItemOrAccess,
    );
    
    ... 

    /// Visitor should finished.
    fn finished(&self) -> bool;
}
~~~
Actual the `ScopeVisitor` is a consumer and can consume the information create by `Project`.

`ItemOrAccess` is either a `Item` or `Access`. Our `goto to definition` and `auto completion` ,... base On `ScopeVisitor`.

For Instance 

When you want to implement `goto to definition`.
* if the `item_or_access` is `Item` you just return the `def_loc` of the `item_or_access`.
* if the `item_or_access` is `Access` you just return the `def_loc` of the `item_or_access`'s item.

So the core purpose of `Porject` is to produce `ItemOrAccess`.

Let me introduce How is `Project` is create.
`Project` creation involves  next steps.

- loading AST and depency's AST into Memory.
- enter all the global function,const,and struct to `Scopes`.`addresses`.

Wait,But How can we do that.

The main entry point for `Project` to enter item and call `ScopeVisitor`.`handle_item_or_access...` is `visit_module`.
~~~
pub fn visit_module(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        provider: impl AstProvider,
    ) {
        ... 
    }

~~~

Let me first introduce `AstProvider`.

`AstProvider` is trait than have a lot of with function.
~~~
fn with_const(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Constant)) {
    ... 
}

fn with_struct(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition)) {
    ... 
}
~~~
This is convenient way for function `visit_module` to access(We don't want to  iter `Vec<Definition>`);
And the trait `AstProvider` provides us a way only visit part of the project's AST, we will talk about it later.

function `visit_module` is reponsible for itration of all AST,create `ItemOrAccess`,enter `Item`,and call `ScopeVisitor`'s method.

For Instance.
~~~
pub fn visit_module(
        &self,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
        provider: impl AstProvider,
    ) {
        ... 
        provider.with_const(|addr, name, c| {
            self.visit_const(Some((addr, name)), c, scopes, visitor);
        });
        ...
}

 pub(crate) fn visit_const(
        &self,
        enter_top: Option<(AccountAddress, Symbol)>,
        c: &Constant,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        ... 
        // Get const's ty
        let ty = scopes.resolve_type(&c.signature, self);
        // Create the ItemOrAccess
        let item = ItemOrAccess::Item(Item::Const(ItemConst {
            name: c.name.clone(),
            ty,
            is_test: attributes_has_test(&c.attributes).is_test(),
        }));
        // Call visitor's handle_item_or_access method.
        // In this case this is a `ItemOrAccess::Item`
        visitor.handle_item_or_access(self, scopes, &item);
        let item: Item = item.into();
        // enter the `Item` into Scope.
        if let Some((address, module)) = enter_top {
            
            scopes.enter_top_item(self, address, module, c.name.value(), item.clone(), false);
        } else {
            scopes.enter_item(self, c.name.value(), item);
        }
    }
~~~

So create `Porject` basic contains two part `Load all the AST` and call `visit_module` build all global item.


### Go-through-a-typcial IDE feature been implemented.




