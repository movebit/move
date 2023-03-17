###  自动生成Modifies Condition 代码
    https://github.com/move-language/move/blob/main/language/move-prover/doc/user/spec-lang.md#modifies-condition
    去查询代码种是否有borrow_global_mut
###  存在requires调用链的函数自动生成requires
    https://github.com/move-language/move/blob/main/language/move-prover/doc/user/spec-lang.md#requires-condition
    感觉很难实现
### 函数调用不为纯函数自动生成spec fun
    什么是纯函数，这个可能需要给出判断的条件 才能做    
###  针对加法 减法 移位等运算自动生成 aborts_if
    感觉能做
###  针对循环生成循环不变量
     什么是循环不变量，要生成什么样的spec
###  判断数组长度等move函数要转换为对应的msl函数
    这个也是要生成requires？？
###  自动针对反射生成aborts_if
    type_of type_name 可能会导致异常
###  自动针对simple_map  table生成spec
    具体是那些函数是特殊处理呢
###  被很多函数调用的函数自动生成schema(根据配置:例如被调用3次以上就自动生成)
    这种还是用户自己来选择函数来生成schema的好