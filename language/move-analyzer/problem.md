 

不理解的msl语法
Invariant  看起来可以声明范型参数，这个在那使用呢。
Variable   也有范型参数，看起来有点诡异。还有is_global看起来就是在指是不是全局的。
forall 有返回值吗？返回值是最有一个语句？ 返回值只能是true或者false这种bool值？

~~~
forall <binding>, ..., <binding> [ where <exp> ] : <exp>
~~~



goto type definition有问题





download_deps_for_package 可以手动下载依赖





module 0x2::xxx{
   
}


#[test_only]
module 0x2::xxx {
    struct Box<T> has copy, drop, store { x: T }
    struct Box3<T> has copy, drop, store { x: Box<Box<T>> }
    struct Box7<T> has copy, drop, store { x: Box3<Box3<T>> }
    struct Box15<T> has copy, drop, store { x: Box7<Box7<T>> }
    struct Box31<T> has copy, drop, store { x: Box15<Box15<T>> }
}


