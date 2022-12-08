
### 模糊测试是一种找到软件安全漏洞的方法

<https://www.cnblogs.com/forwill/p/5756634.html>

由于虚拟机里面的数据结构众多，数据和数据的关联比较强，
很难为所有的数据结构生成fuzzing，然后调用成员方法之类的来测试安全性，即使这样崩溃了，也不能说明move的虚拟机存在问题。
因为虚拟机流水线很长，流水线的下一阶段必定依赖上一阶段的数据。
如果完全fuzzing可能数据完全都对应不上。

鉴于move-bytecode-verifier也有fuzzing。我们可以参考他的实现。

~~~
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#![no_main]
use libfuzzer_sys::fuzz_target;
use move_binary_format::file_format::CompiledModule;

fuzz_target!(|module: CompiledModule| {
    let _ = move_bytecode_verifier::verify_module(&module);
});
~~~

我们只需要fuzzing入口函数的input即可，比如上述的验证module是否合法的verify_module。从理论上讲我们fuzzing的次数足够多，我们最终就可能走到所有的执行路径。

move-vm这个crate提供服务主要的struct是。

~~~
/// 虚拟机对象。
pub struct MoveVM {
    runtime: VMRuntime,
}
/// 这个方法可以得到一个Session
/// 我们可能需要自己去实现MoveResolver，把我们自己fuzzing的module加载到runtime里。
pub fn new_session<'r, S: MoveResolver>(&self, remote: &'r S) -> Session<'r, '_, S> {
    self.runtime.new_session(remote)
}
~~~

~~~
pub struct Session<'r, 'l, S> {
    pub(crate) runtime: &'l VMRuntime,
    pub(crate) data_cache: TransactionDataCache<'r, 'l, S>,
    pub(crate) native_extensions: NativeContextExtensions<'r>,
}
session对象有很多对外接口 execute_entry_function execute_script...
调用即可。
~~~

### 一点点fuzz技巧

好像move-bytecode-verifier里面的compiled_module覆盖率很低，
如果直接去fuzzing整个module，那在验证的时候很快就返回了，很多代码测试不到的，基本上整个crate的覆盖率基本都是0,只有一两个文件的覆盖率比较高。

code_unit覆盖率较高，因为这个fuzzing的是手工生成的module，
module的基础结构是正确的，只是fuzzing代码部分，如果我们要测试vm的话，最多的测试可能就是我们有一个基本的module是合法的，(一个基本合法的函数？)
只是去fuzzing某一个函数或者代码。
