 

不理解的msl语法
Invariant  看起来可以声明范型参数，这个在那使用呢。
Variable   也有范型参数，看起来有点诡异。还有is_global看起来就是在指是不是全局的。
forall 有返回值吗？返回值是最有一个语句？ 返回值只能是true或者false这种bool值？

~~~
forall <binding>, ..., <binding> [ where <exp> ] : <exp>
~~~



goto type definition有问题





download_deps_for_package 可以手动下载依赖


### 2023-3-16
这里可能是spec module解析成功
但是module解析失败了
有些数据 不完整
~~~
ERROR - looks like impossible addr:0x"1" module:"table" item:StructName("Table") x:Table not struct def.
ERROR - looks like impossible addr:0x"1" module:"table" item:StructName("Table") x:Table not struct def.
thread 'main' panicked at 'called `Option::unwrap()` on a `None` value', language/move-analyzer/src/project_context.rs:324:18
stack backtrace:
   0: rust_begin_unwind
             at /rustc/69f9c33d71c871fc16ac445211281c6e7a340943/library/std/src/panicking.rs:575:5
   1: core::panicking::panic_fmt
             at /rustc/69f9c33d71c871fc16ac445211281c6e7a340943/library/core/src/panicking.rs:65:14
   2: core::panicking::panic
             at /rustc/69f9c33d71c871fc16ac445211281c6e7a340943/library/core/src/panicking.rs:115:5
   3: core::option::Option<T>::unwrap
             at /rustc/69f9c33d71c871fc16ac445211281c6e7a340943/library/core/src/option.rs:778:21
   4: move_analyzer::project_context::ProjectContext::enter_top_item
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_context.rs:317:13
   5: move_analyzer::project_visitor::<impl move_analyzer::project::Project>::visit::{{closure}}::{{closure}}
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_visitor.rs:176:17
   6: move_analyzer::project_context::ProjectContext::enter_scope
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_context.rs:227:17
   7: move_analyzer::project_visitor::<impl move_analyzer::project::Project>::visit::{{closure}}
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_visitor.rs:129:13
   8: move_analyzer::project_visitor::<impl move_analyzer::project::Project>::visit::{{closure}}
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_visitor.rs:275:25
   9: move_analyzer::project::AstProvider::with_spec::{{closure}}
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:1209:38
  10: move_analyzer::project::AstProvider::with_module_member::{{closure}}
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:1144:21
  11: <move_analyzer::project::ModulesAstProvider as move_analyzer::project::AstProvider>::with_definition
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:1320:17
  12: move_analyzer::project::AstProvider::with_module_member
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:1141:9
  13: move_analyzer::project::AstProvider::with_spec
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:1208:9
  14: move_analyzer::project_visitor::<impl move_analyzer::project::Project>::visit
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_visitor.rs:250:9
  15: move_analyzer::project_visitor::<impl move_analyzer::project::Project>::run_full_visitor
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project_visitor.rs:857:13
  16: move_analyzer::project::Project::new
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/project.rs:71:9
  17: move_analyzer::context::MultiProject::load_project
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/context.rs:88:23
  18: move_analyzer::on_notification
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/bin/move-analyzer.rs:344:27
  19: move_analyzer::main
             at /Volumes/sanDisk/projects/move/language/move-analyzer/src/bin/move-analyzer.rs:200:34
  20: core::ops::function::FnOnce::call_once
             at /rustc/69f9c33d71c871fc16ac445211281c6e7a340943/library/core/src/ops/function.rs:251:5
note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace.
thread '<unnamed>[Error - 3:57:21 PM] Connection to server got closed. Server will not be restarted.
~~~