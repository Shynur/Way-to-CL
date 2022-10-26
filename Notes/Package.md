# [Package] Common Lisp 中的包管理

*包*拥有自己的名字, 名字可用来查找*包*:

```commonlisp
(find-package :cl-user) ; ==> #<PACKAGE "COMMON-LISP-USER">
```

*Reader* 通过名字访问符号时用到的两个关键函数是 `find-symbol` 和 `intern`, 它们默认在*当前包*中查找符号.<br>`Find-symbol` 在查找成功时返回对应的符号, 失败时返回 `nil`; `intern` 在查找失败时会在给定包中创建一个以给定字符串命名的新符号, 并将其返回:

```commonlisp
(find-symbol "cat") ; ==> nil, nil
(intern "cat") ; ==> |cat|, |cat|
(find-symbol "cat") ; ==> |cat|, :internal
```

想知道**当前包使用了哪些包**? 使用 `package-use-list`:

```commonlisp
(mapcar #'package-name (package-use-list :cl-user)) ; ==>
; ==> ("COMMON-LISP" "SB-ALIEN" "SB-DEBUG" "SB-EXT" "SB-GRAY" "SB-PROFILE")
```

想知道给定**符号来自哪个包**? 使用 `symbol-package`:

```commonlisp
(package-name (symbol-package :Elysia)) ; ==> "KEYWORD"
```

___

## 定义包

### 简介 `defpackage` 宏

`Defpackage` 宏用来定义一个新的包, 但是它所进行的操作也都可以通过一些标准库函数来完成. 但它有自己的优势:

- `Defpackage` 可以采用正确的顺序执行包管理操作. 例如, 它会在 *use* 一个拥有能够引起名字冲突的符号的包之前, 将对应的符号添加至 *shadowing list*.
- `Defpackage` 使用了 `(eval-when (:compile-toplevel :load-toplevel :execute) …)` 包裹了宏展开之后的主体部分, 这将允许作者编译含有包定义的文件, 而不是只能够加载它.

```commonlisp
(defpackage :crate
  (:use :cl)) ; this package uses COMMON-LISP
```

包名由*字符串描述符 (string designator)* 给出, 这里用的是 `:crate`, 另一种和它同样以符号的形式作为字符串描述符的写法是 `#:crate`. 后者的优点是, 在整个 `defpackage` 形式被求值之后, 符号 `#:crate` 允许被当成垃圾清理掉, 这样能节省些微的内存. 然而, 性能的改善是如此的微小, 出于美学的考虑, 仍然建议写成 `:crate`.

*Use* 并不会使被使用的包中的任何符号加入当前包的 *name-symbol table*, 只是允许作者在提及被使用的包中的 *external* 符号时, 不需要写出该符号的包名作为前缀. 换句话说, 被定义的包继承了被使用的包所导出的对外符号, 这些符号并不*存在 (present)* 于该包.

### 切换当前包

以下表达式与 `defpackage` 形式一样, 任何情形下都会被求值:

```commonlisp
(in-package :crate)
```

1. 在 *REPL* 中输入它, 将会改变 `*package*` 的值, 从而影响 *REPL* 读取后续表达式的方式.
2. 使用 `load` 加载 lisp 文件时, 效果同上.
3. 由于这个形式在编译 lisp 文件和加载 fasl 文件时都会被求值, 所以该形式之后的代码也能够被编译器正确地理解.

在 SLIME 中也可以键入逗号, 再输入 `change-package` 完成上述操作. SLIME 的 REPL 提示符默认使用包名的缩写版本.

### 管理符号

#### 导出符号

导出被定义的包的 ***present* 符号 (存在于该包的 *name-symbol table* 中)** 使用 `:export`. 使用这个包的其它包, 将会继承这些符号.

```commonlisp
(defpackage :crate
  (:use :cl
        :box)
  (:export :apple
           :pear
           :peach))
```

#### 导入符号

#### Shadow 符号

___

