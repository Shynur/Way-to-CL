# [Library] Common Lisp 中的包管理与系统定义简介

> In Common Lisp, the build system and the package manager are two separate things.

*包*拥有自己的名字, 名字可用来查找*包*:

```commonlisp
(find-package :cl-user) ; ==> #<PACKAGE "COMMON-LISP-USER">
```

*Reader* 通过名字访问符号时用到的两个关键函数是 `find-symbol` 和 `intern`, 它们默认在*当前包*中查找符号.<br>`Find-symbol` 在查找成功时返回对应的符号, 失败时返回 `nil`; `intern` 在查找失败时会在给定包中创建一个以给定字符串命名的新符号, 并将其返回:

```commonlisp
(find-symbol "cat") ; ==> nil, nil
(intern "cat") ; ==> |cat|, |cat|
(find-symbol "cat") ; ==> |cat|, :internal
(find-symbol "+") ; ==> +, :inherited
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

## `DEFPACKAGE` 宏

`Defpackage` 宏用来定义一个新的包, 但是它所进行的操作也都可以通过一些标准库函数来完成. 但它有自己的优势:

- `Defpackage` 可以采用正确的顺序执行包管理操作. 例如, 它会在 *use* 一个拥有能够引起名字冲突的符号的包之前, 将对应的符号添加至 *shadowing list*.

- `Defpackage` 使用了 `(eval-when (:compile-toplevel :load-toplevel :execute) …)` 包裹了宏展开之后的主体部分, 这将允许作者编译含有包定义的文件, 而不是只能够加载它.

```commonlisp
(defpackage :crate
  (:use :cl)) ; this package uses COMMON-LISP
```

包名由*字符串描述符 (string designator)* 给出, 这里用的是 `:crate`, 另一种和它同样以符号的形式作为字符串描述符的写法是 `#:crate`. 后者的优点是, 在整个 `defpackage` 形式被求值之后, 符号 `#:crate` 允许被当成垃圾清理掉, 这样能节省些微的内存. 然而, 性能的改善是如此的微小, 出于美学的考虑, 仍然建议写成 `:crate`.

关于规范的包名, 常见的命名法是像 Java 那样. 一个反转的 Internet 域名, 跟随一个描述性的字符串, 之间以一个单点分隔.

*Use* 并不会使被使用的包中的任何符号加入当前包的 *name-symbol table*, 只是允许作者在提及被使用的包中的 *external* 符号时, 不需要写出该符号的包名作为前缀. 换句话说, 被定义的包*继承 (inherit)*了被使用的包所导出的对外符号, 这些符号并不*存在 (present)* 于该包.

### 切换当前包

以下表达式与 `defpackage` 形式一样, 任何情形下都会被求值:

```commonlisp
(in-package :crate)
```

1. 在 *REPL* 中输入它, 将会改变 `*package*` 的值, 从而影响 *REPL* 读取后续表达式的方式.

2. 使用 `load` 加载 lisp 文件时, 效果同上.

3. 由于这个形式在编译 lisp 文件和加载 fasl 文件时都会被求值, 所以该形式之后的代码也能够被编译器正确地理解.

在 SLIME 中也可以键入逗号, 再输入 `change-package` 完成上述操作. SLIME 的 REPL 提示符默认使用包名的缩写版本.

___

## 管理符号

### 导出符号

导出被定义的包的 ***present* 符号 (存在于该包的 *name-symbol table* 中)** 使用 `:export`. 使用这个包的其它包, 将会继承这些符号.

```commonlisp
(defpackage :crate
  (:use :cl
        :box)
  (:export :apple
           :pear
           :peach))
```

### 导入符号

导入一个符号, 也就是将其加入到被定义包的 *name-symbol table* 中, 此时称该符号的状态是 *present*. 这么做一般是出于如下几个原因:

- 只是想便捷地书写某个包所导出的特定符号.

    ```commonlisp
    (defpackage :crate
      (:use :cl
            :box)
      (:import-from :pencil-case :pen 
                                 :pencil) ; import some symbols from :pencil-case
      (:export :apple
               :pear
               :peach))
    ```

- 仅当某个符号是 *present* 时, 它可以被导出.

- 仅当某个符号是 *present* 时, 它的名字可以被添加到 *shadowing list*.

一个 *present* 符号可以从对应的包*退出 (unintern)*, 这会导致它被清出 *name-symbol table*; 如果这是一个 *shadowing* 符号, 还会**将 *shadowing list* 中对应的名字清除**.

### Shadow 符号

#### `:shadow` 子句

如果你使用了一个包, 该包中仅有几个符号是你不需要的, 那么可以将那些不需要继承的名字置于 `:shadow` 子句中:

```commonlisp
(defpackage :crate
  (:use :cl
        :box)
  (:shadow :red-box
           :black-box) ; make these two symbols belonging to other packages inaccessible
  (:import-from :pencil-case :pen 
                             :pencil) ; import some symbols from :pencil-case
  (:export :apple
           :pear
           :peach))
```

这个子句做了两件事:

1. 以 `:crate` 包为*主包 (home package)* 创建了名为 `red-box` 和 `black-box` 的符号, 并直接将其添加到 *name-symbol table* 中. 这么做将会改变读取器解释后续这两个符号的方式.

2. 这个符号也会被添加到该包的 *shadowing list* 中. 这就是说, 未来在使用其它包的时候, 包系统将会知道, 这里不会产生冲突, 因为你明确地指出了 *name-symbol table* 中存在同名的符号.

#### `:shadowing-import-from` 子句

如果 `:box` 包也导出了符号 `+` 和 `-`, 与 `:cl` 包产生了冲突, 所以必须 要么直接遮蔽所有这四个符号 要么像下面这样, 选取其中的两个:

```commonlisp
(defpackage :crate
  (:use :cl
        :box)
  (:shadow :red-box
           :black-box) ; make these two symbols belonging to other packages inaccessible
  (:shadowing-import-from :box :+
                               :-) ; import some symbols from :box and make them shadowing symbols
  (:import-from :pencil-case :pen 
                             :pencil) ; import some symbols from :pencil-case
  (:export :apple
           :pear
           :peach))
```

___

## 构建系统

### 系统定义

在加载或编译那些用到 `:crate` 包的代码之前, `:crate` 包必须存在. 这个存在的意思是, 将含有包的完整定义的 lisp 文件或是 fasl 文件加载到 CL 中去. 所以, 如果你将定义 `:crate` 的代码单独分隔成文件, 那么 要么直接加载它 要么加载它编译后的产物, 然后才能加载或编译那些提及到 `:crate` 包的代码. (但是以我在 SBCL 中实验的结果, 求值 `(compile-file "crate.lisp")` 之后 `:crate` 包便存在了. 这算是一个疑点)

手工决定正确的加载顺序可能会很麻烦, 不过 CL 也有它的系统构建工具, 类似于 make 或 ant. 主流的构建系统是 [ASDF](https://lisp-lang.org/learn/writing-libraries), 它还试图只做必要的工作, 比如, 只编译那些发生了改动的文件. ASDF 不仅指示了应该如何构建, 还存储了更多关于项目的信息, 或者说是, 项目简介.

```commonlisp
(defsystem "How to use Package System"
  :description "Write some description here."
  :version "0.1"
  :license "GPL"
  :homepage "..."
  :author "Shynur <one.last.kiss@outlook.com>"
  :depends-on (:another-system-a
               :another-system-b)
  :components ((:module "src" ; `module' means a directory
                :components ((:file "packages") ; `file' means a lisp file
                             (:file "main")
                             (:module "helper-functions"
                              :components ((:file "traverse"))))))
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md")))
```

此外, 建议一个文件中仅包含一个 `in-package` 形式, 且尽可能置于所有代码之前. 因为在许多 CL 开发环境中, 诸如 SLIME 这种基于 Emacs 的, 是通过侦测 `in-package` 形式, 来决定如何与 CL 通信的. 所以, 如果单文件中存在多个 `in-package` 形式, 可能会影响这些辅助你写代码的工具.

### 交付应用程序[^Dump-an-image]

> To further complicate matters, *program* isn't really well defined in Lisp. As you've seen throughout this book, the process of developing software in Lisp is an incremental process that involves making changes to the set of definitions and data living in your Lisp image. The "program" is just a particular state of the image arrived at by loading the `.lisp` or `.fasl` files that contain code that creates the appropriate definitions and data. You could, then, distribute a Lisp application as a Lisp runtime plus a bunch of FASL files and an executable that starts the runtime, loads the FASLs, and somehow invokes the appropriate starting function. However, since actually loading the FASLs can take some time, especially if they have to do any computation to set up the state of the world, most Common Lisp implementations provide a way to *dump an image* — to save the state of a running Lisp to a file called an *image file* or sometimes a *core*. When a Lisp runtime starts, the first thing it does is load an image file, which it can do in much less time than it'd take to re-create the state by loading FASL files.
>
> Normally the image file is a default image containing only the standard packages defined by the language and any extras provided by the implementation. But with most implementations, you have a way to specify a different image file. Thus, instead of packaging an app as a Lisp runtime plus a bunch of FASLs, you can package it as a Lisp runtime plus a single image file containing all the definitions and data that make up your application. Then all you need is a program that launches the Lisp runtime with the appropriate image file and invokes whatever function serves as the entry point to the application.

___

&copy; 2022 <one.last.kiss@outlook.com>. All rights reserved.

[^Dump-an-image]: See [pcl](https://gigamonkeys.com/book/) for this section.
