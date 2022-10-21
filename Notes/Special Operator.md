# [Special Operator] 25

先作一简单列举, 这 $25$ 个分别是:

- `quote` `function` `if` `progn`
- `let` `let*` `setq` `flet` `labels` `macrolet` `symbol-macrolet`
- `block` `return-from` `tagbody` `go`
- `catch` `throw` `unwind-protect`
- `multiple-value-call` `multiple-value-prog1`
- `eval-when`
- `locally` `the` `load-time-value` `progv`

## 控制求值

`quote` 完全阻止求值, 从而获取作为数据的*S-表达式*.

`function` 则避免获取 给定名字的符号 所绑定的值, 而是取得其绑定的函数.

___

`if` 的功能同 C++ 中的 `?:` 三目运算符.

`cond` 宏构建在 `if` 之上.

当然, 在有些方言中, `cond` 是 *special operator* 而 `if` 不是, 包括 McCarthy 的初代 Lisp. 但它俩是可以相互转化的.

___

`progn` 按给定的顺序, 串联一组 *lisp 形式*.

___

## 操纵词法环境

`let` 和 `let*` 通过创建词法域, 允许你引入词法变量.

任何诸如 `do` 或 `dotimes` 这类引入了词法变量绑定的结构, 都将被展开成 `let` 或 `let*` 形式.

Technically, 类似于 `let` 形式这样的结构也可以展开成一个 $\lambda$ *表达式* (一些早期的 Lisp 实现就采用此法), 像这样:

```commonlisp
(let ((x 42))
   (1+ 42))

((lambda (x)
   (1+ 42)) 42)
```

___

`setq` 设置那些由 `let` 和 `let*` 所创建的词法绑定.

___

类似于 `let` 与 `let*`, `flet` 定义只能在其主体内访问的局部函数, 而由 `labels` 所引入的函数名可以**立即**使用 (首先想到的应当是递归). 由于 `flet` 和 `labels` 可以出现在词法域中, 所以这些局部函数也是可能成为*闭包*的.

`flet` 不能用来定义递归函数[^Y-combinator], 似乎是一种限制. 但辩证地看, 这正是它和 `labels` 之间的一种**对称的**差异, `flet` 所引入的局部函数可以调用另一个同名的函数. 像这样:

```commonlisp
(defun f () 42)

(flet ((f () (f)))
  (f)) ; ==> 42
```

它们在*宏展开*中也是很有用的:

1. 作为一种合理组织代码的方式, 可以简单地在宏定义体内定义一个局部函数. 这比定义一个仅供该宏使用但却是全局的 helper 函数要好得多.
2. 提供给用户仅能在宏的主体内调用的局部函数. 例如 CLOS 提供的 `call-next-method`, 它仅能在一个 *method* 的定义体内使用.

___

类似地, `macrolet` 提供局部宏的构造. 由它所引入的宏的名字会 shadow 外围的同名的函数和宏.

局部宏除了简单地正常使用以外, 在定义宏时也有两种惯常用法:

1. 提供给用户仅能在宏的主体内调用的宏.
2. 局部地 Shadow 某些函数或宏的名字.

`symbol-macrolet` 在局部定义了一种特殊类型的宏 —— 符号宏. 举个简单的例子:

```commonlisp
(defclass coordinate ()
  ((x :initarg :x)
   (y :initarg :y)))

(let ((p (make-instance 'coordinate :x 42 :y 233)))
  (symbol-macrolet ((x (slot-value p 'x))
                    (y (slot-value p 'y)))
    `(,x ,y))) ; ==> (42 233)
```

___

## 局部控制流

`block` 通常与 `return-from` 成对使用. `block` 用第一个名字命名块, 块中的形式按顺序求值, 其中最后一个形式求值后作为整个 `block` 形式的值;<br>`return-from` 就如同字面意思, 它的用法是这样 `(return-from block-name &optional value)`.

```commonlisp
(block earth
  (loop :for year :from 0
        :if (= year 1961)
          :do (return-from earth 'launch!)))
```

`block` 定义的块名可以是任何符号 (包括 `nil`). 例如:

- 诸如 `do`, `dotimes` 和 `dolist` 这类宏, 都将用户代码包装在一个名为 `nil` 的块中, 这将允许用户使用 `return` 宏跳出循环 (该宏是 `(return-from nil &optional value)` 的语法糖).

- `defun`, `flet` 和 `labels` 等则会将它们定义的函数体封装在与函数同名的 `block` 中.

___

`tagbody` 通常与 `go` 成对使用. 注意, 任何出现在 `tagbody` 形式的顶层的符号都会被当成 tag. 用法如下:

```commonlisp
(tagbody
  a (print 'a) (when (zerop (random 2))
                 (go a))
  b (print 'b) (when (zerop (random 2))
                 (go a)))
```

由于 `tagbody` 和 `go` 这对操作符与其它语言中的 `goto` 极为相似, 在将其它语言的算法转译成 CL 时会很方便[^f2cl].

___

## 栈上回退

鸽

___

## 多值

少数宏并不会传递子形式. 尤其是 `prog1`, 它像 `progn` 那样求值, 但仅返回第一个子形式的主值; 类似地, `prog2` 仅返回第二个子形式的主值.

`multiple-value-prog1` 可以看作 `prog1` 的变体, 它返回的是多值.

另外, `or` 和 `cond` 也并不总是会返回多值. 对于特定的子形式, 可能只有主值被使用.

___

返回多值通常使用函数 `values` 或函数 `values-list`, 且有 `(values-list values) #| === |# (apply #'values values)`.

`multiple-value-call` 与 `funcall` 相似, 具体如下:

```commonlisp
(funcall #'+ (values 6 9)) ; ==> 6
(multiple-value-call #'+ (values 6 9)) ; ==> 15
```

有关多值的使用, 更常见的是下面这两个宏:

- ```commonlisp
    (multiple-value-bind (x y) (values 'Kaneki-Ken 'Sasaki-Haise)
      `(,x ,y)) ; ==> (KANEKI-KEN SASAKI-HAISE)
    ```

- ```commonlisp
    (multiple-value-list (values 'Kaneki-Ken 'Sasaki-Haise)) ; ==> (KANEKI-KEN SASAKI-HAISE)
    ```

最后, `(setf values)` 函数提供 解构多值 的功能:

```commonlisp
(let (x y)
  (setf (values x y) (values 'c++ 'lisp))
  (multiple-value-list (values x y))) ; ==> (C++ LISP)
```

___

## `EVAL-WHEN`

___

## Others[^the-other-special-operators]

`locally` 和 `the` 是 CL *声明系统* 的一部分, 用于与编译器沟通 而不会影响代码的语义. 主要是帮助生成更好的代码, 比如, 更好的性能, 更清晰的错误信息, 等等.



___

[^Y-combinator]: Maybe surprisingly, 但匿名函数确实有可能成为递归的. 为此, 必须使用一种古怪的手法 ---- *Y 组合子*. 这是一种理论产物, 并非实用工具.

[^f2cl]: 一个例子是将 Fortran 转译到 CL 的 f2cl 转译器, 这使得 Lisper 可以使用某些 Fortran 库. 许多 Fortran 库写于结构化编程革命以前, 所以代码中充斥着 `goto` 语句. 相比之下, f2j (Fortran 到 Java 的转译器) 就繁琐得多: 尽管 JVM 提供 `goto` 指令, 但并没有直接暴露给 Java, 为此, f2j 先借助*哑类 (dummy class)* 表示标签和跳转调用, 将 Fortran 转译成 Java 源码, 再对编译成字节码的 Java 源码进行后处理, 把那些代表对哑类的调用的字节码修正为合适的字节码.

[^the-other-special-operators]: `locally`, `the`, `load-time-value` 和 `progv` 都将允许你访问 以任何其它方式都无法访问的 语言的底层.
