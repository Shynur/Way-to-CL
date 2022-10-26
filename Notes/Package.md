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

