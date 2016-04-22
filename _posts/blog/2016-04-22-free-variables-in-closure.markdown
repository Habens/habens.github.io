---
layout: post
title: 闭包中的自由变量
date: 2016-04-22T16:52:05+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [自由变量, 闭包, free, varible, closure]
image:
  feature: post/free-variables-in-closure/inception.jpg
comments: true
share: true
---
> 挡住了去路的，往往不是大树，而是小藤 —— 于渊

## 疑问
在复习《**On Lisp**》闭包时，偶然发现一些以前没有注意到的现象：

我的 `lisp` 实现是 `SBCL` `1.3.2`

以下是书中闭包的例子：

```common_lisp
(let ((y 7))
  (defun scope-test (x)
  (list x y)))
```

编译该函数，如书中所说

```common_lisp
(scope-test 3)
(let ((y 5)) (scope-test 3))
```

的结果都是：`(3 7)`。

但如果我先定义了全局变量 `y`

```common_lisp
(defvar y 10)
```

然后再编译该闭包，

```common_lisp
(scope-test 3)
```

的结果变成了 `(3 10)`；

```common_lisp
(let ((y 5)) (scope-test 3))
```

的结果变成了：`(3 5)`。

这当然和 `SBCL` 对自由变量 `y` 的处理有关。但是不太清楚背后的逻辑。


## 解释

### 第1种情况

在定义这个闭包时，`y` 处于 `let` 创建的词法空间内，`y` 是词法作用域（scope），动态生命周期（extent）。`defun` 创建（establishing）的 `scope-test` 函数处于 `let` 创建的词法空间内。

所以在执行 `(scope-test 3)` 时，变量 `y` 先从 `scope-test` 执行时的词法空间里找值绑定，但是没有找到，然后在 `scope-test` 函数创建的词法空间里找到了 `7` 这个绑定，所以结果为 `(3 7)`。

同理，虽然 `(let ((y 5)) (scope-test 3))` 里的 `let` 又创建了一个词法空间。在执行这个表达式时，`y` 的值绑定先找到的是定义 `scope-test` 函数时的变量绑定 `7`。 所以，结果是 `(3 7)`。

### 第2种情况

如果我用 `defvar` 定义了一个 `special` 变量 `y`。`y` 是`动态作用域`（不定（indefinite）作用域，动态生命周期）。

在执行 `(scope-test 3)` 时，变量 `y` 先从当前栈空间去找值绑定，但是没有找到，然后在调用栈的空间里去找，找到了 `7` 这个值，所以结果为 `(3 7)`。

在执行 `(let ((y 5)) (scope-test 3))` 时，变量 `y` 先从当前栈空间去找值绑定，但是没有找到，然后在由 `let` 所处的栈空间去找，找到了 `5` 这个值，所以结果为 `(3 5)`。
