---
layout: post
title: 说人话 - 指代宏
date: 2016-06-27T19:02:20+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [指代宏, lisp]
image:
  feature: post/anaphoric-if/lisp.jpg
comments: true
share: true
---

## 指代
>你喜欢作文，而我只读诗。

指代，即对事物的引用。英语`Get the wrench and put it on the table.`中`it`就是上文`the wrench`的指代。`Java`中`this`是当前对象的一个指代。  

你可以轻易想到，指代为语言表达带来的便利。特别是当指代的内容比较复杂的时候!  

在编程时，复杂通常表现为计算量会比较大或者等待时间长，通常我们会将这些复杂过程的结果暂存，然后在后续过程直接使用该结果，避免了对复杂过程的重复调用，如：

```common_lisp
(let ((result (big-long-calculation)))
  (if result
    (foo result)))
```

`JS`版（函数命名与`CL`中的不同仅仅因为编程风格）:

```javascript
var result = big_long_calculation();
if (result) {
	foo (result);
}
```

用自然语言重述上述逻辑是这个样子的：  
计算`big—long-calculation`，并用`result`表示结果，如果`result`为真，在`result`上做`foo`操作。  

以这样的方式写下去，应该可以快速的满足语文考试最后一题作文的第一要求: 字数不少于**800**字。  

换一种写法，是否读起来更加紧凑：  
当`big-long-calculation`计算结果为真时，在`it`的上做`foo`操作。  
如果用程序语言翻译过来，应如下：

```common_lisp
(if (big-long-calculation)
  (foo it))
```
`JS`版（函数命名与`CL`中的不同仅仅因为编程风格）:

```javascript
if (big_long_calculation()) {
	foo (it);
}
```
那么，如何让`it`指代`big-long-calculation`的结果呢？

## 宏
> 从那一刻开始，我才知道，有些人已经可以编码这个世界本身。

这里没有 magic，想要指代，必然存在代词与实体的绑定。人类可以自行脑补，而 Common Lisp 有**变量捕捉**和**宏**。  
显然`it`在“内置”的`if`中是找不到想要的实体的，让我们给`it`创造一个更加合适的舞台 —— Anaphoric if, 简称`aif`：

```common_lisp
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))
```
在这个舞台上，你可以用程序语言说出本只有人类才能理解的话：

```common_lisp
(aif (big-long-calculation)
  (foo it))
```
这段代码的结果和文首代码的结果完全一样，但后者有前者无法比拟的紧凑性。后者更像诗。

看到这个宏调用，容易让人误会的是，认为这个`it`指代是一个全局变量。但你会发现，直接在`toplevel`使用，会得到一个`The variable IT is unbound.`的错误。上述代码之所以正常工作，得益于`aif`宏使其`it`在一个新的词法环境下被求值。

如果`defun`是在扩展词汇，那么`defmacro`就是在扩展语法。  
用`macroexpand-1`将上述宏调用展开，你会看到这样的代码：

```common_lisp
CL-USER> (macroexpand-1 '(aif (big-long-calculation) (foo it)))
(let ((it (big-long-calculation)))
  (if it
    (foo it)
    nil))
```
请注意，这段代码和文首那段代码虽然长得一样，但却存在巨大的不同——**来源**。前者是手写的，而后者是`aif`生成的。这就是宏——**生成代码**的代码！

## 代码既数据
如果函数式编程给程序员以抽象过程的机会，那么宏就提供了抽象程序的机会。在`lisp`中`程序就是数据`，使得修改程序就像修改数据一样方便。在这个舞台上，宏给了你一个轻易地将`lisp`变成你想要的样子。这样的编程更像是我们在改造语言本身，而不是在编写一个新的程序。然后，你会发现`lisp`早已成为你目标程序最自然的载体。
