---
layout: post
title: 什么是一等公民
date: 2016-04-08T20:51:54+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [first-class citizens, 一等公民]
image:
  feature:
comments: true
share: true
---

介绍函数式编程语言的书籍里常常提到，函数是一等公民（first-class citizens）。那么什么是一等公民呢？什么又不是一等公民？

其实很多时候名字已经透露了太多的信息。有一等公民，就有二等公民，三等公民。层级越高，权力越大。很多时候高层的人有权利做的事，下面的人就不能做。

程序世界里，有且不仅有这么几种权力。创建，赋值，传递。

这些权力，object 都具备，function 都不具备。对象可以通过参数传递到另一个对象里，从而两个对象可以互相通信。函数却不行，两个函数想要通信，必须以对象为介质。

以 Java 举个例子：函数a，想要调用函数b。虽然a并不关心函数b是从哪儿来的，只要函数b可以完成这个特定的功能即可。但是在 Java 的世界里函数必须要依附在对象上，所以函数a必须依附在对象A上，函数b必须依附在对象B上，函数a必须通过一个对象才能找到函数b，如下：

```java
public class A {
	public voidd a(Object o) {
		System.out.println("a is invoked");
		o.getClass().getMethod("b").invoke(o);
   }
}

public class B {
	public void b() {
		System.out.println("b is invoked");
	}
}
```

函数b可以这样传递给函数a：

```java
new A().a(new B());
```

结果如下：

```
a is invoked
b is invoked
```

通过这个简单的例子，你可以看出，非一等公民的函数生存条件有多么的恶劣，通讯的阻力有多大。

下面再来看一个函数是一等公民的 Common Lisp 的世界里，怎么去定义两个函数

```common_lisp
(defun a (b) (format t "a is invoked~%") 
             (funcall b))
(defun b () (format t "b is invoked"))
```

函数b可以这样传递给函数a：

```common_lisp
(a #'b)
```

结果如下：

```
a is invoked
b is invoked
```

在函数是一等公民的世界里，函数a可以不再依附于对象A而单独存在，函数a可以直接与函数b交流，不再需要通过对象才能找到函数b。这里真是一个函数的天堂。

所以一等公民即在该世界里限制最少的角色。


