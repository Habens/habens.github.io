---
layout: post
title: 作用域与生命周期
date: 2016-04-20T22:40:17+08:00
author: habens_chen
modified:
categories: blog
excerpt: 原文链接 - https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node43.html#SECTION00700000000000000000
tags: [scope, extent, 作用域, 生命周期, Lisp]
image:
  feature:
comments: true
share: true
---
[原文链接](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node43.html#SECTION00700000000000000000)

在描述 `Common Lisp` 语言特性时，**作用域**与**生命周期**这两个概念往往是很有用的。这些概念在必须引用一些外部对象或结构时被引出。**作用域**指的是在一个程序中可能发生引用的时间和空间区域。**生命周期**指的是在一个程序中可能发生引用的时间段。

举一个简单的例子，请看下面的程序：

```common_lisp
(defun copy-cell (x) (cons (car x) (cdr x)))
```

参数 x 的作用域是整个 `defun` 形式 (form) 内（body），在 `defun` 外，没有任何办法去引用 x。同样，参数 x （调用 copy-cell 的实参）的生命周期是从这个函数被调用到退出的时间。（一般情况下，参数的生命周期可能超过函数退出时间，但在这个简单的例子里面不会发生。）

在 Common Lisp 中，在执行一些语言结构时会**创建**（established）一个可引用的实体，该实体的作用域和生命周期都是相对于该结构和该实体被创建的时间（在执行该结构的时间段内）来说的。这里有必要指出，这个**实体**不仅仅指 Common Lisp 的数据对象，比如 **symbols** 和 **conses**，而且包含 **变量绑定**（variable bindings）（一般变量和特殊变量都包括），**catchers**，和 **go 标签**。重要的是要区分清楚**实体**和**实体的名字**。就像下面这个函数定义

```common_lisp
(defun foo (x y) (* x (+ y 1)))
```

无论什么时候调用这个函数，这里都只有一个单一的名字来表示该程序的第一个参数；但是，**每次**调用都会创建一个新的**绑定**（binding）。一个**绑定**是一个特定的参数实体。x 所引用的值依赖的不仅仅在函数调用时的作用域，而且还依赖于特定的绑定和被调用的实体。在本章的末尾有更加复杂的例子

在 **Common Lisp** 中，这里有几种特别有用的作用域和生命周期：

* **词法作用域**。对已创建实体的引用只可能发生在该创建结构的词法范围内。一般结构都会包含名为 body 的部分，所有创建的实体的作用域都将是（或者包括）这个 body。  
例如：函数的参数名的作用域通常是词法作用域。
* **不定作用域**。引用可以发生在任何程序的任何地方。
* **动态生命周期**。引用可能发生在实体创建到销毁期间的任何时刻。作为一个规则, 实体在创建的结构运行完成后或者在其他地方被终结时被销毁。因此具有动态生命周期的实体遵循栈氏规则，把嵌套运行的那些创建结构并联起来。  
例如：`with-open-file` 结构建立了与一个文件的联系，然后创建了一个流对象来表示这个关联。这个流对象拥有不定生命周期，但是与被打开文件的关联是动态生命周期的：当控制退出 with-open0file 结构，不论是否正常，这个流都会被自动关闭。
* **不定生命周期**。只要有可能引用仍然存在实体就继续存在。（如果可以证明没有任何引用指向这个实体，`Common Lisp` 的具体实现可以任意的销毁一个实体。垃圾回收机制隐式地使用这样的证明。）  
例如：大多数 Common Lisp 的数据对象拥有不定生命周期。  
例如：**词法作用域**函数的**参数绑定**具有**不定生命周期**。（相反的是，在 Algol 里，词法作用与函数参数的绑定具有动态生命周期)  
```common_lisp
(defun compose(f g)
   #'(lambda (x)
       (funcall f (funcall g x))))
```  
给定两个参数，立即返回一个他们值的函数。因为返回了函数，f 和 g 的参数绑定不会立即消失，当返回的函数被调用时，依然可以使用这些绑定。所以：  
```lisp
(funcall (compose #'sqrt #'abs) -9.0)
```  
运行结果是 3.0。（类似的程序在典型的 Algol 实现和大多数的 Lisp 方言可能不一定会正常工作。）

除了上述术语，还定义了**动态作用域**用来更方便的表示**不定作用域**和**动态生命周期**。因此我们说 `special` 变量具有动态作用域，或者说是作用域是动态的，因为他们拥有不定作用域和动态生命周期：只要该变量的绑定在当前是有效的，那么一个 special 变量可以在任何地方被引用。

> 动态作用域是一个不太恰当的术语，尽管它传统且有用。

以上的定义都没有考虑 shadowing 的可能。远程引用的实体是通过一个或者另一个的名字来完成的。如果两个实体具有相同的名字，那么第二个名字可能会遮蔽第一个，在这种情况下，表现为这个名字会引用第二个实体，不能引用第一个实体。

在词法作用域的情况下，如果两个结构是词法嵌套的，并且他们是使用相同名字创建的实体，那么内部的结构的引用表示内部名字创建出的实体；内部的名字遮蔽了外部的名字。外部的表示由外部名字创建的实体。例如：

```common_lisp
(defun test (x z)
  (let ((z (* x 2))) (print z))
  z)
```

let 结构对变量 z 的绑定遮蔽了函数 test 的参数绑定。在 `print` 形式中变量 z 的引用指向 let 的绑定。在函数的结尾处，变量 z 的引用指向的是函数参数 z。

在动态生命周期的情况下。一旦两个实体的生命周期发生了重叠，那么一个周期会被嵌套在另一个里。这是 `Common Lisp` 设计的特性。

> 实现注意：在断言动态生命周期会嵌套的前提是单进程和单处理器。`Common Lisp` 并没有指出单 Lisp 环境下多道程序（分时）或者多处理器（超过一个处理器）的问题...

在一个动态生命周期中，通过名字去引用一个实体时，该引用总是指向那个还没有销毁的，最近使用这个名字去创建的实体。例如：

```common_lisp
(defun fun1 (x)
  (catch 'trap (+ 3 (fun2 x))))

(defun fun2 (y)
  (catch 'trap (+ 5 (fun3 y))))

(defun fun3 (z)
  (throw 'trap z))
```

思考一下 `(fun1 7 )`。结果将是10。当 `throw` 被执行时，显然有两个具有相同名字 `trap` 的 `catchers`：一个在执行 fun1 是被创建，另一个在执行 fun2 时被创建。后者更加贴近，所以值 7 从 fun2 中被 catch 返回。从 fun3 的角度来看，fun2 中的 catch 遮蔽了 fun1 中的 catch。如果 fun2 被定义成：

```common_lisp
(defun fun2 (y)
  (catch 'snare (* 5 (fun3 y))))
```

那么两个 catchers 有不同的名字，那么 fun1 中的就不会被遮蔽。这时结果会是 7.

作为一个规则，这本书只是简单的谈到一个实体的作用域和生命周期；遮蔽的可能性默认不谈。

下面是一些比较重要的 作用域 和 生命周期 的规则:

 * **变量绑定**一般拥有**词法作用域**和**不定生命周期**。
 * 具有**动态作用域**声明的变量绑定**依旧**是具有**词法作用域**和**不定生命周期**，但是这些**绑定的变量对象**可能拥有**动态生命周期**。（这个声明是程序员保证在即使实际的数据对象只拥有动态生命周期而不是通常的不定生命周期时，程序也可以正常执行）。
 * `symbol-macrolet` 创建的字符宏的变量名绑定拥有词法作用域和不定生命周期。
 * 声明为 `special` 的变量名具有**动态生命周期**（不定作用域和动态生命周期）
 * 创建的**函数名绑定**，例如，通过 `flet` 和 `labels` 创建的函数，具有词法作用域和不定生命周期。
 * 声明为**动态生命周期**的函数名绑定依然是**词法作用域**和**不定生命周期**，但是这个绑定下的**函数对象**可能会有**动态声明周期**。
 * `macrolet` 创建的**函数名绑定**拥有**词法作用域**和**不定生命周期**。
 * 状况 `handlers` 和 `restarts` 拥有**动态生命周期**。
 * `catch` 或 `unwind-protect` 创建的 `catcher` 特殊形式拥有**动态生命周期**。
 * `block` 结构创建的退出点（exit point）拥有词法作用域和动态命周期。（比如 `do`，`prog`，和其他的迭代结构所创建的退出点。）
 * 由 `tagbody` 创建的 `go` 标签，使用 `tagbody` 中的标签命名，并且被 `go` 引用，具有**词法作用域**和**动态生命周期**。（这些 go 标签可能出现在do，prog，和其他的迭代结构 body 中。）
 * 被命名为**常量**如 nil 和 pi 等，具有**不定作用域**和**不定生命周期**。

function 构造中出现的 lambda-expressions 词法作用域的规则，通常会导致闭包里的 non-special 变量在 lambda-expression 里可见。即，由 `lambda 表达式` 表示的函数可以引用任何词法层面的 non-special 变量并且得到正确的值，即使那些构造创建的绑定在执行时已经退出。关于这点本章上面 compose 的例子就是一个很好地佐证。这些规则当然也暗含 special **变量绑定**不会像在某些 Lisp 方言那样“完全的关闭“。

使用词法作用域的结构每次执行都会为每个创建的实体生成一个新名字。所以动态遮蔽是不可能发生的（虽然词法遮蔽是可能的）。这一点在涉及到动态生命周期时是特别重要的。例如：

```common_lisp
(defun contorted-example (f g x)
  (if (= x 0) 
      (funcall f) 
      (block here 
         (+ 5 (contorted-example g 
                                 #'(lambda () 
                                     (return-from here 4)) 
                                 (- x 1))))))
```

如果这样调用 `(contorted-example nil nil 2)`。执行结果是 4。在它的执行过程中，两个 `block`穿插了 3 次 contorted-example 调用：

```common_lisp
(contorted-example nil nil 2) 

  (block here ...) 

  (contorted-example nil #'(lambda () (return-from here 4)) 1) 

    (block here ...) 

      (contorted-example #'(lambda () (return-from here 4)) 
                         #'(lambda () (return-from here 4)) 
                         0) 
        (funcall f) 
              where f => #'(lambda () (return-from here 4)) 

          (return-from here 4)
```

在 funcall 被执行时，有两个 block 退出点，每个退出点都显示的命名为 here。在上方的跟踪，为了说明方便都加上了下标。funcall 操作的运行结果 `return-form` 指向更外层的退出点（here1），而不是更内层的（here2）。这是词法作用域的规则导致的：funcall 实际调用时产生的函数对象的 function 构造（这里缩写为 #’ 语法）执行时，return-form 指向此法可见的退出点。
 
如果将这个例子的 (function f) 改为 (function g)，然后调用  (contorted-example nil nil 2) ，结果会是 9。结果改变的原因是 funcall 会执行 (return-from here2 4)，因此程序从更加内层的退出点（here2）返回。这时，值 4 从中间层 contorted-example 的调用中退出，在此基础上加上 5，就得到了9，然后这个值从外层的块中返回到最外层 contorted-example 的调用。退出点的选择并不取决于该退出点处于最内层或最外层，相反，他取决于当 function 结构被调用时有效包装了一个 lambda 表达式的词法作用域信息。

这个 contorted-example 函数能够工作，是因为名为 f 的函数在退出点的生命周期内被调用。块退出点和 non-special 变量绑定一样拥有词法作用域，但是不一样的是她拥有的是动态生命周期而不是不定生命周期。一旦执行流离开了块结构，这个退出点就被销毁了。例如：

```common_lisp
(defun illegal-example ()
  (let ((y (block here #'(lambda (z) (return-from here z))))) 
    (if (numberp y) y (funcall y 5))))
```

根据以下几个不正确的理由很有可能有人会认为调用 (illegal-example) 的结果是 5：let 语句把变量 y 绑定到块结构的值上；这个值是 lambda 表达式的函数结果。因为 y 不是一个数字，它调用了值 5。return-from 应该从名为 here 的地方放回这个值，因此再次从块中退出并且给 y 赋值为 5， 5 是一个数字，5 作为调用illegal-example 的值被返回。

这些论点唯一错的地方在 Common Lisp 中定义的退出点是**动态生命周期**的。直到执行 return-from 这个论点是正确的。return-from 执行时发生了错误，然而，不是因为不能引用这个退出点，而是因为他正确引用了这个对出点，但这个退出点已经被销毁了。

