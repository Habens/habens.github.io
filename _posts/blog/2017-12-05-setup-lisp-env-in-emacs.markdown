---
layout: post
title: Common Lisp开发环境设置
date: 2017-12-05T20:51:54+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [lisp, emacs, config]
image:
  feature:
comments: true
share: true
---

在 `Spacemacs` 里可以用`SPC m h H`(在 emacs 里是`c-c c-d h` )查看 Common Lisp 的函数用法。

通过这个快捷键可以快速定位到 [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm) 文档，但这是个在线文档， 如果你需要已经下载了该文档，可以在 `Spacemacs` 的 `init.el` 里添加以下代码即可以使用本地的 `HyperSpec` 文档：


`quicklisp` 里面有个 CLHs 包是专门干这个事情的，而且，还自带了一份 `HyperSpec` 文档到本地


```common_lisp
(ql:quickload "clhs") 
```


修改 `.sbclrc` 让 `sbcl` 启动的时候自动完成：
1. 加载 `quicklisp` 
2. 设置当前目录为工作根目
3. 设置 sbcl 源码位置

```common_lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(push  (probe-file ".") asdf:*central-registry*)
(sb-ext:set-sbcl-source-location "/path/to/code/of/sbcl")
```
