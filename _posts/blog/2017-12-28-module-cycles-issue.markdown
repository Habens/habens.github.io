---
layout: post
title: IntelliJ Module Cycles Issue
date: 2017-12-28T15:54:02+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [IntelliJ 16, Module Cycles, Querysdl, Gradle]
image:
  feature:
comments: true
share: true
---

## 产生 Module Cycles 错误现场
在一个 Gradle 项目中用到了 [Querysdl](http://www.querydsl.com/)（4.0.9），它会为每个被 @Entity 修饰的class生成一个对应的以 Q 开头的class。生成路径为 `src/querydsl/java`。

在项目代码中使用生成的 Q 开头的 class，会使得 `src/main/java` 模块需要 import `src/querydsl/java` 模块，而 `src/querydsl/java` 模块里又import 了一些 `src/main/java` 里的class，这就形成了 module cycles error。

## 现象
Gradle 任务可以正常执行，可是在 IntelliJ 16 (+) 里无法执行单元测试，报错与 module cycles 有关。

## 原因
IntelliJ 16 引入了一个新特性，默认将每一个 source set 都当做一个 module。但又不支持 module 之间的循环嵌套引用。

## 解决办法
1. 在使用 IntelliJ 引入/打开 一个 Gradle 项目时，去掉 `create separate module per source set` 的默认选项；
![](/images/post/module-cycles/intellij-setup.png)

2. 使用一些 Gradle 脚本修改默认的 source set:
```
sourceSets {
    main {
        java {
            srcDir "$buildDir/generated-sources/apt"
        }
    }
}
querydsl {
    querydslSourcesDir = "$buildDir/generated-sources/apt"
}
```
