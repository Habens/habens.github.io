---
layout: post
title: 微服务下的分布式事务实践
date: 2019-03-21T21:49:24+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [TCC, 分布式事务]
image:
  feature:
comments: true
share: true
---

JTA 给出了一系列 api 规范以解决 Java 世界的分布式事务问题。之前的文章里对 JTA 的英文规范做了一部分翻译，如果有需要可以参看[Java Transaction API - 译](https://habens.github.io/blog/jta/)

如果你弄清楚了 JTA 规范里的那几张图，也就基本理解整个分布式事务是如何玩儿的。

随着微服务的流行，引入了微服务下的分布式事务的问题。通过搜索，我知道有这么几种方案可以解决这个问题：XA/2PC，TCC，基于可靠的消息队列 ，最大努力型，SAGA方案，阿里推出的GTS。

虽然已经有很多开源的库支持以上方案，但除了去阅读他们的源码，我没有发现太多可参看的资料。所以我结合一道虚拟的题目来走查一下[ByteTCC](https://www.bytesoft.org/)是怎么解决微服务的分布式事务。

#### 待解决问题
2个用户(处于两个业务系统之中)之间转账

#### 实现
[https://gitee.com/Habens/demos-bytetcc](https://gitee.com/Habens/demos-bytetcc)

#### 实现说明
在这个实现里总共有以下角色：
1. eureka-server 用于各个服务间的相互发现
2. remitter 汇款人所在的服务
3. receiver 收款人所在的服务
4. api-gateway 网关

##### 事务管理器
ByteTCC 提供了自己的 TransactionManager，接管了 Spring 的 TransactionManager，这使得ByteTCC具备JTA里提到的和资源服务器，应用服务器打交道的能力。如果你有兴趣可以看看`CompensableManagerImpl`的具体实现。


##### 事务上下文
通过以下代码（选自`CompensableFeignInterceptor`），ByteTCC使得事务的上下文在微服务之间相互传递。
```
if (!headers.containsKey("org.bytesoft.bytetcc.transaction")) {
  template.header("org.bytesoft.bytetcc.transaction", new String[]{transactionText});
}

if (!headers.containsKey("org.bytesoft.bytetcc.propagation")) {
  template.header("org.bytesoft.bytetcc.propagation", new String[]{this.identifier});
}
```

##### 事务日志
每个服务的数据库里都需要创建一个名为 `bytejta` 的表，用来记录事务的处理情况。
```sql
CREATE TABLE bytejta (
  xid  varchar(32),
  gxid varchar(40),
  bxid varchar(40),
  ctime bigint(20),
  PRIMARY KEY (xid)
);
```

##### 其他
`CompensableCoordinatorController`为每个微服务暴露协调事务的入口，然后再由分支协调者（RemoteCoordinator）来协调本地事务以及远程事务:
![](/images/post/bytetcc/coordinator.jpg)

###### 参考资料：
* [Java Transaction API - 译](https://habens.github.io/blog/jta/)
* [JTA 深度历险 - 原理与实现](https://www.ibm.com/developerworks/cn/java/j-lo-jta/)
* [Transaction management API for REST: TCC](https://www.atomikos.com/Blog/TransactionManagementAPIForRESTTCC)
* [ByteTCC](https://www.bytesoft.org/)
* [ByteTCC Developer Guide](https://github.com/liuyangming/ByteTCC/wiki/Developer-Guide)
