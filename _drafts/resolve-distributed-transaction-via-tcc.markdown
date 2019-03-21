---
layout: post
title: 微服务下的分布式事务实践
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

如果你弄清楚了 JTA 规范里的那几个图，也就基本理解了整个分布式事务是如何玩儿的了。

随着微服务的流行，引入了微服务下的分布式事务的问题。通过搜索，我知道有这么几种方案可以解决这个问题：XA/2PC，TCC，基于可靠的消息队列 ，最大努力型，SAGA方案，阿里推出的GTS。

虽然已经有很多开源的库支持以上方案，但对于具体怎么解决的没有太多可参看的资料。

本文通过对 [ByteTCC](https://www.bytesoft.org/) 实践来谈论一下微服务的分布式事务如何处理的。

### 待解决问题
2个用户(处于两个业务系统之中)之间转账

### 实现
[https://gitee.com/Habens/demos-bytetcc](https://gitee.com/Habens/demos-bytetcc)

### 实现说明
在这个实现里总共有以下角色：
1. eureka-server 服务注册中心：下面的几个服务都会注册在这个服务注册中心，用于各个服务间的相互发现
2. remitter 汇款人所在的服务
3. receiver 收款人所在的服务
4. api-gateway 网关

ByteTCC 提供了自己的 TransactionManager，然后再 Spring 启动的时候接管了 Spring 的 TransactionManager，这使得ByteTCC具备JTA里提到的和资源服务器，应用服务器打交道的能力。如果你有兴趣可以看看 CompensableManagerImpl 的具体实现。

ByteTCC 在 rpc 的包里提了以下三个文件：
```java
public interface TransactionInterceptor {
    public void beforeSendRequest(TransactionRequest request) throws IllegalStateException;
    public void beforeSendResponse(TransactionResponse response) throws IllegalStateException;
    public void afterReceiveRequest(TransactionRequest request) throws IllegalStateException;
    public void afterReceiveResponse(TransactionResponse response) throws IllegalStateException;
}
```
```java
public interface TransactionRequest {
    public RemoteCoordinator getTargetTransactionCoordinator();
    public TransactionContext getTransactionContext();
    public void setTransactionContext(TransactionContext transactionContext);
}
```
```java
public interface TransactionResponse {
    public boolean isParticipantStickyRequired();
    public boolean isParticipantRollbackOnly();
    public RemoteCoordinator getSourceTransactionCoordinator();
    public TransactionContext getTransactionContext();
    public void setTransactionContext(TransactionContext transactionContext);
}
```

至于 ByteTCC 是怎么将 remitter 中的事务传递到 receiver 并进行同一管理


#### 参考资料：
* [Java Transaction API - 译](https://habens.github.io/blog/jta/)
* [JTA 深度历险 - 原理与实现](https://www.ibm.com/developerworks/cn/java/j-lo-jta/)
* [Transaction management API for REST: TCC](https://www.atomikos.com/Blog/TransactionManagementAPIForRESTTCC)
* [ByteTCC](https://www.bytesoft.org/)
