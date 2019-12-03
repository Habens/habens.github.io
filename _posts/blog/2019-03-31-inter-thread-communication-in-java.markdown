---
layout: post
title: Java 线程间通信方式实践
date: 2019-03-31T02:44:30+08:00
author: habens_chen
modified:
categories: 
excerpt:
tags: []
image:
  feature:
comments: true
share: true
---

最近**又**回顾了一遍并发编程，忘得实在太快。正好在搜索的时候看见了这篇文章[JAVA线程间通信的几种方式](http://edisonxu.com/2017/03/02/java-thread-communication.html)，我就顺着作者提供的几条实现思路实践了一下作者提到的几个方法，用于巩固并发编程和线程间通信相关的知识。

* Table of Contents
{:toc}

## 题目
> 编写两个线程，一个线程打印1~52，另一个线程打印字母A~Z。  
> 打印顺序为12A34B56C……5152Z

## 答案

### 1. synchronized 

利用两个线程`synchronized`修改`enabledPrinter`的方式来协调这两个线程的执行。

```java
public class SynchronizedSolution {

  private enum Printer {
    NUMBER, LETTER
  }

  @Data
  @Accessors(chain = true)
  private class Scheduler {

    private Printer enabledPrinter = Printer.NUMBER;
  }

  private final Scheduler scheduler = new Scheduler();

  @Test
  public void verifySynchronized() throws InterruptedException {
    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        synchronized (scheduler) {
          try {
            while (scheduler.getEnabledPrinter() != Printer.NUMBER) {
              scheduler.wait();
            }
            for (int j = 0; j < 2; j++) {
              System.out.print(i + j);
            }
            scheduler.setEnabledPrinter(Printer.LETTER);
            scheduler.notify();
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        }
      }
    });
    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        synchronized (scheduler) {
          try {
            while (scheduler.getEnabledPrinter() != Printer.LETTER) {
              scheduler.wait();
            }
            System.out.print(i);
            scheduler.setEnabledPrinter(Printer.NUMBER);
            scheduler.notify();
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        }
      }
    });
    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }
}
```

### 2. Lock and Condition
相比`Synchronized`而言，`Lock`提供了更多对锁的可操作性，比如支持可中断的尝试获取锁，`ReadWriteLock`允许同一时间有多个线程访问到共享资源等。

这里我实现了两种方式，一种只使用`Lock`，另一种是结合`Lock`和`Condition`。

第一种方式，通过两个线程竞争`Lock`锁，并修改`enabledPrinter`的方式来协调这两个线程的执行。

单用`Lock`，即使可打印的条件不满足，也可以获取到锁，然后什么也没干又释放了锁，显然是一种浪费。

第二种方式，通过增加`Condition`，直到条件满足时才唤醒线程。

```java
public class LockConditionSolution {

  private enum Printer {
    NUMBER, LETTER
  }

  @Data
  @Accessors(chain = true)
  private class Scheduler {

    private Printer enabledPrinter = Printer.NUMBER;
  }

  private Scheduler scheduler = new Scheduler();

  @Test
  public void verifyLock() throws InterruptedException {

    Lock lock = new ReentrantLock(true);
    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        lock.lock();
        try {
          if (scheduler.getEnabledPrinter() == Printer.NUMBER) {
            for (int j = 0; j < 2; j++) {
              System.out.print(i + j);
            }
            scheduler.setEnabledPrinter(Printer.LETTER);
          }
        } finally {
          lock.unlock();
        }
      }
    });

    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        lock.lock();
        try {
          if (scheduler.getEnabledPrinter() == Printer.LETTER) {
            System.out.print(i);
            scheduler.setEnabledPrinter(Printer.NUMBER);
          }
        } finally {
          lock.unlock();
        }
      }
    });
    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }

  @Test
  public void verifyLockWithCondition() throws InterruptedException {

    Lock lock = new ReentrantLock(true);
    Condition conditionOfNumberPrint = lock.newCondition();
    Condition conditionOfLetterPrint = lock.newCondition();
    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        lock.lock();
        try {
          while (scheduler.getEnabledPrinter() != Printer.NUMBER) {
            conditionOfNumberPrint.wait();
          }
          for (int j = 0; j < 2; j++) {
            System.out.print(i + j);
          }
          scheduler.setEnabledPrinter(Printer.LETTER);
          conditionOfLetterPrint.signal();
        } catch (InterruptedException e) {
          e.printStackTrace();
        } finally {
          lock.unlock();
        }
      }
    });

    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        lock.lock();
        try {
          while (scheduler.getEnabledPrinter() != Printer.LETTER) {
            conditionOfNumberPrint.wait();
          }
          System.out.print(i);
          scheduler.setEnabledPrinter(Printer.NUMBER);
          conditionOfLetterPrint.signal();
        } catch (InterruptedException e) {
          e.printStackTrace();
        } finally {
          lock.unlock();
        }
      }
    });
    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }
}
```

### 3. volatile

对`volatile`修饰的变量的修改是多线程可见的。所以基于这样的特性，可以直接在线程里面加死循环等待状态更改。

前两个解法是通过直接操作线程的方式来执行线程的`start`和`join`的，基于练习的目的，我在这个解法里加入了线程池的使用。

```java
public class VolatileSolution {

  private enum Printer {
    NUMBER, LETTER
  }

  @Data
  @Accessors(chain = true)
  private class Scheduler {

    private Printer enabledPrinter = Printer.NUMBER;
  }

  private volatile Scheduler scheduler = new Scheduler();

  @Test
  public void verifyVolatile() throws ExecutionException, InterruptedException {
    Runnable numberPrint = () -> {
      {
        for (int i = 1; i < 52; i = i + 2) {
          while (scheduler.getEnabledPrinter() != Printer.NUMBER) {
            // waiting for numberPrinter be enabled.
          }
          for (int j = 0; j < 2; j++) {
            System.out.print(i + j);
          }
          scheduler.setEnabledPrinter(Printer.LETTER);
        }
      }
    };
    Runnable letterPrint = () -> {
      for (char i = 'A'; i <= 'Z'; i++) {

        while (scheduler.getEnabledPrinter() != Printer.LETTER) {
          // waiting for letterPrinter be enabled.
        }
        System.out.print(i);
        scheduler.setEnabledPrinter(Printer.NUMBER);
      }
    };

    ExecutorService fixedThreadPool = Executors.newFixedThreadPool(2);
    Future numberPrintFuture = fixedThreadPool.submit(numberPrint);
    Future letterPrintFuture = fixedThreadPool.submit(letterPrint);

    fixedThreadPool.shutdown();

    numberPrintFuture.get(); // waiting for numberPrint done.
    letterPrintFuture.get(); // waiting for letterPrint done.
  }
}
```

### 4. AtomicBoolean
基于`AtomicBoolean`里封装了一个`volatile int value`，这里没有用到`Atomic`提供的原子操作，和直接使用`volatile`变量是一样的效果。

基于练习的目的，这里引入了线程池和`Callable`返回值的练习。

```java
public class AtomicBooleanSolution {

  private AtomicBoolean isNumberPrinterEnable = new AtomicBoolean(true);

  @Test
  public void verifyAtomic() throws ExecutionException, InterruptedException {
    Callable<String> numberPrint = () -> {
      for (int i = 1; i < 52; i = i + 2) {
        while (!isNumberPrinterEnable.get()) {
          // waiting for numberPrinter be enabled.
        }
        for (int j = 0; j < 2; j++) {
          System.out.print(i + j);
        }
        isNumberPrinterEnable.set(false);
      }
      return "number print done";
    };
    Callable<String> letterPrint = () -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        while (isNumberPrinterEnable.get()) {
          // waiting for letterPrinter be enabled.
        }
        System.out.print(i);
        isNumberPrinterEnable.set(true);
      }
      return "letter print done";
    };

    ExecutorService fixedThreadPool = Executors.newFixedThreadPool(2);

    // Executes the given tasks, returning a list of Futures holding their status and results when all complete.
    List<Future<String>> futures = fixedThreadPool
        .invokeAll(Lists.newArrayList(numberPrint, letterPrint));

    fixedThreadPool.shutdown();

    for (Future future : futures) {
      System.out.print("\n" + future.get());
    }
  }
}
```

### 5. CyclicBarrier
利用`CyclicBarrier`设置只有当数字和字母都输出到指定个数的时，调用`barrierAction`，然后再继续执行的机制来调度两个线程的暂停和继续。

```java
public class CyclicBarrierSolution {

  @Test
  public void verifyCyclicBarrier() throws InterruptedException {
    List<String> temp = Lists.newArrayList();

    Runnable barrierAction = () -> {
      Collections.sort(temp);
      temp.forEach(System.out::print);
      temp.clear();
    };

    CyclicBarrier cyclicBarrier = new CyclicBarrier(2, barrierAction);

    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        for (int j = 0; j < 2; j++) {
          temp.add(String.valueOf(i + j));
        }
        try {
          cyclicBarrier.await();
        } catch (InterruptedException | BrokenBarrierException e) {
          e.printStackTrace();
        }
      }
    });
    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        temp.add(String.valueOf(i));
        try {
          cyclicBarrier.await();
        } catch (InterruptedException | BrokenBarrierException e) {
          e.printStackTrace();
        }
      }
    });

    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }
}
```

### 6. PipedInputStream
通过两个线程在输出、输出流上消耗、产生信息来实现两个线程之间的通信。

这段代码执行起来非常**慢**，耐心等输出结果。

```java
public class PipedStreamSolution {

  @Test
  public void verifyPipedStream() throws InterruptedException, IOException {

    final PipedInputStream numberPrinterInputStream = new PipedInputStream();
    final PipedOutputStream numberPrinterOutputStream = new PipedOutputStream();
    final PipedInputStream letterPrinterInputStream = new PipedInputStream();
    final PipedOutputStream letterPrinterOutputStream = new PipedOutputStream();

    numberPrinterOutputStream.connect(letterPrinterInputStream);
    letterPrinterOutputStream.connect(numberPrinterInputStream);

    final byte[] GO_MSG = "Go".getBytes();

    Thread numberPrintThread = new Thread(() -> {
      byte[] inArr = new byte[2];
      for (int i = 1; i < 52; i = i + 2) {
        try {
          for (int j = 0; j < 2; j++) {
            System.out.print(i + j);
          }
          numberPrinterOutputStream.write(GO_MSG);
          do {
            numberPrinterInputStream.read(inArr);
          } while (!Arrays.equals(GO_MSG, inArr));
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    });
    Thread letterPrintThread = new Thread(() -> {
      byte[] inArr = new byte[2];
      for (char i = 'A'; i <= 'Z'; i++) {
        try {
          do {
            letterPrinterInputStream.read(inArr);
          } while (!Arrays.equals(GO_MSG, inArr));
          System.out.print(i);
          letterPrinterOutputStream.write(GO_MSG);
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    });

    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();

    numberPrinterInputStream.close();
    numberPrinterOutputStream.close();
    letterPrinterInputStream.close();
    letterPrinterOutputStream.close();
  }
}
```
### 7. BlockingQueue
利用`poll`，`peek`，`take`的特性，通过消费队列上首位对象的方式来调度两个线程。
* poll(time)：获取并删除BlockingQueue里排在首位的对象，若不能立即取出，则可以等time参数规定的时间，取不到时返回null。当不传入time值时，立刻返回。
* peek()：立刻获取BlockingQueue里排在首位的对象，但不从队列里删除，如果队列为空，则返回null。
* take()：获取并删除BlockingQueue里排在首位的对象，若BlockingQueue为空，阻断进入等待状态直到BlockingQueue有新的对象被加入为止。

注意这里第一种解法`poll`是在`offer`之后，最后一行的位置调用的。  
注意这里第二种解法`offer`是在`take`之后，最后一行的位置调用的。

```java
public class BlockingQueueSolution {

  private enum Printer {
    NUMBER, LETTER
  }

  /**
   * 共享一个queue，根据peek和poll的不同来实现
   */
  @Test
  public void verifyBlockingShareQueue() throws InterruptedException {
    final LinkedBlockingQueue<Printer> queue = new LinkedBlockingQueue<>();
    queue.offer(Printer.NUMBER);

    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        while (Printer.NUMBER != queue.peek()) {
          // waiting numberPrint be enabled.
        }
        for (int j = 0; j < 2; j++) {
          System.out.print(i + j);
        }
        queue.offer(Printer.LETTER);
        queue.poll();
      }
    });
    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        while (Printer.LETTER != queue.peek()) {
          // waiting letterPrint be enabled.
        }
        System.out.print(i);
        queue.offer(Printer.NUMBER);
        queue.poll();
      }
    });

    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }

  /**
   * 两个queue，利用take()会自动阻塞来实现
   */
  @Test
  public void verifyBlockingQueue() throws InterruptedException {
    final LinkedBlockingQueue<Printer> numberPrintQueue = new LinkedBlockingQueue<>();
    final LinkedBlockingQueue<Printer> letterPrintQueue = new LinkedBlockingQueue<>();
    numberPrintQueue.offer(Printer.NUMBER);

    Thread numberPrintThread = new Thread(() -> {
      for (int i = 1; i < 52; i = i + 2) {
        try {
          numberPrintQueue.take();
          for (int j = 0; j < 2; j++) {
            System.out.print(i + j);
          }
          letterPrintQueue.offer(Printer.LETTER);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      }
    });
    Thread letterPrintThread = new Thread(() -> {
      for (char i = 'A'; i <= 'Z'; i++) {
        try {
          letterPrintQueue.take();
          System.out.print(i);
          numberPrintQueue.offer(Printer.NUMBER);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      }
    });

    numberPrintThread.start();
    letterPrintThread.start();

    numberPrintThread.join();
    letterPrintThread.join();
  }
}
```


文中所有的代码已上传[https://gitee.com/Habens/demos-concurrent](https://gitee.com/Habens/demos-concurrent)