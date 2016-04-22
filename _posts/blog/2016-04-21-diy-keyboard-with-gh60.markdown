---
layout: post
title: 自制机械键盘
date: 2016-04-21T18:07:59+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [DIY, 机械, 键盘, 自制]
image:
  feature: post/diy-keyboard/diy-keyboard-with-gh60.jpg
comments: true
share: true
---
> 如果不是亲身去做，你可能永远都不知道，困难是什么 ——  于渊

## 材料：
* 键帽
* 外壳
* GH60 PCB 板
* 卫星轴
* 5 脚轴 / 定位板 + 3 脚轴
* MINI USB 线
* 焊接套件

### 材料注意事项：
1. 先想好自己需要什么样的**配列**（键位），这样你才能买到合适的**键帽**。不同配列的个别键帽是存在差异的
2. 注意 PCB 板上键位和键帽的配合。比如分清空格键是多长的（7x / 6.5x / 6.25x）
3. 卫星轴要配合 PCB 来买，注意空格键的卫星轴长度 7x / 6.5x / 6.25x
4. 买5脚的轴就不需要定位板，买3脚的需要定位板，不然轴易晃动

## 键盘检测工具：
* MAC - Karabiner -> EventViewer

## 刷配列：
* [键位设计](http://www.keyboard-layout-editor.com/) - [离线版](https://github.com/ijprest/keyboard-layout-editor)
* [生成刷配列需要的文件](http://www.enjoyclick.org/tkg/) - [离线版](https://github.com/kairyu/tkg.git)
* [刷配列的工具](https://github.com/kairyu/tkg-toolkit)

### 配列注意事项：
* 必须清楚是哪个版本的 GH60 (A/B/C, RevCHN ...)

## 如果再来一次，我的组装步骤会是：
1. 买齐材料
   * **note**：选择最合适的**键帽**和**外壳**（可能有套件）
2. 检查材料
	* 键帽，pcb板，卫星轴合适否
	* 轴是否有损坏
	* pcb 板的每个键位是否可用
3. 焊轴
   * 在此之前要把键帽、轴、pcb板大致组一下，确定个别键究竟该用哪个孔
4. 安键帽
5. [设计键位](http://www.keyboard-layout-editor.com/) / [离线版](https://github.com/ijprest/keyboard-layout-editor)，其实直接改[tmk_keyboard](https://github.com/tmk/tmk_keyboard)里的源码来得直观些，快些。
6. [生成刷配列需要的 .eep 文件](http://www.enjoyclick.org/tkg/) / [离线版](https://github.com/kairyu/tkg.git)
7. 利用[tkg-toolkit](https://github.com/kairyu/tkg-toolkit)刷配列
8. 利用 Karabiner 检测每个键值
9. Enjoy

<figure class="third">
	<a href="/images/post/diy-keyboard/1.pic.jpg"><img src="/images/post/diy-keyboard/1.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/2.pic.jpg"><img src="/images/post/diy-keyboard/2.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/3.pic.jpg"><img src="/images/post/diy-keyboard/3.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/4.pic.jpg"><img src="/images/post/diy-keyboard/4.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/8.pic.jpg"><img src="/images/post/diy-keyboard/8.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/9.pic.jpg"><img src="/images/post/diy-keyboard/9.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/10.pic.jpg"><img src="/images/post/diy-keyboard/10.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/11.pic.jpg"><img src="/images/post/diy-keyboard/11.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/13.pic.jpg"><img src="/images/post/diy-keyboard/13.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/14.pic.jpg"><img src="/images/post/diy-keyboard/14.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/15.pic.jpg"><img src="/images/post/diy-keyboard/15.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/16.pic.jpg"><img src="/images/post/diy-keyboard/16.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/5.pic.jpg"><img src="/images/post/diy-keyboard/5.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/6.pic.jpg"><img src="/images/post/diy-keyboard/6.pic.jpg" alt="image"></a>
	<a href="/images/post/diy-keyboard/7.pic.jpg"><img src="/images/post/diy-keyboard/7.pic.jpg" alt="image"></a>
</figure>

