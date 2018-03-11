---
layout: posts
title: Erlang代码热更新
date: 2017-05-04 23:30:18
tags: Erlang
categories: Erlang入门教程
---
通过上一篇的文章{% post_link Erlang动态代码载入小实验 %}，我们可以了解到Erlang的热更机制，在Erlang里面会维护两个版本的代码。在新版本载入的时候如果有进程在老版本运行的话，运行那些内部调用的函数（只通过函数名调用的）代码将不会被更新，只有那些通过M:F格式调用的内部函数才能热更。举个例子：
``` erlang
%% 如果代码在loop上执行的话，有两种情况

loop() ->
  io:format("v1 ~n"),  %% 这条语句这种情况不能热更
  loop().

loop() ->
  io:format("v1 ~n"),  %% 这条语句这种情况可以热更
  ?MODULE:loop().
```
在Erlang里面有分本地调用（local calls）和外部调用（external calls），本地调用的函数名是不需要被导出的。本地调用的格式是Fun(Args)，外部调用的格式是M:F(Args)。

Erlang运行时会保存一份代码的两个版本，所有本地调用的函数地址都会指向程序运行时最初的那个版本（如上面例子的情况一），而所有外部调用的函数地址都会指向最新的版本（如上面例子的情况二）。所以如果想要让代码能够热更新的话，需要使用外部调用的格式。

在我们项目中一般热更的流程是先：code:soft_purge(ModName)或者code:purge(ModName)然后再code:load_file(ModName)进行热更，针对这一热更流程我之前一直存在两个问题，最近仔细研究下才找到了答案，分别是以下这两个问题：
* 为什么load_file之前要先soft_purge或者purge一下呢？
这个是load_file函数的问题，如果在load_file执行的时候，本身要热更的模块就有一个老的版本的代码存在的话，load_file就会返回一个not_purged的错误代码，导致新版本不能正常的载入。如果load_file执行自动删除最老版本的话，就不需要purge了（像在Erlang Shell里面执行c(ModName)一样）。当然如果一个模块从来都没有热更过的话（在系统里面只有一个版本），直接使用load_file是没有问题的，不过之后就要先purge再load_file了。
* soft_purge和purge有什么不同吗？
函数的功能上是有所不同的，但是在我们项目的使用中几乎是没有什么不同的。soft_purge和purge的函数的功能区别是如果清理的模块的老的版本中有进程在上面运行的话，purge就会杀掉进程，然后把老的版本给清理掉，soft_purge则会清理失败。热更的时候是先执行purge然后再loadfile，由于进程一般都是在当前的版本上面执行，这时候老的版本上面不会有进程在运行，所以执行purge和soft_purge是一样的，如果真的想要热更的时候把进程杀掉的话应该执行purge/soft_purge->loadfile->purge。

以上就是我对Erlang代码热更的总结～