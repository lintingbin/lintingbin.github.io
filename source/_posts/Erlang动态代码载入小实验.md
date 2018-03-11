---
layout: posts
title: Erlang动态代码载入小实验
date: 2017-05-01 23:19:04
tags: Erlang
categories: Erlang入门教程
---
> 下面的内容为《Erlang程序设计（第2版）》8.10节的内容，这个动态代码载入的小实验非常的简单生动，通过这个小实验能够充分理解Erlang代码的载入机制。

动态代码载入是内建于Erlang核心的最惊人特性之一。它的美妙之处在于你无需了解后台的运作就能顺利实现它。
它的思路很简单：每当调用 someModule:someFunction(...)时，调用的总是最新版模块里的最新版函数，哪怕当代码在模块里运行时重新编译了该模块也是如此。
如果在a循环调用b时重新编译了b，那么下一次a调用b时就会自动调用新版的 b 。如果有许多不同进程正在运行而它们都调用了b，那么当b被重新编译后，所有这些进程就都会调用新版的b 。为了了解它的工作原理，我们将编写两个小模块：a和b。
``` erlang
%% b.erl
-module(b).
-export([x/0]).

x() -> 1.
```
``` erlang
%% a.erl
-module(a).
-compile(export_all).

start(Tag) ->
  spawn(fun() -> loop(Tag) end).

loop(Tag) ->
  sleep(),
  Val = b:x(),
  io:format("Vsn1 (~p) b:x() = ~p~n", [Tag, Val]),
  loop(Tag).

sleep() ->
  receive
    after 3000 -> true
  end.
```
现在可以编译a和b，然后启动两个a进程。
> 1> c(a).
{ok,a}
2> c(b).
{ok,b}
3> a:start(one).
<0.70.0>
Vsn1 (one) b:x() = 1
Vsn1 (one) b:x() = 1
4> a:start(two).
<0.72.0>
Vsn1 (one) b:x() = 1
Vsn1 (two) b:x() = 1

这些a进程休眠3秒钟后唤醒并调用b:x()，然后打印出结果。现在进入编辑器，把模块b改
成下面这样：
``` erlang
-module(b).
-export([x/0]).

x() -> 2.
```
然后在shell里面重新编译b。这是现在所发生的：
> 5> c(b).
{ok,b}
Vsn1 (one) b:x() = 2
Vsn1 (two) b:x() = 2

两个原版的a仍然在运行，但现在它们调用了新版的b。所以在模块a里调用b:x()时，实际上是在调用“b的最新版”。我们可以随心所欲地多次修改并重新编译b，而所有调用它的模块无需特别处理就会自动调用新版的b。
现在已经重新编译了b，那么如果我们修改并重新编译a会发生什么？来做个试验，把a改成下面这样：
``` erlang
-module(a).
-compile(export_all).

start(Tag) ->
  spawn(fun() -> loop(Tag) end).

loop(Tag) ->
  sleep(),
  Val = b:x(),
  io:format("Vsn2 (~p) b:x() = ~p~n", [Tag, Val]),
  loop(Tag).

sleep() ->
  receive
    after 3000 -> true
  end.
```
现在编译并启动a。
> 6> c(a).
{ok,a}
Vsn1 (two) b:x() = 2
Vsn1 (one) b:x() = 2
Vsn1 (two) b:x() = 2
7> a:start(three).
<0.84.0>
Vsn1 (two) b:x() = 2
Vsn1 (one) b:x() = 2
Vsn2 (three) b:x() = 2
Vsn1 (two) b:x() = 2

有趣的事情发生了。启动新版的a后，我们看到了新版正在运行。但是，那些运行最初版a的现有进程仍然在正常地运行旧版的a。
现在可以试着再次修改b。
``` erlang
-module(b).
-export([x/0]).

x() -> 3.
```
我们将在shell里重新编译b，观察会发生什么。
> 8> c(b).
{ok,b}
Vsn1 (one) b:x() = 3
Vsn2 (three) b:x() = 3
Vsn1 (two) b:x() = 3

现在新旧版本的a都调用了b的最新版。
最后，再次修改a（这是第三次修改a了）。
``` erlang
-module(a).
-compile(export_all).

start(Tag) ->
  spawn(fun() -> loop(Tag) end).

loop(Tag) ->
  sleep(),
  Val = b:x(),
  io:format("Vsn3 (~p) b:x() = ~p~n", [Tag, Val]),
  loop(Tag).

sleep() ->
  receive
    after 3000 -> true
  end.
```
现在，当我们重新编译a并启动一个新版的a时，就会看到以下输出：
> 9> c(a).
{ok,a}
Vsn2 (three) b:x() = 3
Vsn2 (three) b:x() = 3
Vsn2 (three) b:x() = 3
Vsn2 (three) b:x() = 3
10> a:start(four).
<0.96.0>
Vsn2 (three) b:x() = 3
Vsn3 (four) b:x() = 3
Vsn2 (three) b:x() = 3
Vsn3 (four) b:x() = 3
Vsn2 (three) b:x() = 3

这段输出里的字符串是由两个最新版本的a（第2版和第3版）生成的，而那些运行第1版a代码的进程已经消失了。
在任一时刻，Erlang允许一个模块的两个版本同时运行：当前版和旧版。重新编译某个模块时，任何运行旧版代码的进程都会被终止，当前版成为旧版，新编译的版本则成为当前版。可以把这想象成一个带有两个版本代码的移位寄存器。当添加新代码时，最老的版本就被清除了。一些进程可以运行旧版代码，与此同时，另一些则可以运行新版代码。