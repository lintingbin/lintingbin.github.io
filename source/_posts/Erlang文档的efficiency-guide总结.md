---
title: Erlang文档的efficiency_guide总结
date: 2017-10-13 23:07:04
tags: Erlang
categories: Erlang Efficiency Guide
---
> 之前刚开始学习的Erlang的时候稍微看过这个教程，但是没有看全，发现这个教程还涵盖了挺多的信息的，今天把这个教程看完，顺便做一下总结，[教程原版地址](http://erlang.org/doc/efficiency_guide/introduction.html)

本来是想把所有的总结写在一篇文章里面的，但是由于篇幅比较大，所以就把所有的总结分为以下几篇文章：

* {% post_link efficiency-guide-七个Erlang性能的误区 %}
* {% post_link efficiency-guide-需要注意的模块和BIF %}
* {% post_link efficiency-guide-Binary的构建和匹配 %}
* {% post_link efficiency-guide-List处理 %}
* {% post_link efficiency-guide-函数 %}
* {% post_link efficiency-guide-表和数据库 %}
* {% post_link efficiency-guide-进程 %}

最后再提两个误区：
* 匿名函数很慢
匿名函数过去很慢，慢于apply/3。最初，使用编译器技巧，普通元组，apply/​3和大量的精巧方法实现了匿名函数。但那是历史。匿名函数在R6B中给出了自己的数据类型，并在R7B中进一步优化。现在，一个匿名函数的调用开销大概在调用本地函数和apply/3的开销之间。
* 列表推导很慢
 以前通过匿名函数实现列表推导，而在过去匿名函数确实很慢。 如今，编译器将列表推导重写成一个普通的递归函数。
