---
title: 怎么实现一个Sublime的自动补全插件
date: 2017-09-03 13:45:49
tags: [Sublime, 插件]
categories: 教程
---
> 使用Erlang开发了快三年的游戏了，一直使用的是Sublime编辑器，也就这样没有自动补全的情况下使用了三年，本来打算切换到有Erlang自动补全的Ide的，但是在Sublime上面开发了那么久，切换到其他的编辑器觉得很不习惯，所以就自己写了一个Erlang的自动补全的插件，[点这里可以看到我的插件](https://github.com/lintingbin2009/Erl-AutoCompletion)

Sublime插件是用Python写的，所以打算开发Sublime插件的话要稍微去学习下Python，不用学的很深入，懂得基本的语法就可以愉快的开始开发插件了。我之前的入门教程看的是[creating-sublime-text-3-plugins-part-1](https://cnpagency.com/blog/creating-sublime-text-3-plugins-part-1/)，如果打算开发Sublime插件的话，看这篇文章就可以写一个简单的Sublime插件的Demo。这个网址[api_reference](http://www.sublimetext.com/docs/3/api_reference.html)可以查看开发Sublime插件所提供的各种API。

我写Erlang自动补全代码和自动跳转的原理是在打开Sublime的时候，扫描所有Erlang的源代码和Sublime中已经打开的所有的Erlang代码，然后利用正则表达式匹配来找出所有函数和模块所在的文件和位置，把这些信息都写入到Sqlite数据库中，然后在用户在编写Erlang源代码的时候提供补全的函数和模块。当用户把鼠标指向某个函数的时候，在Sqlite数据库中查询相应的函数所在的文件和位置，当用户选中的时候打开该文件并且定位到文件的相应的位置。具体的代码可以在[点这里可以看到我的插件](https://github.com/lintingbin2009/Erl-AutoCompletion)这里查看。当写好一个插件后我们最好能把插件放到Package Control中，这样用户安装和升级插件就会非常的方便，通过这个[submitting_a_package](https://packagecontrol.io/docs/submitting_a_package)教程能够顺利的提交自己的插件到Package Control中。

自己写一个小插件有时候还是可以学到一点东西的，通过这次编写自动补全的插件，让我对正则表达式稍微熟悉了一点。