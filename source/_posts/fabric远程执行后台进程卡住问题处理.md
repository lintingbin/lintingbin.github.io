---
title: fabric远程执行后台脚本卡住的问题
date: 2018-07-28 13:24:38
tags: fabric
---
> 最近用fabric开发了一个简单的运维系统，该系统可以在远程机器上执行一些命令，在批量执行后台脚本的启动的时候，发现fabric会卡住，不能正常的返回。在fabric官方有给出一些解决方案[Why can’t I run programs in the background with &? It makes Fabric hang](http://www.fabfile.org/faq.html#why-can-t-i-run-programs-in-the-background-with-it-makes-fabric-hang)，但是这些方案都要额外的工具，有些麻烦。接下来简单的介绍下fabric的机制和我的解决方案。

#### fabric是怎么在远程机器上执行命令的？
fabric是先用ssh连接到远程机器上，然后再执行相关的命令。


#### fabric为什么会卡住不返回？
ssh连接到远程机器上的时候会起一个session，fabric卡住不返回是因为这个session一直结束不掉，这个session之所以结束不掉是因为我们起的后台进程有输出是定向到这个session的。


#### 怎么解决？
只要把后台进程的输出重定向到其他地方，fabric就可以正常返回。比如把后台脚本的启动方式改成`yourcommand > /dev/null 2>&1 &`，这样就会把yourcommand脚本的所有输出重定向到/dev/null，因此fabric就可以正常返回了。


#### 注意事项
我之前后台脚本的启动方式是`yourcommand 2>&1 1> /dev/null`，按照我本来的理解，应该是把stderr和stdout都重定向到/dev/null了，但是这个命令只是把stdout重定向到/dev/null，stderr还是没有被重定向。 

使用`yourcommand &> /dev/null`这种方式也可以把所有的输出重定向到/dev/null，这种方式的命令也更短一些。


