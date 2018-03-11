---
title: 'efficiency-guide:进程'
date: 2017-10-15 14:38:57
tags: Erlang
categories: Erlang Efficiency Guide
---
> 这篇文章主要介绍进程的堆大小的优化，以及模块内的常量池，如果之前没有看过的，值得一看。

#### 创建一个进程
与操作系统中的线程和进程相比，Erlang进程是轻量级的。
新创建的Erlang进程在非SMP和非HiPE模式下中使用309个字的内存。（SMP和HiPE支持会增加一些内存）进程占用的内存大小可以通过以下方式得到：
> Erlang (BEAM) emulator version 5.6 [async-threads:0] [kernel-poll:false]
Eshell V5.6  (abort with ^G)
1> Fun = fun() -> receive after infinity -> ok end end.
#Fun<...>
2> {_,Bytes} = process_info(spawn(Fun), memory).
{memory,1232}
3> Bytes div erlang:system_info(wordsize).
309

此大小包括堆区域（包括堆栈）的233个字。垃圾收集器根据需要增加堆。
进程的主（外）循环必须是尾递归的。否则，堆栈会一直增长直到进程终止。
``` Erlang
%% 不要这样做
loop() -> 
  receive
     {sys, Msg} ->
         handle_sys_msg(Msg),
         loop();
     {From, Msg} ->
          Reply = handle_msg(Msg),
          From ! Reply,
          loop()
  end,
  io:format("Message is processed~n", []).
```
对io:format/2的调用永远不会被执行，但是每次loop/0被递归调用时，返回地址仍然被推送到堆栈。该函数的正确尾递归版本如下所示：
``` Erlang
   loop() -> 
      receive
         {sys, Msg} ->
            handle_sys_msg(Msg),
            loop();
         {From, Msg} ->
            Reply = handle_msg(Msg),
            From ! Reply,
            loop()
    end.
```

##### 初始堆大小
对于具有数十万甚至数百万个进程的Erlang系统，默认的初始堆大小为233个字节是相当保守的。垃圾收集器根据需要增加和收缩堆。
在使用相对较少进程的系统中，可以通过使用erl的+h选项或使用spawn_opt/4的min_heap_size选项在每个进程的基础上增加最小堆大小来提高性能。主要有以下两个好处：
* 虽然垃圾收集器可以逐步增长堆的大小，但这比生成进程时直接建立更大的堆来的低效。
* 垃圾收集器还可以收缩堆，如果它比存储在其上的数据量大得多;设置最小堆大小可以防止这种情况发生。
> 注意：由于虚拟机可以使用更多的内存，则内存回收的次数将减少，因此Binary可以被保存的更久才进行内存回收

在具有许多进程的系统中，运行时间很短的计算任务可以生成具有更大最小堆大小的新进程。当进程完成后，它将计算结果发送到另一个进程并终止。如果正确计算最小堆大小，则该过程可能根本不需要执行任何垃圾回收。**如果没有进行适当的测量，则不进行此优化。**

#### 进程消息
除了在同一个Erlang节点上的refc binary，Erlang进程之间的所有消息都会被复制。
当消息发送到另一个Erlang节点上的进程时，它首先被编码为Erlang外部格式，然后通过TCP/IP套接字发送。接收的Erlang节点解码消息并将其分发到相应的进程。
##### 常量池（Constant Pool）
Erlang常量（也称为文字(literals)）保存在常量池中;每个加载的模块都有自己的池。以下函数不会在每次调用时构建元组（仅在下次运行垃圾回收时丢弃它），因为元组位于模块的常量池中：
``` Erlang
days_in_month(M) ->
    element(M, {31,28,31,30,31,30,31,31,30,31,30,31}).
```
但是如果一个常量被发送到另一个进程（或存储在一个Ets表中），那么它将被复制。原因是运行时系统必须能够跟踪所有对常量的引用，以正确卸载包含常量的代码。（当代码被卸载时，这些常量被复制到引用它们的进程的堆中。）复制常量可能在将来的Erlang/OTP版本中被删除。
##### 共享丢失
在以下情况下，共享不会保留：
* 当一个共享变量被发送到另一个进程 
* 当一个共享变量作为spawn调用中的初始进程参数传递时
* 当共享变量被存储在Ets表中时

这是一个优化。大多数应用不发送带有共享变量的消息。
以下示例显示如何创建共享变量：
``` Erlang
kilo_byte() ->
    kilo_byte(10, [42]).

kilo_byte(0, Acc) ->
    Acc;
kilo_byte(N, Acc) ->
    kilo_byte(N-1, [Acc|Acc]).
```
kilo_byte/1创建一个嵌套列表。如果调用list_to_binary/1，则可以将嵌套列表转换为1024字节的Binary：
> 1> byte_size(list_to_binary(efficiency_guide:kilo_byte())).
1024

使用erts_debug:size/1 BIF，可以看出嵌套列表只需要22个字的堆空间：
> 2> erts_debug:size(efficiency_guide:kilo_byte()).
22

使用erts_debug:flat_size/1 BIF，可以忽略共享来计算嵌套列表的大小。当它被发送到另一个进程或存储在Ets表中时，它将成为列表的实际大小：
> 3> erts_debug:flat_size(efficiency_guide:kilo_byte()).
4094

将数据插入到Ets表中，则可以确认共享丢失：
> 4> T = ets:new(tab, []).
#Ref<0.1662103692.2407923716.214181>
5> ets:insert(T, {key,efficiency_guide:kilo_byte()}).
true
6> erts_debug:size(element(2, hd(ets:lookup(T, key)))).
4094
7> erts_debug:flat_size(element(2, hd(ets:lookup(T, key)))).
4094

当数据被插入到Ets表时，erts_debug:size/1和erts_debug:flat_size/1返回相同的值。共享已经丢失。
在未来的Erlang/OTP版本中，可能会实现一种方式（可选）保留共享。

#### SMP
SMP模式（在R11B中引入）通过运行多个Erlang调度器线程（通常与内核数相同）来利用多核计算机的性能。每个调度器线程以与非SMP模式中的Erlang调度器相同的方式调度Erlang进程。

为了通过使用SMP模式获得更多的性能提升，应用程序**大多数时候都必须有多个可运行的Erlang进程**。否则，Erlang虚拟机仍然只能运行一个Erlang进程，但是仍然必须增加锁的开销。尽管Erlang/OTP尝试尽可能减少锁开销，但它永远不会变为零。