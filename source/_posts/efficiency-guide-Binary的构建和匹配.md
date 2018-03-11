---
title: 'efficiency-guide:Binary的构建和匹配'
date: 2017-10-14 13:11:44
tags: Erlang
categories: Erlang Efficiency Guide
---
> 这篇文章非常详细的介绍了Binary是怎么样构造和匹配的，同时介绍了一些优化的技巧，没看过的一定要仔细看下。

以下代码可以高效的构建Binary(有点奇怪，和List不一样，下面构造Binary的段落会有解释):
``` Erlang
my_list_to_binary(List) ->
    my_list_to_binary(List, <<>>).

my_list_to_binary([H|T], Acc) ->
    my_list_to_binary(T, <<Acc/binary,H>>);
my_list_to_binary([], Acc) ->
    Acc.
```
以下代码可以高效的匹配Binary:
``` Erlang
my_binary_to_list(<<H,T/binary>>) ->
    [H|my_binary_to_list(T)];
my_binary_to_list(<<>>) -> [].
```

#### Binary类型是怎么实现的?

Binary和Bitstring在虚拟机的内部实现是一样的。在虚拟机的源代码中都叫做Binary。Binary类型在虚拟机内部由四种Binary对象实现:
* **Refc Binaries**
Refc Binaries由两部分组成：
  * 存储在进程堆上的对象，称为ProcBin 
  * Binary对象本身,它存储在所有进程堆之外 

  Binary对象可以由任意数量的进程引用任意数量的ProcBin。该对象包含一个引用计数器，用于跟踪引用数量，以便在最后一个引用消失时可以将其删除。进程中的所有ProcBin对象都是链表的一部分，因此当ProcBin消失时，垃圾回收器可以跟踪它们并减少二进制中的引用计数器。当构建的Binary大于64Byte的时候就会使用这种类型，此时进程之间发送Binary只是发送一个ProcBin。
* **Heap Binaries**
Heap binaries是小型Binary，最多64Byte，并直接存储在进程堆中。当Heap Binary被进程垃圾回收或者是作为消息发送时，都需要被复制。垃圾收集器不需要特殊处理。
* **Sub Binaries**
Sub Binaries和match contexts对象能引用refc binary和heap binary对象的部分内容。
Sub Binary是由split_binary/2创建的。一个Sub Binary引用另一个Binary的部分内容（只能引用Refc和Heap Binary，不能引用另一个Sub Binary）。因此，匹配一个Binary类型是比较高效的，因为实际的Binary数据是不会被复制的。
* **Match Context**
Match context和Sub binary比较类似，但是Match context专门为Binary匹配优化。（原文比较拗口，这里不做解释了，我也没太看懂）

#### 构建Binary

Binary和Bitstring的append操作是被运行时系统特别优化的，只有在极少数的情况下优化是不起作用的。
如下代码可以解释优化是如何起作用的:
``` Erlang
Bin0 = <<0>>,
Bin1 = <<Bin0/binary,1,2,3>>,
Bin2 = <<Bin1/binary,4,5,6>>,
Bin3 = <<Bin2/binary,7,8,9>>,
Bin4 = <<Bin1/binary,17>>,
{Bin4,Bin3}
```
* 第1行分配一个Heap Binary给Bin0变量
* 第2行是append操作。由于Bin0没有涉及append操作，所以创建一个新的Refc Binary，并将Bin0的内容复制到其中。Refc Binary的ProcBin部分存有Binary对象的数据大小，而Binary对象还分配了额外的空间。Binary对象的大小是Bin1或256的大小的两倍，以较大者为准。在这个例子中是256。
* 第3行更有意思。Bin1已被用于append操作，最后有252字节的未使用内存，因此3个新的字节会被存储在这些空闲的内存中。
* 第4行。和第3行一样。剩下249个字节，所以存储另外3个新字节没有问题。
* 第5行。有趣的事情发生。请注意，Bin4是用Bin1来append值17。Bin4将被赋值为<<0,1,2,3,17>>。Bin3将保留其价值<<0,1,2,3,4,5,6,7,8,9>>。显然，运行时系统不能将字节17写入上述的Refc Binary中，因为这会将Bin3的值更改为<<0,1,2,3,4,17,6,7,8,9>>。
运行时系统知道Bin1是先前append操作的结果，所以它将Bin1的内容复制到一个新的Binary，预留额外的存储空间等等类似上面的操作。（这里没有解释为什么运行时系统知道不能写入到Bin1中，如果有兴趣的话可以阅读erl_bits.c源代码）

##### 强制拷贝的情况
Binary的append操作优化要求对于Binary:只有一个ProcBin指向一个Refc Binary的Binary对象。原因是优化需要在append操作期间移动（重新分配）Binary对象，并且同时更新ProcBin中的指针。如果有多个ProcBin指向Binary对象，则无法找到并更新它们。
因此，对Binary的某些操作会被做标记，以便在将来做append操作的时候知道是否要强制拷贝Binary。在大多数情况下，Binary额外分配的空间也会在这个时候也被回收掉。
如果将Binary作为消息发送到其他进程或端口，则binary对象会缩小，任何进一步的append操作都会将Binary数据复制到新的Binary中。例如，在下面的代码，第3行的Bin1将会被复制：
``` Erlang
Bin1 = <<Bin0,...>>,
PortOrPid ! Bin1,
Bin = <<Bin1,...>>  %% Bin1 will be COPIED
```
同样的情况一样会发生，如果将Binary插入到Ets表中、使用erlangport_command/2将其发送到端口、或者将其传递给NIF中的enif_inspect_binary。
匹配Binary也会导致其缩小，下一个append操作将会复制Binary数据：
``` Erlang
Bin1 = <<Bin0,...>>,
<<X,Y,Z,T/binary>> = Bin1,
Bin = <<Bin1,...>>  %% Bin1 will be COPIED
```
原因是match context包含直接指向二进制数据的指针。
如果一个进程简单地保留Binary（在“循环数据”或进程字典中），垃圾回收器最终可以收缩这个Binary。如果只保留一个这样的Binary，则不会收缩。如果该进程后续append到已收缩的Binary中，则Binary对象将被重新分配，以使数据被加上。
#### 匹配Binary
重新看下文章开头的匹配例子：
``` Erlang
my_binary_to_list(<<H,T/binary>>) ->
    [H|my_binary_to_list(T)];
my_binary_to_list(<<>>) -> [].
```
第一次调用my_binary_to_list/1时，会创建match context。match context指向Binary的第一个字节。匹配1个字节，并更新match context以指向Binary的第二个字节。
在此时，创建一个sub binary似乎是有意义的，但是在这个特定的例子中编译器知道每次匹配后会马上调用一个函数（在这个例子中，是my_binary_to_list/1本身），这会导致要创建一个新的match context然后丢弃sub binary。
因此，my_binary_to_list/1使用match context而不是使用sub binary调用自身。初始化匹配操作的指令当它看到它被传递给match context而不是sub binary时基本上什么都不做。
当到达Binary的末尾并且第二个子句匹配时，match context将被简单地丢弃（在下一个垃圾回收中被移除，因为不再有任何引用）。
总而言之，my_binary_to_list/1只需要创建一个match context，而不需要sub binary。
注意，当遍历完整个Binary后，my_binary_to_list/1中的match context被丢弃。如果迭代在Binary结束之前停止，会发生什么？优化还会生效吗？
``` Erlang
after_zero(<<0,T/binary>>) ->
    T;
after_zero(<<_,T/binary>>) ->
    after_zero(T);
after_zero(<<>>) ->
    <<>>.
```
答案是依然生效，编译器将在第二个子句中删除sub binary的构建：
``` Erlang
...
after_zero(<<_,T/binary>>) ->
    after_zero(T);
...
```
但是它会生成在第一个子句中构建sub binary的代码：
``` Erlang
after_zero(<<0,T/binary>>) ->
    T;
...
```
因此，after_zero/1将构建一个match context和一个sub binary（如果它传进一个包含0的binary）。
以下代码也将进行优化：
``` Erlang
all_but_zeroes_to_list(Buffer, Acc, 0) ->
    {lists:reverse(Acc),Buffer};
all_but_zeroes_to_list(<<0,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, Acc, Remaining-1);
all_but_zeroes_to_list(<<Byte,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, [Byte|Acc], Remaining-1).
```
编译器将删除第二和第三个子句中的sub binary的构建，并向第一个子句添加一个指令，该指令将Buffer从match context转换成sub binary（如果Buffer已经是binary，则不执行任何操作）。
在开始认为编译器可以优化任何Binary模式匹配之前，以下函数不能由编译器进行优化（至少当前是这样的）：
``` Erlang
non_opt_eq([H|T1], <<H,T2/binary>>) ->
    non_opt_eq(T1, T2);
non_opt_eq([_|_], <<_,_/binary>>) ->
    false;
non_opt_eq([], <<>>) ->
    true.
```
之前提到，如果编译器知道Binary不会被共享，则会延迟创建sub binary。在当前的这种情况下，编译器无法知道。
很快，下面的章节将解释如何重写non_opt_eq/2，以便可以应用延迟sub binary的优化，更重要的是，还能发现你的代码是否可以优化。
##### bin_opt_info选项
使用bin_opt_info选项可以让编译器打印大量有关二进制优化的信息。
> erlc +bin_opt_info Mod.erl

请注意，bin_opt_info不能是能永久添加到Makefile中的选项，因为它生成的所有信息都不能被删除。因此，通过环境选择在大多数情况下是最实际的方法。
为了更准确地说明警告所引用的代码，以下示例中的警告将作为注释引用到它们所引用的子句之后插入，例如：
``` Erlang
after_zero(<<0,T/binary>>) ->
         %% NOT OPTIMIZED: sub binary is used or returned
    T;
after_zero(<<_,T/binary>>) ->
         %% OPTIMIZED: creation of sub binary delayed
    after_zero(T);
after_zero(<<>>) ->
    <<>>.
```
上述代码说明第一个匹配没有优化，第二个匹配会被优化。
让我们重新回顾一下无法优化的代码的例子，并找出原因：
``` Erlang 
non_opt_eq([H|T1], <<H,T2/binary>>) ->
        %% INFO: matching anything else but a plain variable to
    %%    the left of binary pattern will prevent delayed 
    %%    sub binary optimization;
    %%    SUGGEST changing argument order
        %% NOT OPTIMIZED: called function non_opt_eq/2 does not
    %%    begin with a suitable binary matching instruction
    non_opt_eq(T1, T2);
non_opt_eq([_|_], <<_,_/binary>>) ->
    false;
non_opt_eq([], <<>>) ->
    true.
```
编译器发出两个警告。INFO警告指的是函数non_opt_eq/2作为被调用者，表示任何调用non_opt_eq/2的函数都不能进行延迟sub binary优化。还有一个建议来改变参数顺序。第二个警告（恰好是指同一行）是指sub binary本身的构造。
下面的另一个例子将显示INFO和NOT OPTIMIZED警告之间的区别，这些警告有些清晰，但是让我们先来试一下改变参数顺序的建议：
``` Erlang
opt_eq(<<H,T1/binary>>, [H|T2]) ->
        %% OPTIMIZED: creation of sub binary delayed
    opt_eq(T1, T2);
opt_eq(<<_,_/binary>>, [_|_]) ->
    false;
opt_eq(<<>>, []) ->
    true.
```
编译器给出以下代码片段的警告：
``` Erlang
match_body([0|_], <<H,_/binary>>) ->
        %% INFO: matching anything else but a plain variable to
    %%    the left of binary pattern will prevent delayed 
    %%    sub binary optimization;
    %%    SUGGEST changing argument order
    done;
...
```
这个警告意味着如果有一个对match_body/2的调用（来自match_body/2中的另一个子句或另一个函数），那么延迟的子二进制优化是不可能的。在二进制匹配结束处的任何地方将发生更多警告，并作为match_body/2的第二个参数传递，例如：
``` Erlang
match_head(List, <<_:10,Data/binary>>) ->
        %% NOT OPTIMIZED: called function match_body/2 does not
    %%     begin with a suitable binary matching instruction
    match_body(List, Data).
```
##### 未使用的变量
编译器能够算出一个变量是否未被使用。然后为以下每个功能生成相同的代码：
``` Erlang
count1(<<_,T/binary>>, Count) -> count1(T, Count+1);
count1(<<>>, Count) -> Count.

count2(<<H,T/binary>>, Count) -> count2(T, Count+1);
count2(<<>>, Count) -> Count.

count3(<<_H,T/binary>>, Count) -> count3(T, Count+1);
count3(<<>>, Count) -> Count.
```
在每次迭代中，二进制中的前8位将被跳过，不匹配。
#### 历史笔记
R12的Binary处理显着改善。因此R11B中执行高效的代码在R12B中可能不是那么高效，反之亦然，此efficiency-guide的较早版本包含了有关R11B中二进制处理的一些信息。