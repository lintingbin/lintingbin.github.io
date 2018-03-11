---
title: 'efficiency_guide:需要注意的模块和BIF'
date: 2017-10-14 12:38:02
tags: Erlang
categories: Erlang Efficiency Guide
---
> 以下就是要注意的模块和BIF，这些内容之前大多数知道，就setelement/3和size/1的使用优化是不知道的，值得一读

#### Timer模块
使用erlang:send_after/3和erlang:start_timer/3创建定时器比使用Timer模块更有效率。Timer模块使用单独的进程来管理定时器。如果许多进程在同时创建和取消定时器，这个进程很容易成为瓶颈。此外Timer模块的有些函数是不通过单一的进程实现的（如定时器：tc/3或定时器：sleep/1），因此这些函数是可以放心使用的。
#### list_to_atom/1
Atom是不进行内存回收的，一旦一个Atom被创建，它永远不会被移除。如果Atom的数量到达虚拟机的上限（默认1,048,576）的话，虚拟机将会奔溃。
因此，在一个连续运行的系统中，将任意输入字符串转换成Atom是危险的。如果只允许某些定义好的原子作为输入，使用list_to_existing_atom/1可以用来防范拒绝服务攻击。
使用list_to_atom/1构建一个可以被传入apply/3函数的atom，效率是比较低的，不推荐在需要运行很快的代码中使用:
``` Erlang
apply(list_to_atom(“some_prefix”+ + Var), foo, Args)
```
#### length/1
与tuple_size/1、byte_size/1和bit_size/1的O(1)时间复杂度不同，length/1执行的时间与List的长度成正比为O(n)
。通常，没有必要担心length/1的速度，因为它是用C语言很高效的实现的，但是如果List很长，为了避免O(n)的时间复杂度。在一些使用场景中length/1可以被模式匹配来代替,如下：
``` Erlang
foo(L) when length(L) >= 3 ->
    ...
```
  可以被写成模式匹配的方式
``` Erlang
foo([_,_,_|_]=L) ->
   ...
```
  上面两段代码的区别是：如果L不是的列表，length(L)将会出错，而第二段代码中将不能正确匹配。
#### setelement/3
setelement/3会复制其修改的tuple。因此，使用setelement/3循环更新一个tuple的不同字段，会每次创建一个tuple的新副本。
有一种情况是可以例外的，如果编译器清楚地知道，破坏性地更新tuple会产生tuple复制相同的结果，那么对setelement/3的调用将被替换为一个特殊的破坏性设置指令。在以下代码中，第一个setelement/3调用复制该tuple并修改第9个元素：
``` Erlang
multiple_setelement(T0) ->
    T1 = setelement(9, T0, bar),
    T2 = setelement(7, T1, foobar),
    setelement(5, T2, new_value).
```
  后面两个setelement/3调用将该复制的tuple的d第7和第5个元素也修改了，不再复制新的tuple。
要实现上述的优化，必须满足以下条件：
  * 索引必须是整数文字，而不是变量或表达式。
  * 索引必须按降序给出。 
  * 在连续的setelement/3调用之间不能有任何其他函数调用。 
  * 从一个setelement/3调用返回的元组只能在随后的setelement/3调用中使用。

  如果代码不能像multi_setelement/1示例中那样被构造，那么修改大元组中的多个元素的最好方法是将元组转换为列表，修改列表，并将其转换回元组。

#### size/1
size/1可以用来返回tuple和binary的大小。但是如果使用tuple_size/1和byte_size/1的话，能为编译器和运行时系统提供了更多优化机会。另一个优点是能给了Dialyzer提供更多的类型信息。
#### split_binary/2
使用模式匹配而不是调用split_binary/2函数来分割二进制通常更有效率。此外，混合使用比特语法匹配和split_binary/2会使比特语法匹配的一些优化失效。
``` Erlang
<<Bin1:Num/binary,Bin2/binary>> = Bin %% 推荐
{Bin1,Bin2} = split_binary(Bin, Num) %% 不推荐
```
#### 运算符 "- -"
"- -" 运算符具有与其操作数长度乘积成比例的时间复杂度（O(m*n)）。这意味着如果该操作符的两个操作数都是长列表，那么操作者非常慢：
``` Erlang
HugeList1 -- HugeList2
%% 上述操作应该被替换成下面的操作
HugeSet1 = ordsets:from_list(HugeList1),
HugeSet2 = ordsets:from_list(HugeList2),
ordsets:subtract(HugeSet1, HugeSet2)
%% 如果在意列表的原始顺序的话，可以退换成如下的操作
Set = gb_sets:from_list(HugeList2),
[E || E <- HugeList1, not gb_sets:is_element(E, Set)]
```
  注意：如果列表包含重复的元素(HugeList2中出现一个元素在HugeList1中删除了所有出现的元素)，则该代码的行为与“- -”不同。另外，这个代码比较了使用“==”运算符的列表元素，而“- -”使用“=:=”运算符。如果这个区别很重要，那么可以使用set代替gb_set，但是在长列表中set:from_list/1比gb_sets:from_list/1慢得多。
使用“- -”运算符从列表中删除一个元素不会有性能问题：HugeList1 -- [Element] 。