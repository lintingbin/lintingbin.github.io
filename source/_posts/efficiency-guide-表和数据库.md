---
title: 'efficiency-guide:表和数据库'
date: 2017-10-15 13:36:22
tags: Erlang
categories: Erlang Efficiency Guide
---
> 这篇文件主要讲一些简单的Erlang数据库注意要点，干货好像没那么多，可以快速浏览下。

#### Ets、Dets、Mnesia
每个Ets的例子都有一个与之对应的Mnesia的例子。一般来说，所有Ets示例也适用于Dets表。
##### Select/Match操作
在Ets和Mnesia表上的Select/Match操作是非常低效的。他们通常需要扫描整个表。需要优化数据的结构来减少对Select/Match操作的使用。但是，如果确实需要Select/Match操作，它仍然比使用tab2list更有效率。函数ets:select/2和mnesia:select/3是优于ets:match/2、ets:match_object/2和mnesia:match_object/3的。
在某些情况下，Select/Match操作是不需要扫描整张表的。例如，在ordered\_set的表中搜索，或者搜索的Mnesia表的字段是有建立索引的。
当创建要在Select/Match操作中使用的记录时，如果希望大多数字段具有值“_”。最简单和最快捷的方法如下：
``` Erlang
#person{age = 42, _ = '_'}. 
```

##### 删除一个元素
如果元素不存在于表中，则删除操作被认为是成功的。因此，在删除之前，所有尝试检查元素是否存在于Ets / Mnesia表中都是不必要的。以下是Ets表的示例：
``` Erlang
%% 直接删就可以了
...
ets:delete(Tab, Key),
...
%% 这样做，效率低而且没有意义
...
case ets:lookup(Tab, Key) of
    [] ->
        ok;
    [_|_] ->
        ets:delete(Tab, Key)
end,
...
```

##### 非持久数据库存储
对于非持久数据库存储，优先考虑Ets表，而不是Mnesia local_content表。与Ets写入相比，即使是Mnesia dirty_write操作具有常量的开销。 Mnesia也必须检查表是否被复制或具有索引，这涉及每个dirty_write至少一次Ets查找。因此，Ets总是比Mnesia写的快。

##### tab2list
简单的说tab2list肯定不要用，除非是需要返回所有的Ets数据。如果需要选择一部分的数据可以用Select/Match操作，因为获取Ets的数据是需要拷贝的，tab2list返回所有的数据，需要大量的拷贝，效率非常低。

##### ordered_set表
ordered_set仅保证按Key的顺序处理对象。即使Key不包含在结果中，也可以按Key顺序显示ets:select/2等函数的结果（还有select,match_object,foldl,first,next等函数）。

#### 优化掉Select/Match操作
##### Ets
由于Ets用Key来Lookup获取数据是可以在常量的时间内完成的（使用哈希和树结构），所以如果要优化掉Select操作，可以再建一个要Select的字段到先前的Ets的Key的新的Ets表就可以了，这样通过两次lookup就可以取得想要的数据了。
##### Mnesia
在Mnesia中只要多建立一些想要Select字段的索引就可以，这样就不用扫描整张表了。