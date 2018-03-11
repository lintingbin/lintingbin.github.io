---
title: 'efficiency-guide:函数'
date: 2017-10-15 12:15:32
tags: Erlang
categories: Erlang Efficiency Guide
---
> 本篇文章主要介绍函数的模式匹配和调用，函数的模式匹配优化还是有必要了解一下的。

#### 模式匹配
在函数的头部以及case和receive子句中的模式匹配都会被编译器优化。但是有一些例外，重新排列匹配子句没有什么好处。
Binary的模式匹配就是一个例外。编译器不重新排列与Binary相匹配的子句。最后放置与空Binary匹配的子句通常比放置在第一个的子句执行的更快。
下面是另一种例外的例子：
``` Erlang
atom_map1(one) -> 1;
atom_map1(two) -> 2;
atom_map1(three) -> 3;
atom_map1(Int) when is_integer(Int) -> Int;
atom_map1(four) -> 4;
atom_map1(five) -> 5;
atom_map1(six) -> 6.
```
问题在于带有变量Int的子句。由于变量可以匹配任何内容，包括原子four、five、six，后续的子句也匹配，所以编译器必须生成次优代码，执行如下：
* 首先，将输入值与one、two、three（使用二分查找，即使有很多值也非常有效）来选择要执行的前三个子句中的哪一个（如果有的话）。 
* 如果前三个子句中没有一个匹配，则第四个子句匹配变量的话会始终匹配。 
* 如果测试is_integer(Int)成功，则执行第四个子句。
* 如果测试失败，则将输入值与four、five、six进行比较，并选择适当的子句。 （如果没有匹配成功，则会产生一个function_clause异常。）

如果想让匹配代码更加高效，则上面的代码可以重写成：
``` Erlang
atom_map2(one) -> 1;
atom_map2(two) -> 2;
atom_map2(three) -> 3;
atom_map2(four) -> 4;
atom_map2(five) -> 5;
atom_map2(six) -> 6;
atom_map2(Int) when is_integer(Int) -> Int.
```
或者:
``` Erlang
atom_map3(Int) when is_integer(Int) -> Int;
atom_map3(one) -> 1;
atom_map3(two) -> 2;
atom_map3(three) -> 3;
atom_map3(four) -> 4;
atom_map3(five) -> 5;
atom_map3(six) -> 6.
```

还有下面的例子：
``` Erlang
map_pairs1(_Map, [], Ys) ->
    Ys;
map_pairs1(_Map, Xs, [] ) ->
    Xs;
map_pairs1(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs1(Map, Xs, Ys)].
```
第一个参数没有问题。它是在所有匹配子句都有的一个变量。有问题的是第二个参数在第二个匹配子句的变量Xs。因为Xs变量可以匹配任何东西，所以编译器不能重新排列匹配子句，而是必须按照上述代码的顺序生成与它们匹配的代码。 
如果函数按如下方式重写，编译器就可以自由地重新排列子句：
``` Erlang
map_pairs2(_Map, [], Ys) ->
    Ys;
map_pairs2(_Map, [_|_]=Xs, [] ) ->
    Xs;
map_pairs2(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs2(Map, Xs, Ys)].
```
编译器将生成与此类似的代码：
``` Erlang
explicit_map_pairs(Map, Xs0, Ys0) ->
    case Xs0 of
    [X|Xs] ->
        case Ys0 of
        [Y|Ys] ->
            [Map(X, Y)|explicit_map_pairs(Map, Xs, Ys)];
        [] ->
            Xs0
        end;
    [] ->
        Ys0
    end.
```
这可能是最常见的情况，输入列表不是空或非常短。（另一个优点是Dialyzer可以为Xs变量推导出更准确的类型。）

#### 函数调用
以下是对不同函数调用类型的代价的粗略的估计。它是在Solaris/Sparc上运行测试出来的基准数据：
* 调用本地或外部函数（foo()，m:foo()）是最快的。
* 调用或者apply调用一个匿名函数（Fun(),apply(Fun, [])）的大概时间花费差不多是调用一个本地函数的三倍。
* Apply调用一个被导出的函数(Mod:Name(),apply(Mod, Name, [])),大概的时间花费差不多是调用匿名函数的两倍，也就是本地函数调用的六倍

##### 注释和实现细节
调用和apply一个匿名函数不涉及任何的哈希表查找。一个匿名函数变量包含一个（间接的）指向匿名函数实现的函数的指针。
apply/3必须在哈希表中查找执行的函数的代码。因此，总是比直接调用或匿名函数调用来的慢。
它不再重要（从性能的角度来看）是否写成：
``` Erlang
Module:Function(Arg1, Arg2)
```
或者：
``` Erlang
apply(Module, Function, [Arg1,Arg2])
```
编译器会将后一种代码重写为前一种的方式。
以下代码会稍微慢一点，因为参数的个数在编译时是未知的。
``` Erlang
apply(Module, Function, Arguments)
```

#### 递归的内存使用
当编写递归函数时，最好使用尾递归的方式，以便它们可以在恒定的内存空间中执行：
最好这样写：
``` Erlang
list_length(List) ->
    list_length(List, 0).

list_length([], AccLen) -> 
    AccLen; % Base case

list_length([_|Tail], AccLen) ->
    list_length(Tail, AccLen + 1). % Tail-recursive
```
而不要这样写：
``` Erlang
list_length([]) ->
    0. % Base case
list_length([_ | Tail]) ->
    list_length(Tail) + 1. % Not tail-recursive
```