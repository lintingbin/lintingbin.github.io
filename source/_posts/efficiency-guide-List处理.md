---
title: 'efficiency-guide:List处理'
date: 2017-10-14 17:21:11
tags: Erlang
categories: Erlang Efficiency Guide
---
> 关于列表的处理，大多懂得Erlang的同学都差不多知道列表要怎么用，但是关于列表嵌套和拉伸的优化点还是值得一看的。

#### 列表创建
不能使用如下代码创建列表，因为每次迭代都会创建一个新的列表:
``` Erlang
bad_fib(N) ->
    bad_fib(N, 0, 1, []).

bad_fib(0, _Current, _Next, Fibs) ->
    Fibs;
bad_fib(N, Current, Next, Fibs) -> 
    bad_fib(N - 1, Next, Current + Next, Fibs ++ [Current]).
```
应该使用如下的代码创建列表:
``` Erlang
tail_recursive_fib(N) ->
    tail_recursive_fib(N, 0, 1, []).

tail_recursive_fib(0, _Current, _Next, Fibs) ->
    lists:reverse(Fibs);
tail_recursive_fib(N, Current, Next, Fibs) -> 
    tail_recursive_fib(N - 1, Next, Current + Next, [Current|Fibs]).
```

#### 列表推导
列表推导现在仍然被认为是缓慢的。他们过去常常使用funs来实现，而funs过去很慢。
以下的列表推导：
``` Erlang
[Expr(E) || E <- List]
```
会被转换成本地的函数实现：
``` Erlang
'lc^0'([E|Tail], Expr) ->
    [Expr(E)|'lc^0'(Tail, Expr)];
'lc^0'([], _Expr) -> [].
```
如果列表推导的结果不会被使用，则不会构造列表。如下的代码：
``` Erlang
[io:put_chars(E) || E <- List],
ok.
%% 或者
...
case Var of
    ... ->
        [io:put_chars(E) || E <- List];
    ... ->
end,
some_function(...),
...
```
上述的代码不会构造列表，所以转换成以下的本地函数实现：
``` Erlang
'lc^0'([E|Tail], Expr) ->
    Expr(E),
    'lc^0'(Tail, Expr);
'lc^0'([], _Expr) -> [].
```
编译器知道分配给'_'意味着该值不会被使用。因此，以下示例中的代码也将进行优化：
``` Erlang
_ = [io:put_chars(E) || E <- List],
ok.
```

#### 嵌套和拉伸列表(Deep and Flat Lists)

lists:flatten/1比++操作更加的低效，在下述的情况中，可以很简单的避免使用lists:flatten/1:
* 向端口发送数据时。端口了解嵌套列表，所以没有理由在将列表发送到端口之前拉伸列表。
* 当调用接受嵌套列表的BIF时，例如list_to_binary/1或iolist_to_binary/1。
* 当知道列表只有一级嵌套时，可以使用list:append/1。

##### 端口例子
``` Erlang
...
port_command(Port, DeepList) %% DO
...

...
port_command(Port, lists:flatten(DeepList)) %% DO NOT
...
```
通常会这样向端口发送一个以0为结尾的字符串：
``` Erlang
...
TerminatedStr = String ++ [0], % String="foo" => [$f, $o, $o, 0]
port_command(Port, TerminatedStr)
...
```
上述效率比较低，应该用下述方式代替：
``` Erlang
...
TerminatedStr = [String, 0], % String="foo" => [[$f, $o, $o], 0]
port_command(Port, TerminatedStr) 
...
```

##### Append例子
``` Erlang
lists:append([[1], [2], [3]]). %% DO

lists:flatten([[1], [2], [3]]). %% DO NOT
```

#### 递归列表函数
普通递归列表函数和尾部递归函数在结束的时候反转列表之间通常没有太大差异。因此，专注于编写好看的代码，并忘记了列表功能的性能。在代码的性能关键部分（仅在那里），用比较高效的写法就行了。
> 这部分是关于构造列表的列表函数。不构造列表的尾递归函数运行在常量空间中，而相应的普通递归函数使用与列表长度成比例的堆栈空间。

例如，一个将整数列表相加的函数不能写成如下：
``` Erlang 
recursive_sum([H|T]) -> H+recursive_sum(T);
recursive_sum([])    -> 0.
```
应该写成:
``` Erlang
sum(L) -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.
```