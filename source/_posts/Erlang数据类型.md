---
layout: posts
title: Erlang数据类型
date: 2017-04-30 11:23:06
tags: Erlang
categories: Erlang入门教程
---
Erlang提供的数据类型，包括以下几种：
#### 基本类型
* 数字(Number)
数字类型又包含整数(integers)和浮点数(floats)类型，其中整数是精确的而且是支持大数的，小数是满足IEEE754规则的64位浮点数。Erlang支持ASCII或者Unicode转换成整数值，同时支持整数不同进制的表示。（‘%’后的内容为注释）
> 1> $a.　　%% ASCII表中的a是97
97
2> $哈.
21704
3> $\n.
10
4> 2#100.　　%% 用100表示的二进制是4
4
5> 4#100.
16
6> 16#100.
256
* 原子(Atom)
原子可以理解为一个不可变的常量，必须以小写字母开头，如果要以大写、下划线或者其他的特殊字符开头，必须加上单引号。原子在Erlang里面是存在一张表上面的，原子的个数有上限，大概是在一百万个左右。
> test
'Myhome'
'_hero'
* 位串和二进制(Bit Strings and Binaries)
在大多数情况下，二进制型里的位数都会是8的整数倍，因此对应一个字节串。如果位数不是8的整数倍，就称这段数据为位串（bitstring）。所以当我们说位串时，是在强调数据里的位数不是8的整数倍。位语法是一种表示法，用于从二进制数据里提取或加入单独的位或者位串。当你编写底层代码，以位为单位打包和解包二进制数据时，就会发现位语法是极其有用的。
> 1> <<257,1,2,3,5>>. 　　%%二进制型的元素如果大于8位的会自动截断，257截断成1
<<1,1,2,3,5>>
2> <<0:7,1:2>>. 　　%%二进制型位数如果不是8的整数倍就会产生位串，这边多了1位1
<<0,1:1>>
3> <<0:3,0:4,1:2>>.
<<0,1:1>>
* 引用(Reference)
可以通过make_ref/0函数来创建一个引用，引用在Erlang程序运行时调用make_ref函数产生的是全局唯一的。比如timer模块在创建一个定时任务的时候通常会返回一个引用，可以通过这个引用来取消定时任务。
* 函数(Fun)
函数在Erlang里面也算是一种数据类型，通过给变量绑定函数，可以通过变量名来执行函数。
> 1> Fun = fun(X) -> X * X end.
#Fun<erl_eval.6.50752066>
2> Fun(9).
81
* 端口标识符(Port Identifier)
端口用于与外界通信，由通过函数open_port/2来创建。消息可以通过端口进行收发，但是这些消息必须遵守所谓“端口协议”(port protocol)的规则。
* 进程标识符(Pid)
当创建一个进程的时候会产生一个进程标识符，可以通过这个进程标识符和进程进行通讯。
> 1> Process1 = spawn(fun() -> receive X -> io:format("recv ~p, bye~n", [X]) end end).
<0.34.0>　　%% 创建一个进程等待接收消息
2> Process1 ! my_test.　　%% 给进程发消息
recv my_test, bye
my_test

#### 复合类型
为了方便定义以下的这些复合类型，我把上述的所有基本类型都称为Term。
* 元组(Tuple)
元组类似于C语言里面的结构体(Struct)，是由固定数量的元素组成的复合数据类型，可以定义成如下结构：
> {Term1, Term2, ..., TermN}

  可以通过模式匹配或者element/2函数来提取元组里面元素的值，通过setelement/3来设置元组里面元素的值，size可以取元组里面元素的个数。
> 1> P = {adam,24,{july,29}}.
{adam,24,{july,29}}
2> element(1,P).
adam
3> element(3,P).
{july,29}
4> P2 = setelement(2,P,25).
{adam,25,{july,29}}
5> size(P).
3
6> {adam, Old, {Month, Day}} = P.
{adam,24,{july,29}}
7> Old.
24
* 映射组(Map)
映射组是一个由多个Key-Vaule结构组成的符合数据类型，可以定义为如下结构：
> #{Key1=>Value1, Key2=>Value2, ..., KeyN=>ValueN}
其中Key、Value都是Term

  可以通过maps模块提供的一些函数对映射组进行操作
> 1> M1 = #{name=>adam,age=>24,date=>{july,29}}.
#{age => 24,date => {july,29},name => adam}
2> maps:get(name,M1).
adam
3> maps:get(date,M1).
{july,29}
4> M2 = maps:update(age,25,M1).
#{age => 25,date => {july,29},name => adam}
5> map_size(M).
3
6> map_size(#{}).
0
* 列表(List)
列表类似于其他语言里面的数组，是由可变数量的元素组成的复合数据结构，可以定义成如下结构：
> [Term1, Term2, ..., TermN]

  在Erlang里面，列表由一个头和一个尾组成，空列表也是一个列表。所以列表也可以有一个递归的定义  
> List = [Term| List] | []
[] 是一个列表, 因此 
[c|[]] 是一个列表, 因此 
[b|[c|[]]] 是一个列表, 因此  
[a|[b|[c|[]]]] 是一个列表, 或者简写为 [a,b,c]

  lists模块可以提供大量函数对列表进行操作：
> 1> L = [3,3,4,2,1,2,34].
[3,3,4,2,1,2,34]
2> length(L).
7
3> lists:sort(L).
[1,2,2,3,3,4,34]
4> lists:reverse(L).
[34,2,1,2,4,3,3]

#### 其他类型(不算数据类型)
* 字符串(String)
字符串用一对双引号括起来，但不算是Erlang中的数据类型。字符串仅仅是列表的一个缩写，比如：字符串"hello"是列表[$h,$e,$l,$l,$o]的一个缩写。两个相邻的字符串在编译的时候连接成一个字符串，不会造成任何运行时开销。
> 1> "hello" " " "world".
"hello world"
* 记录(Record)
记录其实就是元组的另一种形式。通过使用记录，可以给元组里的各个元素关联一个名称。对记录的处理是在编译的时候完成的，在运行时是不会有记录的，可以把记录理解成是元组的一种语法糖。
``` erlang
-module(person).
-export([new/2]).
-record(person, {name, age}).
new(Name, Age) ->
    #person{name=Name, age=Age}.
```
  > 1> person:new(ernie, 44).
{person,ernie,44}

* 布尔类型(Boolean)
在Erlang中没有Boolean类型。而是用原子true和false来表示布尔值。
> 1> 2 =< 3.
true
2> true or false.
true

#### 类型转换
Erlang提供了一些内置的类型转换函数，可以方便地进行类型转换，下面是一些类型转换的例子：
> 1> atom_to_list(hello).
"hello"
2> list_to_atom("hello").
hello
3> binary_to_list(<<"hello">>).
"hello"
4> binary_to_list(<<104,101,108,108,111>>).
"hello"
5> list_to_binary("hello").
<<104,101,108,108,111>>
6> float_to_list(7.0).
"7.00000000000000000000e+00"
7> list_to_float("7.000e+00").
7.0
8> integer_to_list(77).
"77"
9> list_to_integer("77").
77
10> tuple_to_list({a,b,c}).
[a,b,c]
11> list_to_tuple([a,b,c]).
{a,b,c}
12> term_to_binary({a,b,c}).
<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>
13> binary_to_term(<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>).
{a,b,c}
14> binary_to_integer(<<"77">>).
77
15> integer_to_binary(77).
<<"77">>
16> float_to_binary(7.0).
<<"7.00000000000000000000e+00">>
17> binary_to_float(<<"7.000e+00>>").
7.0