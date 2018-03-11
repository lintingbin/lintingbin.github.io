---
layout: posts
title: Erlang中catch和try...catch的区别
date: 2017-05-01 16:06:51
tags: Erlang
categories: Erlang入门教程
---
在Erlang的错误处理中，catch并不是try...catch的缩写，try...catch和catch是不同的。下面我将通过一个例子来区别出他们的不同，为以后的使用做一个参考。
``` erlang
%% exception_test.erl 代码文件
-module(exception_test).

-compile(export_all).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> error(a);
generate_exception(4) -> exit(a);
generate_exception(5) -> {'EXIT', a}.

test_use_catch() ->
    [{I, catch generate_exception(I)} || I <- lists:seq(1, 5)].

test_user_try_catch() ->
  [begin
    try generate_exception(I) of
        NormalRes ->
            {I, normal, NormalRes}
    catch
        ErrorType : Error ->
            {I, exception, ErrorType, Error}
    end
  end || I <- lists:seq(1, 5)].
```
``` erlang
%% 执行exception_test:test_use_catch().函数的返回结果
[{1,a},
 {2,a},
 {3,
  {'EXIT',{a,[{exception_test,generate_exception,1,
                              [{file,"exception_test.erl"},{line,7}]},
              {exception_test,'-test_use_catch/0-lc$^0/1-0-',1,
                              [{file,"exception_test.erl"},{line,12}]},
              {exception_test,'-test_use_catch/0-lc$^0/1-0-',1,
                              [{file,"exception_test.erl"},{line,12}]},
              {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
              {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
              {shell,eval_exprs,7,[{file,"shell.erl"},{line,641}]},
              {shell,eval_loop,3,[{file,"shell.erl"},{line,626}]}]}}},
 {4,{'EXIT',a}},
 {5,{'EXIT',a}}]
```
``` erlang
%% 执行exception_test:test_user_try_catch().函数的返回结果
[{1,normal,a},
 {2,exception,throw,a},
 {3,exception,error,a},
 {4,exception,exit,a},
 {5,normal,{'EXIT',a}}]
```
通过上面的列子我们可以看到，如果使用标准的try...catch来处理错误的话，调用者是可以正确的识别出错误，然后对错误进行相应的处理的。

但是如果用的是catch来处理错误的话，情况是不能乐观的，使用catch处理错误，exception(1)和exception(2)返回的结果是一样的，exception(4)和exception(5)返回的结果是一样的。catch在处理throw的时候只是简单的把throw的内容给返回，在处理exit的时候会返回一个tuple是带'EXIT'和exit里面的内容的结果，在处理error的时候会把堆栈给打印出来（这点比较人性化）。

所以大家在使用catch的时候要注意catch的返回值，正常的情况下还是推荐使用try...catch来处理错误，不然很容易就会掉到坑里面的。