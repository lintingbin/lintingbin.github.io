---
layout: posts
title: '使用lager为什么要加入编译选项{parse_transform,lager_transform}'
date: 2017-05-30 13:03:05
tags: [Erlang, Lager]
categories: Erlang深入
---
在使用的lager的时候我们需要加入一行编译选项——{parse_transform,lager_transform}，或者是在每个使用lager的文件模块的头部加入一行-compile([{parse_transform, lager_transform}]).，这通常会让我们感觉非常的麻烦，但是大家有没有觉得好奇，为什么使用这个参数呢？

首先我们看下Erlang文档，在compile模块中有parse_transform参数的相关说明：
> **{parse_transform,Module}**
Causes the parse transformation function Module:parse_transform/2 to be applied to the parsed code before the code is checked for errors.

通过上面的文档我们知道，在编译的时候使用{parse_transform,Module}参数，会使用Module:parse_transform/2函数对代码进行一次解析转换。接下来我们在lager的源代码目录下可以看到lager_transform.erl的代码文件，里面也有一个parse_transform/2的函数。
``` erlang
parse_transform(AST, Options) ->
    TruncSize = proplists:get_value(lager_truncation_size, Options, ?DEFAULT_TRUNCATION),
    Enable = proplists:get_value(lager_print_records_flag, Options, true),
    Sinks = [lager] ++ proplists:get_value(lager_extra_sinks, Options, []),
    put(print_records_flag, Enable),
    put(truncation_size, TruncSize),
    put(sinks, Sinks),
    erlang:put(records, []),
    %% .app file should either be in the outdir, or the same dir as the source file
    guess_application(proplists:get_value(outdir, Options), hd(AST)),
    walk_ast([], AST).
```
parse_transform/2函数的第一个参数是AST，这个是代码在被编译成二进制前的一种格式[The Abstract Format](http://erlang.org/doc/apps/erts/absform.html),第二个参数是在编译的时候传入的编译参数，比如要加入一个sink的话不单单要在配置文件里面加入配置，还要在编译参数里面加入{lager_extra_sinks, [audit]}，这样parse_transform/2函数才能在proplists:get_value(lager_extra_sinks, Options, [])的时候获得audit这个sink。

顺着代码往下走，我们看到只有调用的函数的模块名是Sinks中之一的才会被解析转换（lists:member(Module, Sinks)），比如lager:info、lager:error、audit:info、audit:error等函数（audit为我们配置的sink）。
``` erlang
walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H, get(sinks))|Acc], T).

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, Module},
                                  {atom, _Line3, Function}}, Arguments0} = Stmt,
                    Sinks) ->
    case lists:member(Module, Sinks) of
        true ->
            case lists:member(Function, ?LEVELS) of
                true ->
                    SinkName = lager_util:make_internal_sink_name(Module),
                    do_transform(Line, SinkName, Function, Arguments0);
                false ->
                    case lists:keyfind(Function, 1, ?LEVELS_UNSAFE) of
                        {Function, Severity} ->
                            SinkName = lager_util:make_internal_sink_name(Module),
                            do_transform(Line, SinkName, Severity, Arguments0, unsafe);
                        false ->
                            Stmt
                    end
            end;
        false ->
            list_to_tuple(transform_statement(tuple_to_list(Stmt), Sinks))
    end;
```

最后来到解析转换真正起作用的地方，这边的注释写的很清楚，下面的解析转换等于就是lager:dispatch_log/6里面的内容，如果直接调用lager:dispatch_log/6函数的话，是不需要这样的解析转换的，我对此特地问了下lager的开发者，这样做的话能够提高多少的性能，对方给的答复是能快一倍（图 1-1），因为在log不需要输出的情况下就不需要拷贝内容到外部的函数了，个人觉得一次外部函数调用应该费不了多少时间吧。
``` erlang
    %% Wrap the call to lager:dispatch_log/6 in case that will avoid doing any work if this message is not elegible for logging
    %% See lager.erl (lines 89-100) for lager:dispatch_log/6
    %% case {whereis(Sink), whereis(?DEFAULT_SINK), lager_config:get({Sink, loglevel}, {?LOG_NONE, []})} of
    {'case',Line,
         {tuple,Line,
                [{call,Line,{atom,Line,whereis},[{atom,Line,SinkName}]},
                 {call,Line,{atom,Line,whereis},[{atom,Line,?DEFAULT_SINK}]}, 
                 {call,Line,
                       {remote,Line,{atom,Line,lager_config},{atom,Line,get}},
                       [{tuple,Line,[{atom,Line,SinkName},{atom,Line,loglevel}]},
                        {tuple,Line,[{integer,Line,0},{nil,Line}]}]}]},
         %% {undefined, undefined, _} -> {error, lager_not_running};
         [{clause,Line,
                  [{tuple,Line,
                          [{atom,Line,undefined},{atom,Line,undefined},{var,Line,'_'}]}],
                  [],
                  %% trick the linter into avoiding a 'term constructed but not used' error:
                  %% (fun() -> {error, lager_not_running} end)()
                  [{call, Line, {'fun', Line, {clauses, [{clause, Line, [],[], [{tuple, Line, [{atom, Line, error},{atom, Line, lager_not_running}]}]}]}}, []}]
          },
          %% {undefined, _, _} -> {error, {sink_not_configured, Sink}};
          {clause,Line,
                  [{tuple,Line,
                          [{atom,Line,undefined},{var,Line,'_'},{var,Line,'_'}]}],
                  [],
                  %% same trick as above to avoid linter error
                  [{call, Line, {'fun', Line, {clauses, [{clause, Line, [],[], [{tuple,Line, [{atom,Line,error}, {tuple,Line,[{atom,Line,sink_not_configured},{atom,Line,SinkName}]}]}]}]}}, []}] 
          },
          %% {SinkPid, _, {Level, Traces}} when ... -> lager:do_log/9;
```
![图 1-1](/images/lager_issue.png)

总结一下，我们平时在用Erlang编程的时候应该不会涉及到自己编写parse_transform函数的需求，这个函数的功能非常强大，可以理解成是一个功能非常强大的宏，但是我觉得编写这个函数的话也会非常容易出错的，看下lager_transform.erl文件里面的代码就知道了。其实不单单lager使用了parse_transform函数的功能，ets也使用了这个功能，由于ets的select和match匹配的可读性实在太差了，所以可以使用ets:fun2ms/1模拟函数的写法来写匹配规则（当然不是真正的函数了，写起来有很多限制的），然后在编译的时候转化成select和match的匹配格式。