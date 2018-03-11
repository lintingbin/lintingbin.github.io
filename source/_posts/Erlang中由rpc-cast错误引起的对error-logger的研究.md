---
title: 'Erlang中由rpc:cast错误引起的对error_logger的研究'
date: 2017-06-15 22:45:28
tags: Erlang
categories: Erlang深入
---
> 之所以会写这篇文章，是因为rpc:cast函数的使用超出了我的理解范围，本来我的理解是：如果rpc:cast执行失败的话，是不会有任何报错的。但是由于最近的线上存在两个版本的代码进行的互相调用引发了一些报警，让我好奇rpc:cast和error_logger是怎么工作的。

#### rpc:cast 是怎么工作的？
我们在查阅rpc的源代码的时候可以发现以下代码：
``` erlang
-define(NAME, rex).
...
handle_cast({cast, Mod, Fun, Args, Gleader}, S) ->
    spawn(fun() ->
		  set_group_leader(Gleader),
		  apply(Mod, Fun, Args)
	  end),
    {noreply, S};
...
cast(Node, Mod, Fun, Args) when Node =:= node() ->
    catch spawn(Mod, Fun, Args),
    true;
cast(Node, Mod, Fun, Args) ->
    gen_server:cast({?NAME,Node}, {cast,Mod,Fun,Args,group_leader()}),
    true.
...
```
从上面的代码我们可以看到，rpc:cast的时候如果目标node和本地node一样的话就会直接spawn一个进程处理，如果是远程的话，则会调用一个名字为rex的gen_server到远程的服务器上执行，远程的服务器同样也是spawn一个进程来处理。如果一个服务器是为其他服务器提供服务的（通过rpc模块），那么这个服务器的rex应该会是最繁忙的。通过上面的分析我们知道rpc:cast的错误是spawn函数通知error_logger的。

#### spawn出来的进程执行遇到错误怎么处理？
我自己试验了下，比如我自己在shell里面执行spawn(fun() -> 1 = 2 end).语句的话，error_logger就会收到如下的一个错误事件：
``` erlang
{error,<113291.32.0>,
  {emulator,"~s~n",
  ["Error in process <0.13313.2075> on node 'all_in_one_33000@192.168.1.102' with exit value: {{badmatch,2},[{erl_eval,expr,3,[]}]}\n"]}}
```
为了明白上述的情况为什么会发生，我在Erlang邮件列表里面找到两个类似的问题，可以解答我的疑问：

[[erlang-questions] An answer: how does SASL know that a process	died?](http://erlang.org/pipermail/erlang-questions/2013-October/075867.html)

[[erlang-questions] error_logger events sent by emulator](http://erlang.org/pipermail/erlang-questions/2014-April/078792.html)

简单的总结上面的两个问题，spawn执行的程序遇到异常的话，是由虚拟机的C语言代码向error_logger发送的错误事件。

#### error_logger 是怎么工作的？
[error_logger](http://erlang.org/doc/man/error_logger.html)是Erlang的错误记录器，由[gen_event](http://erlang.org/doc/man/gen_event.html)实现，在Erlang系统中会有一个注册名为error_logger的事件管理器(event manager)，可以在事件管理器中加入各种处理模块来处理事件。默认的系统中会加入以下两个错误处理模块：
``` erlang
$ erl
Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:12:12] 
[async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
1> gen_event:which_handlers(error_logger).
[error_logger,error_logger_tty_h]
```
简单的说下这两个错误处理模块，首先是error_logger模块，以下是该模块的处理事件的部分代码：
``` erlang
handle_event({Type, GL, Msg}, State) when node(GL) =/= node() ->
    gen_event:notify({error_logger, node(GL)},{Type, GL, Msg}),
    %% handle_event2({Type, GL, Msg}, State);  %% Shall we do something
    {ok, State};                               %% at this node too ???
handle_event({info_report, _, {_, Type, _}}, State) when Type =/= std_info ->
    {ok, State};   %% Ignore other info reports here
handle_event(Event, State) ->
    handle_event2(Event, State).
...
handle_event2(Event, {1, Lost, Buff}) ->
    display(tag_event(Event)),
    {ok, {1, Lost+1, Buff}};
handle_event2(Event, {N, Lost, Buff}) ->
    Tagged = tag_event(Event),
    display(Tagged),
    {ok, {N-1, Lost, [Tagged|Buff]}};
handle_event2(_, State) ->
    {ok, State}.
...
display2(Tag,F,A) ->
    erlang:display({error_logger,Tag,F,A}).
```
该模块把是本node产生的事件调用erlang:display()输出，把不是本node产生的事件发送到目标node上面，由目标node的error_logger进行处理。

接着是error_logger_tty_h模块，以下是该模块的处理事件的部分代码：
``` erlang
handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, State) ->
    write_event(tag_event(Event),io),
    {ok, State}.
...
write_event({Time, {error, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F, [Format,Args])
    end;
...
format(IOMod, String)       -> format(IOMod, String, []).
format(io_lib, String, Args) -> io_lib:format(String, Args);
format(io, String, Args) -> io:format(user, String, Args).
```
该模块把不是该node的事件直接忽略，然后把本node的事件调用io:format输出到终端上面。

除了这两个处理模块，Erlang的[sasl](http://erlang.org/doc/man/sasl_app.html)应用还提供了三个模块：sasl_report_tty_h、sasl_report_file_h、log_mf_h。log_mf_h模块的功能最为强大，能够把错误写入指定个数的文件中，当文件用完后会自动删除最老的事件以腾出空间记录最新的事件。但是log_mf_h的缺点是记录的是二进制的格式，要查看记录的事件的话，还需要使用sasl提供的rb模块来解析，颇为繁琐。而且该模块没有对单事件的最大上限做保护，如果有超大的事件写入的话，就会导致文件错乱，看不了事件（这个可以自己写代码做保护，我们项目之前就是这样做的）。

当然除了官方提供的处理模块，也可以使用第三方提供的模块。现在我们项目就把所有官方提供的模块都删除掉了，只使用lager提供的error_logger_lager_h模块来处理事件，然后自己编写了一个alarm_handle_error模块用来发送报警。error_logger_lager_h使用文本的方式来记录事件，查看起来比较方便，而且对Erlang内部一些比较难以理解的错误进行翻译，比较容易理解；但是由于使用文本的方式进行记录，没有对事件消息进行格式化，如果消息比较大的话，读起来比较费劲。

#### error_logger 添加处理模块的注意事项
当使用sasl提供的log_mf_h处理模块的时候不能删除系统提供的error_logger模块，不然像rpc:cast通知的事件就不能正常的捕获了，原因如下：
``` erlang
%% 当NodeA执行以下函数的时候，在NodeB会接收到一个错误，
%% 由于NodeB只有log_mf_h模块，log_mf_h模块会对接收的事件使用sasl:pred/1函数进行过滤
%% sasl:pred/1会过滤不是本node的产生的事件，因此该错误被过滤
%% 如果这时候NodeB有error_logger模块的话，error_logger模块就会将事件通知NodeA
%% 然后NodeA就能使用log_mf_h模块正确记录该错误，该错误记录在NodeA的机器上
NodeA: rpc:cast(NodeB, M, ErrorFun, []).
```
lager的error_logger_lager_h模块默认会记录所有的事件，不管该事件是属于哪个Node的，如下：
``` erlang
%% 当NodeA执行以下函数的时候，在NodeB会接收到一个错误，
%% error_logger_lager_h直接记录错误在NodeB的机器上
NodeA: rpc:cast(NodeB, M, ErrorFun, []).
```
#### 最后说两句
之前项目使用log_mf_h模块处理事件的配置文件如下：
``` erlang
[{sasl, [
          {sasl_error_logger, false},
          {errlog_type, error},
          {error_logger_mf_dir, "logs"},
          {error_logger_mf_maxbytes, 1073741824}, % 1GB
          {error_logger_mf_maxfiles, 10}
    ]}].
```
之前一直觉得errlog_type是控制log_mf_h模块的处理事件级别的参数，这边设置的参数是error，为什么info的信息还会记录下来呢？后面看了下sasl.erl模块的代码，errlog_type和log_mf_h模块根本没有关系，然后回头再看了一下sasl的文档:

log_mf_h

This error logger writes **all** events sent to the error logger to disk. Multiple files and log rotation are used. For efficiency reasons, each event is written as a binary. For more information about this handler, see the STDLIB Reference Manual.

To activate this event handler, three SASL configuration parameters must be set, error_logger_mf_dir, error_logger_mf_maxbytes, and error_logger_mf_maxfiles. The next section provides more information about the configuration parameters.

文档中**all**已经加黑了，我居然没看到，以后还得好好认真看文档！