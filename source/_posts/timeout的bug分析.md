---
title: 关于erlang-mysql-driver timeout的bug分析
date: 2018-03-10 22:08:17
tags: [erlang-mysql-driver, emysql]
categories: Erlang
---
> 我们游戏之前使用的是erlang-mysql-driver来连接数据库，经常会碰到一些timeout和一条纪录被重复插入多次的bug，后面把erlang-mysql-driver替换成emysql就没有问题了。研究了下erlang-mysql-driver的源代码才知道具体的问题出在哪里，下面就简单的介绍下这个问题，同时介绍下emysql是如何避免这个问题的

#### erlang-mysql-driver 用法
``` erlang
mysql:start_link(DB, Pool, Server, Port, User, Passwd, DBName, fun mysql_log/4, utf8),
[mysql:connect(DB, Pool, Server, Port, User, Passwd, DBName, utf8, true, true) 
|| _ <- lists:seq(1, PoolCount)]
```
使用erlang-mysql-driver的时候首先要用mysql:start_link来建立一个连接池，然后再自己调用mysql:connect来建立多个连接。对应的erlang-mysql-driver库里面会创建一个名为DB的连接池gen_server，然后再建立多个mysql数据库的连接，每个连接会有一个进程来接管，并把这些进程和连接信息放入连接池gen_server中。按道理我们建立了多个数据库的连接，在进行数据库操作的时候应该能够并发访问数据库的，确实erlang-mysql-driver实现也是多进程访问数据库的，但是由于连接池gen_server的单点瓶颈，会导致一些事实上成功的操作被认为是失败的。

#### erlang-mysql-driver 的sql执行过程
在我们执行一个mysql:execute的函数的时候，具体的执行过程如下：
``` erlang
%% mysql.erl 文件中的代码
execute(SvrName, PoolId, Name, Params, Timeout) ->
  case get(?STATE_VAR) of
    undefined ->
      call_server(SvrName, {execute, SvrName, PoolId, Name, Params}, Timeout);
    State ->
      case mysql_conn:execute_local(SvrName, State, Name, Params) of
        {ok, Res, NewState} ->
          put(?STATE_VAR, NewState),
          Res;
        Err ->
          Err
      end
  end.
  
handle_call({execute, SvrName, PoolId, Name, Params}, From, State) ->
  with_next_conn(PoolId, State,
    fun(Conn, State1) ->
      case gb_trees:lookup(Name, State1#state.prepares) of
        none ->
          {reply, {error, {no_such_statement, Name}}, State1};
        {value, {_Stmt, Version}} ->
          mysql_conn:execute(SvrName, Conn#conn.pid, Name,
            Version, Params, From),
          {noreply, State1}
      end
    end);
```
``` erlang
%% mysql_conn的代码
execute(SvrName, Pid, Name, Version, Params, From, Timeout) ->
    send_msg(Pid, {execute, SvrName, Name, Version, Params, From}, From, Timeout).
    
loop(State) ->
    RecvPid = State#state.recv_pid,
    LogFun = State#state.log_fun,
    receive
    ....
    {execute, SvrName, Name, Version, Params, From} ->
        State1 =
        case do_execute(State, SvrName, Name, Params, Version) of
            {error, _} = Err ->
            send_reply(From, Err),
            State;
            {ok, Result, NewState} ->
            send_reply(From, Result),
            NewState
        end,
        loop(State1);
        
send_reply(GenSrvFrom, Res) ->
    gen_server:reply(GenSrvFrom, Res).
```
mysql:execute在执行的过程中首先调用名为DB的连接池gen_server，该gen_server会执行with_next_conn选择一个持有数据库连接的进程（进程x），然后通过mysql_conn中的send_msg函数向进程x发送需要执行的sql语句，sql语句发送成功后gen_server会返回noreply，这时调用mysql:execute的进程会一直阻塞，直到进程x执行gen_server:reply来返回结果。

通过上面的执行过程我们可以知道以下两点：
1. mysql:execute的timeout为gen_server:call调用的timeout时间
2. 连接池gen_server只要把sql语句发送给进程x，进程x就会去执行（可能执行的比较慢，但是已经加入进程x的信箱）

现在我们可以知道mysql:execute返回timeout的情况有两种：
1. 连接池gen_server太过繁忙，mysql:execute的请求还没执行，mysql:execute就已经timeout
2. 连接池gen_server已经成功执行请求，返回noreply，mysql:execute的执行进程一直在等待进程x的返回，而进程x一直不返回，这时mysql:execute触发timeout

不管是上述那种timeout情况，只要是mysql数据库没有问题，sql语句都能够执行成功（可能会执行的慢点）。mysql:execute的调用者在发现mysql:execute返回timeout的情况下，肯定会认为sql语句没有执行成功，这时候会重新调用mysql:execute，导致一条相同的记录被多次插入。

#### emysql 的sql执行过程
emysql的连接池也会有一个gen_server进行管理，emysql:execute在执行的过程中是去向该gen_server申请一个可用的连接，然后再spawn一个进程来执行sql语句，而不是委托该gen_server来执行sql语句，从而避免了这个timeout的bug。

#### 总结
通过上面的分析，我觉得erlang-mysql-driver会写出这个bug的原因主要是对gen_server noreply的误用。所以以后如果有需要用noreply的话，要注意避免该问题。