---
title: '使用erlang:get_stacktrace注意避开的坑'
date: 2018-03-29 11:03:03
tags: Erlang
categories: Erlang
---
之前在使用erlang:get_stacktrace()函数的时候发现不能正确的获取发生异常的栈内容，但是错误类型和原因却是正常，感觉非常奇怪，下面是具体的代码：
``` erlang
dispatch_cmd(User, Mod, Msg) ->
  try Mod:req(User, Msg) of
    Result ->
      Result
  catch
    Class:Reason ->
      monitor:notify(ws_dispatch_crash, io_lib:format("<error-info: ~p:req ~p:~p>", [Mod, Class, Reason])),
      ?ERROR("Req Msg: ~p.~nStacktrace: ~s", [?PR(Msg), ?PR_ST(erlang:get_stacktrace(), {Class, Reason})]),
      ?ERR_AT_DISPATCH_CMD
  end.
```
**上面的代码有什么问题呢？**
主要的问题是在调用erlang:get_stacktrace()之前执行了其他有可能会有异常捕获的语句，而在io_lib:format里面会有catch函数，如果io_lib:format函数里面的catch被调用的话，erlang:get_stacktrace()返回的就不是我们想要打印的异常栈，而是io_lib:format里面的异常栈。
**如何解决？**
在catch之后里面立马调用erlang:get_stacktrace()
``` erlang
dispatch_cmd(User, Mod, Msg) ->
  try Mod:req(User, Msg) of
    Result ->
      Result
  catch
    Class:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      monitor:notify(ws_dispatch_crash, io_lib:format("<error-info: ~p:req ~p:~p>", [Mod, Class, Reason])),
      ?ERROR("Req Msg: ~p.~nStacktrace: ~s", [?PR(Msg), ?PR_ST(Stacktrace, {Class, Reason})]),
      ?ERR_AT_DISPATCH_CMD
  end.
```
据说erlang的开发团队也认为erlang:get_stacktrace()是一个不好的东西，会在OTP 21中把它废弃掉，有一位叫@peterdmv的开发人员是这样说的：
``` eralng
erlang:get_stacktrace/0 is deprecated in OTP 21, you can use the following expression instead:

try Expr
catch
  Class:Reason:Stacktrace ->
   {Class,Reason,Stacktrace}
end
```
我试了下这个新语法，在OTP 20.3上面还不行，应该在接下来的OTP 21中能够使用它吧~