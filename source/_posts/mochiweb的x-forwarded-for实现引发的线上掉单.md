---
title: mochiweb的x-forwarded-for实现引发的线上掉单
date: 2017-08-18 21:13:44
tags: [mochiweb, x-forwarded-for]
categories: Erlang
---
> 记录一次线上充值服的掉单问题，同时学习下什么是x-forwarded-for

#### 掉单原因？
因为充值服都设有白名单，如果充值请求的机器的IP不在白名单里面的话会被视为非法IP，在掉单期间，线上的充值服发现有大量的100.116.* .* 的非法IP的访问，之后在网上一查，原来100.64.0.0/10也是属于内网IP的。我们的充值服务器使用了负载均衡，所以100.116.*.*的IP应该是负载均衡机器的内网IP，同时由于我们充值服务器使用的是mochiweb的服务器，所以第一时间查看了下mochiweb获取IP的源代码：
``` erlang
get(peer, {?MODULE, [Socket, _Opts, _Method, _RawPath, _Version, _Headers]}=THIS) ->
    case mochiweb_socket:peername(Socket) of
        {ok, {Addr={10, _, _, _}, _Port}} ->
            case get_header_value("x-forwarded-for", THIS) of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        %% Copied this syntax from webmachine contributor Steve Vinoski
        {ok, {Addr={172, Second, _, _}, _Port}} when (Second > 15) andalso (Second < 32) ->
            case get_header_value("x-forwarded-for", THIS) of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {Addr={192, 168, _, _}, _Port}} ->
            case get_header_value("x-forwarded-for", THIS) of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {{127, 0, 0, 1}, _Port}} ->
            case get_header_value("x-forwarded-for", THIS) of
                undefined ->
                    "127.0.0.1";
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {Addr, _Port}} ->
            inet_parse:ntoa(Addr);
        {error, enotconn} ->
            exit(normal)
    end;
```
从上面的代码可以看出，如果在服务器内网里面使用了代理服务器之后，mochiweb是能够自动获取原始的访问IP。但是仅限内网代理服务器的IP是一些常见的内网IP，100.64.0.0/10段的IP地址并不包括在里面，所以这时候获取的IP就不是原始IP，而是负载均衡机器的内网IP。

#### 内网IP段有哪些？

10.0.0.0/8  
10.0.0.0 - 10.255.255.255 

172.16.0.0/12  
172.16.0.0 - 172.31.255.255  

192.168.0.0/16  
192.168.0.0 - 192.168.255.255  

以上三个网段分别属于A、B、C三类IP地址

100.64.0.0/10  
100.64.0.0 - 100.127.255.255  
由运营商使用的私网IP段，随着IPv4地址池的耗光，会有更多用户被分配到这个网段。我们的线上掉单问题就是因为阿里云把内网IP切换到这个网段造成的。

#### http协议头标：x-forwarded-for
X-Forwarded-For(XFF)是用来识别通过HTTP代理或负载均衡方式连接到Web服务器的客户端最原始的IP地址的HTTP请求头字段。 Squid 缓存代理服务器的开发人员最早引入了这一HTTP头字段，并由IETF在Forwarded-For HTTP头字段标准化草案中正式提出。

#### 总结

这次的掉单问题算起来应该算是一个不太能够发现的坑，主要是依赖第三方库的实现，我们这边相关的同事已经把修复代码提交pull request到mochiweb的github主页了，防止有更多的人碰到这个坑。