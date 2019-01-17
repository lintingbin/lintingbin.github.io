---
title: 为集群搭建DNS服务的过程记录
date: 2018-11-01 11:56:33
tags: [dns, dnsmasq, nscd]
categories: 教程
---
> 之前集群维护hostname和ip之间的映射关系使用的是/etc/hosts文件，但是由于每次新加机器的时候都要更新每台机器的/etc/hosts文件，比较繁琐而且容易出错，所以就考虑为集群增加dns服务。

#### 搭建dns服务器
我们这边的dns服务器是运维直接帮我搭的，使用的是dnsmasq。相比于bind，dnsmasq的主要优势是轻量级、配置简单，然后我们机器的hostname和ip的映射文件放于这台机器的`/etc/hosts`文件下面，每次增加新机器的时候只要修改`/etc/hosts`文件，然后重启dnsmasq就可以了。

#### 集群机器的配置
集群的机器配置主要是修改dns服务器的地址，把`/etc/resolv.conf`文件的内容修改为：`nameserver 192.168.1.1（你自己的dns服务器地址）`。  
如果只是修改`/etc/resolv.conf`文件的话，centos7会在重启的时候重新生成该文件，把修改的内容覆盖。要解决这个问题的话需要在`/etc/sysconfig/network-scripts/ifcfg-eth0`文件中增加`DNS1=8.8.8.8`和`DNS2=8.8.4.4`类似配置，重启后`/etc/resolv.conf`文件的内容就会从`ifcfg-eth0`文件中生成。  
在这步，我遇到了一个问题：用dig和nslookup可以找到一个hostname对应的ip，但是使用ping或者telnet的时候就会返回`Name or service not known`。我在网上查了很多的资料，有人遇到类似的问题，但是他们的问题是因为`/etc/nsswitch.conf`的host配置没配好，而我这边并没有该配置问题。最后我在一个帖子回答中找到了解决方法，原来是我启用了nscd服务，这样的话，修改dns服务器就需要把该服务重启下，重启完后问题就解决了。

#### 配置优化
虽然经过上述的步骤后，集群的机器就可以使用dns服务了，但是还需要一些优化，才能让dns服务有更好的性能。
##### 启用nscd服务
在centos7中默认是没有开启nscd服务的，这样的话，每次查询一个域名都需要向dns服务器发起一次请求，如果请求非常多的话就会给dns服务器带来压力，所以我们需要启用nscd服务。如果机器没有安装该服务的话，可以使用命令`yum install nscd`进行安装，然后使用命令`systemctl start nscd`启动该服务，使用命令`systemctl enable nscd`把该服务设置成开机启动。
#### 配置dnsmasq的local-ttl参数
集群的机器启动了nscd服务以后，按道理对dns服务器的请求会大大的减少，但是我在dns服务器上使用`tcpdump udp port 53`查看集群机器对dns服务器的请求的时候，集群机器会一直重复的请求一个hostname的ip，nscd根本没有起到缓存到的效果，如下图所示：
![tcpdump显示log](/images/dns_request.png)
后面查看dnsmasq的man手册知道，dnsmasq的local-ttl参数是控制本地域名的缓存时间的，比如存在`/etc/hosts`下的域名映射关系的缓存时间就是由local-ttl参数控制的，该值默认是0。我们集群的所有机器的hostname和ip的映射关系都是存储在dns服务器的`/etc/hosts`文件中的，所以这些域名在集群机器中是不缓存的，所以集群机器才会一直重复的请求某一个hostname的ip。最后为local-ttl配置一个适当值后该问题就解决了。

#### 总结
为集群配置dns服务看起来是一件比较简单的事情，但是如果不知道其中的一些坑的话，配置起来就会挺费精力的，我就花了不少的时间。希望这篇文章能让其他人少走一些弯路。

