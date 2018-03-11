---
title: git如何在本地生成多个ssh key
date: 2018-03-11 17:03:15
tags: [Git]
categories: 教程
---

在本地上传Git项目到远程时，本地需要对应的Git账户信息和允许连接的SSH key信息，远程才会允许上传。

<!-- more -->
[Windows下配置SSH连接Github]()介绍了如何生成SSH key。但当遇到需要有多个Git账户信息时怎么办呢，如果删除密钥重新创建，那每次切换账户时都要重复这样的操作，太过繁琐。

这个问题我们可以通过在`~/.ssh`目录下增加`config`文件来解决。

### 配置Git用户名和邮箱

配置Git用户名和邮箱，换成现在需要连接的Git账户信息。
```
$ git config user.name <Git注册用户名>
$ git config user.email <Git注册邮箱>
```

### 生成SSH key

生成SSH key，并指定文件名，避免覆盖原有的默认`id_rsa`文件。

```
$ ssh-keygen -t rsa -f ~/.ssh/id_rsa.another -C <Git注册邮箱>
```

> windows用户打开Git Bash来执行ssh-keygen命令。

### 配置config文件

在`~/.ssh`下添加config文件，如果已经存在，就直接打开修改。

```
$ touch ~/.ssh/config   // 创建
```

在config文件中添加如下信息。其中Host后面添加远程Git仓库域名，IdentityFile填写对应的id_rsa文件，User添加Git用户名。

```
Host github.com
    IdentityFile ~/.ssh/id_rsa.another
    User anotherUser
```

### 上传SSH key

在远程Git账号中添加SSH key，将id_rsa.another.pub中的内容全部粘贴进去。

> pub信息一般以ssh-rsa开头

### 测试SSH key

```
ssh -T git@git.com  // 或者其他域名地址
```

弹出成功信息，则表示SSH key添加成功，接下来就可以推送代码到远程了。
