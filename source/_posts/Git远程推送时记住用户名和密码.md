---
title: Git远程推送时记住用户名和密码
date: 2018-03-11 17:04:20
tags: [Git]
categories: 教程
---

当使用HTTPS协议推送代码到Git仓库时，发现每次都需要输入密码，操作起来非常麻烦。下面介绍几种免去输入密码的方法。

<!-- more -->

### HTTPS协议推送

使用HTTPS协议，有一种简单粗暴的方式是在远程地址中带上密码。

```
> git remote set-url origin http://yourname:password@bitbucket.org/yourname/project.git
```

还有一种方法，是创建文件存储Git用户名和密码。

以Windows环境为例，在%USERPROFILE%目录中（一般为C:\Users\yourname)，打开Git Bash命令行，创建文件

```
> touch .git-credentials
```

在文件中输入仓库域名，这里使用了`bitbucket.org`。

```
https://yourname:password@bitbucket.org
```

在CMD终端中设置在全局Git环境中，长期存储密码

```
> git config --global credential.helper store
```

> 其他设置密码方式
> 记住密码（默认15分钟）：`git config --global credential.helper cache`
> 自定义存储时间：`git config credential.helper 'cache --timeout=3600'`

### SSH协议推送

如果原来的推送地址协议是HTTPS，可以通过换成SSH协议，在远程仓库添加SSH Key来实现推送时免账户密码输入。

```
> git remote -v             // 查看远程地址
> git remote rm origin      // 删除原有的推送地址
> git remote add origin git@github.com:<用户名>/版本库名
```

或者

```
> git remote -v
> git remote set-url origin git@github.com:<用户名>/版本库名
```

执行推送。
```
> git push -u origin master
```

发现提示权限不够。
```
The authenticity of host 'bitbucket.org (104.192.143.1)' can't be established.
RSA key fingerprint is SHA256:zzXQOXSRBEiUtuE8AikJYKwbHaxvSc0ojez9YXaGp1A.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added 'bitbucket.org,104.192.143.1' (RSA) to the list of kn
own hosts.
Permission denied (publickey).
fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.
```

需要在本地创建该帐号的RSA Key。可以参考以下两篇文章：
{% post_link Windows下配置SSH连接Github Windows下配置SSH连接Github %}
{% post_link Git如何在本地生成多个SSH-key Git如何在本地生成多个SSH key %}

然后再执行推送。
```
> git push -u origin master
```

就可以推送成功了。
