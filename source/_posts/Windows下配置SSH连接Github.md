---
title: Windows下配置SSH连接Github
date: 2018-03-11 16:55:55
tags: [Git]
categories: 教程
---

当使用git协议推送本地代码到远程时，需要配置ssh连接到GitHub。

<!-- more -->

## 设置账户
打开系统cmd。设置Git的user name和email。

```
> git config --global user.name "username"
> git config --global user.email "username@domain.com"
```

## 生成ssh key
* 查看ssh密钥

查看是否已经有了ssh密钥。

```
> C:
> cd %USERPROFILE%/.ssh
```

默认.ssh文件夹会在用户文件夹中生成。可以前往该路径查看。如果没有该文件夹，终端会提示`系统找不到指定的路径`,有的话可以备份删除。

> %USERPROFILE%是环境变量，表示当前用户文件夹路径。

> 因为%USERPROFILE%一般在C盘，从其他盘无法直接cd到该路径，所以需要先切换到c盘。

* 生成密钥

```
> ssh-keygen -t rsa -C "username@domain.com"
```

或者

```
ssh-keygen -t rsa -C "username@domain.com" -f %USERPROFILE%/.ssh/githug_blog_keys #生成ssh key的名称为githug_blog_keys，慎用容易出现其它异常。
```

产生如下交互提示:

```
Generating public/private rsa key pair.
Enter file in which to save the key (/c/Users/username/.ssh/id_rsa):
Created directory '/c/Users/username/.ssh'.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /c/Users/username/.ssh/id_rsa.
Your public key has been saved in /c/Users/username/.ssh/id_rsa.pub.
The key fingerprint is:
SHA256:ggjGi2UqwYTJR9TP6fdpqsY+COeMdeTPL+IW0mo4eUI username@domain.com
The key's randomart image is:
+---[RSA 2048]----+
|oooo.            |
|*. . .           |
|o++   o .        |
|o*.. . =         |
|+.. . * S        |
|.  E + B .       |
|  . X * = . .    |
|   * B * + +     |
|    = =++.=.     |
+----[SHA256]-----+
```

命令生成两个文件，默认名称是id_rsa和id_rsa.pub。
复制id_rsa.pub中的内容到远程GitHub账号中就行了。

> 默认下，cmd无法识别ssh-keygen命令。可以打开git bash 执行上述命令。

> 如何找到http://github.com上的ssh设置：右上角图标（view profile and more）=》Settings =》 SSH and GPG keys，在Key输入框中输入公钥。公钥内容形如：

```
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2cauWf8lNjbED9RvLBWoEXB3Lq5NRLWqVYSaFcTmZ1Qsak2LdR/9bkdTVMsTyqVxnB+bDMlVJlvPP6Zv5dDwEdcdytALUGCSdOXAmRtgxHZPEnKl8Hyl5wZdSNi0mwXYbjpUZ7HEw3vU8K/5whVVCbIzkqnlLAs9nXdORNkidOZRnyt+ETQzU/F1KVUb9HoMbB1Cw0zLvWTRIOHRXa2mKjNHS7W9HJiPEWQaeEXwh1CoredCxs0K7+KBfMkdgNYnDudtz2/AmH7qHnaVsdYNkg1V4XvWJ8Yn7Pkw1SpvTkaXXCiyA5wsPGgLFWSB+dgNroGwqw4X96/ZUfFHDtD/t yanyinhong@baidu.com

```