---
title: Markdown语法速记
date: 2017-06-17 22:44:26
tags: Markdown
categories: 教程
---
> 有时候自己也会忘记Markdown的语法，在这边做一个备忘，以后找起来比较方便，这边记录的是最基本的Markdown语法。

#### 粗体和斜体
```
_下划线是斜体_
```
_下划线是斜体_ 
```
**两个星是粗体**
```
**两个星是粗体**
```
**_粗体斜体一起用_**
```
**_粗体斜体一起用_**

#### 六种标题 
几个#号代表标题几,#号后面有空格
```
# 标题1
## 标题2
### 标题3
#### 标题4
##### 标题5
###### 标题6
```
#### 链接

``` erlang
这是一个 [普通的链接方式](https://www.github.com)
这是一个 [引用的链接方式][another place].
这还是一个 [引用的链接方式][another-link].

[another place]: https://www.github.com
[another-link]: https://www.google.com
```

这是一个 [普通的链接方式](https://www.github.com)
这是一个 [引用的链接方式][another place].
这还是一个 [引用的链接方式][another-link].

[another place]: https://www.github.com
[another-link]: https://www.google.com
#### 图片
```
![我的头像](https://lintingbin2009.github.io/img/avatar.jpg)
![又是一个头像][other]

[other]: https://lintingbin2009.github.io/img/avatar.jpg
```
![我的头像](https://lintingbin2009.github.io/img/avatar.jpg)
![又是一个头像][other]

[other]: https://lintingbin2009.github.io/img/avatar.jpg
#### 引用
```
>在要被引用的段落或者行前面加大括号
>
>即使是空行也要加一下，保持一致
```
>在要被引用的段落或者行前面加大括号
>
>即使是空行也要加一下，保持一致

#### 列表
```
1. 有序用数字
 继续保持缩进,只需加空格
2. 有序用数字
 * 无序用星号
    * 还可再缩进,只需再加空格
```
1. 有序用数字
 继续保持缩进,只需加空格
2. 有序用数字
 * 无序用星号
    * 还可再缩进,只需再加空格

#### 段落
```
我在逗号后加了两个空格,  
所以不在一行
```
我在逗号后加了两个空格,  
所以不在一行
