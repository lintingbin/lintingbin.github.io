---
title: 设置Shell脚本执行错误自动退出
date: 2017-07-06 21:44:10
tags: Shell
categories: 备忘
---
> 这是一篇备忘记录，以后再写Shell脚本的时候需要注意！

之前项目使用Jenkins打包的时候，有时候因为一些错误的提交，导致出包的时候编译失败，从而导致打包出来的包里面只有部分的代码，这是因为我们写的Shell脚本没有对每条Shell命令的结果进行检查，不管执行结果是否成功都会继续往下执行。所以即使我们在编译环节有错误产生，打包的脚本还是会继续执行后面的打包指令。所以必须让脚本在某条命令执行失败的时候停止执行后续的指令。  
在Shell脚本中加入：

#!/bin/bash -e
或者
set -e

就能够让脚本在有错误的时候退出。下面是网上查的拓展：

#### 使用set -e
```
你写的每一个脚本的开始都应该包含set -e。这告诉bash一但有任何一个语句返回非真的值，则退出bash。 

使用-e的好处是避免错误滚雪球般的变成严重错误，能尽早的捕获错误。更加可读的版本：set -o errexit 

使用-e把你从检查错误中解放出来。如果你忘记了检查，bash会替你做这件事。

不过你也没有办法使用$? 来获取命令执行状态了，因为bash无法获得任何非0的返回值。

你可以使用另一种结构，使用command
```

#### 使用command
``` shell
if [ "$?"-ne 0]; then echo "command failed"; exit 1; fi "

可以替换成： 

command ||  echo "command failed"; exit 1; （这种写法并不严谨，我当时的场景是执行ssh "commond"，
所以可以返回退出码后面通过[ #？ -eq 0 ]来做判断，如果是在shell中无论成功还是失败都会exit）

修改如下（谢谢评论的朋友指正）


command ||  （echo "command failed"; exit 1） ; 

或者使用： 

if ! command; then echo "command failed"; exit 1; fi
```