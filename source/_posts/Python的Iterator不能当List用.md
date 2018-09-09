---
title: Python的Iterator不能当List用
date: 2018-09-09 12:49:24
tags: python
---
> 之前在重构流处理框架的时候，把在每个模块里面处理的数据类型从List，变成了pyspark里面foreachPartition输入的函数的参数，其实就是一个Iterator类型的参数，用来遍历整个Partition的数据。但是后面发现有些模块没有执行，最后发现竟然是误用Iterator造成的bug。

#### 问题
Iterator类型的数据只能遍历一次，但是List可以一直遍历，很简单的一段代码就可以说明这个问题：
```python
lst = [1, 2, 3, 4, 5]
it = iter(lst)
print([v for v in lst])
print([v for v in lst])
print([v for v in it])
print([v for v in it])
```
这段代码的输出是：
```python
[1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
[]
```
很简单的就可以看出Iterator和List的不同了，我项目中的问题就对Iterator遍历了两次，第二次遍历的代码等于没执行。这样说来Iterator是不是就都可以用List替换了，或者说Iterator就没有优势了？答案显然不是的。

#### Iterator

##### 什么是Iterator
Iterator是访问集合元素的一种方式。Iterator对象从集合的第一个元素开始访问，直到所有的元素被访问完结束。Iterator只能单向访问，且不能回退。

##### Iterator的优势
我觉得Iterator的主要优势是延迟计算，他并不像List那样需要事先把所有的元素都放到List，而是访问到的时候才产生所需要的元素，访问之后的元素如果其他地方没有用到，其占用的内存也可以被回收掉，大大的减少了集合遍历所需的内存。想象一下，如果需要访问的集合是非常巨大的话，这样的话List就需要分配非常多的内存。下面的代码举个例子：
```python
import random
def gen():
    i = 0
    while i < 1000000000:
        yield random.random()

for rand in gen():
    print(rand)
```

#### 总结
这篇文章主要了解了下List和Iterator的区别，不过在有些方法中不管你传的是Iterator还是List都会转成List类型，用List来求集合的长度，比如进程池的map函数。当集合比较小的时候不管是使用List还是Iterator，都行。