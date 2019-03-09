---
title: spark执行map-join优化
date: 2019-03-09 14:15:58
tags: spark, map-join
categories: 优化
---
> 在使用map reduce处理数据的时候，join操作有两种选择：一种选择是在map端执行join操作，即所谓的Map-side Join（Broadcast join）；另一种选择是在reduce端执行join操作，即所谓的Reduce-side Join（shuffle join）。在map端执行join操作，适合在有一个表比较小的情况下，能把整个表放到内存，发送到各个节点进行join操作。下面的文章主要介绍在spark使用Map-side Join的时候需要注意的问题。

#### spark使用map-join的时候可能会遇到什么问题？
我在执行以下sql语句的时候遇到了执行内存不够的问题，原先我还以为是内存不够，然后加大了内存，但是还是执行失败。
```sql
// a是一个几亿行的大表，b是一个只有几十行的小表。a和b都是由hive创建的表
select * from a where id not in (select id from b)
```
后面我在spark ui中看到了该sql的执行计划，该sql语句执行了Map-side Join操作，但是spark把a表当成了小表，准备把a表broadcast到其他的节点，然后就是一直卡在这步broadcast操作上。
造成上述问题的原因就是spark认为a表是一个小表，但是在spark ui上明显可以看到a表读了很多的行。但是为什么spark还会认为a表是一个小表呢？原因是spark判断一个hive表的大小会用hive的metastore数据来判断，因为我们的a表没有执行过ANALYZE TABLE，自然a表的metastore里面的数据就不准确了。

#### 解决方法？
既然知道了问题，要解决就很简单了。有如下几个解决方法：
##### 设置spark.sql.statistics.fallBackToHdfs=True
该参数能让spark直接读取hdfs的文件大小来判断一个表达大小，从而代替从metastore里面的获取的关于表的信息。这样spark自然能正确的判断出表的大小，从而使用b表来进行broadcast。
##### 使用hint
在使用sql语句执行的时候在sql语句里面加上mapjoin的注释，也能够达到相应的效果，比如把上述的sql语句改成:
```sql
select /*+ BROADCAST (b) */ * from a where id not in (select id from b)
```
这样spark也会使用b表来进行broadcast。
##### 使用spark代码的方式
使用broadcast函数就能达到此效果：
```python
from pyspark.sql.functions import broadcast
broadcast(spark.table("b")).join(spark.table("a"), "id").show()
```
#### 拓展知识
##### 什么时候spark才会使用Map-side Join？
只有当要进行join的表的大小小于spark.sql.autoBroadcastJoinThreshold（默认是10M）的时候，才会进行mapjoin。

#### 总结
上述的优化同样也适用于其他的计算引擎，比如Impala通过hint和执行表的位置调整也能够优化join操作，通过explain也可以查看sql的执行计划，然后再进行优化。