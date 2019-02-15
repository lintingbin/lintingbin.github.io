---
title: Hadoop系统Parquet文件的Timestamp类型的时区问题
date: 2019-02-14 18:55:32
tags: [parquet, timestamp, hive, impala，spark, presto]
categories: 问题研究
---
> 在数据仓库离线处理中，为了能够兼容不同的数据处理引擎，我们通常会把数据保存成Parquet格式的文件。但是在不同的数据处理引擎中会出现时间读取不一致的问题，比如我用impala写入的时间，在用hive和spark读取的时候会差几个小时，这是为什么呢？

#### Hive中的Timestamp

Hive在0.8的版本后开始支持Timestamp的格式。Hive在储存时间戳的时候会先把时间转成UTC的时间，然后再把转换后的时间存储到Parquet文件中。在读取Parquet文件的时候Hive会把时间从UTC时间再转化回成本地的时间。这样的话，如果存和读取都是用Hive的话，时间不会有任何的问题。上述说的是用Parquet文件来存取时间格式流程，如果是存成普通的文本文件的话，存取都不会进行任何时间的转换。

#### Impala中的Timestamp

Impala在存取Parquet格式的时间的时候和Hive不一样，Impala在存储时间的时候不转换成UTC的时间，而是直接保存当前时区的时间。所以Impala在读的时候也不进行任何的转换，是直接读取Parquet文件中的时间。这样的话，如果存和读取都是用Impala的话，时间也不会有任何的问题。但是如果用Hive存，但是用Impala读的话，就会出现时间不一致的问题，反之亦然。 
由于让Impala和Hive之间的时间格式互相兼容是一件比较重要的事情，从而出现了让两个系统的时间格式互相兼容的解决方案。

#### 让Hive和Impala互相兼容
##### Impala正确读取Hive存储的Parquet时间
在Impala的配置里面设置 
> convert_legacy_hive_parquet_utc_timestamps=true (default false) 

该参数会让Impala在读取Parquet文件的时候，会先检查Parquet文件的meta信息，查看该Parquet文件是否由Hive创建，如果是由Hive创建的话，在读取的时候会进行时间格式的转换，这样读取出来的时间就能和Hive存进去的时间一致了。

##### Hive正确读取Impala存储的Parquet时间
同样在Hive里面有配置可以设置
> hive.parquet.timestamp.skip.conversion=true (default true)

Hive设置了该参数后，在读取Parquet文件的时候，同样会读取该Parquet文件的meta信息，如果是由Impala创建的Parquet文件的话，在读取的时候就不会进行时间格式的转换了。


#### Spark和Presto的Timestamp
Spark和Presto同Hive是兼容的，所以时间读取方面不会有问题。但是这样就意味着Spark和Presto就不能与Impala的时间相互兼容了，现在Spark和Presto似乎还没有和Impala相互兼容的解决方案。所以如果数据是由Impala来存储的，但是数据处理又是用Spark或者Presto的话，就要手动进行时区转换。

#### Parquet中的Timestamp
Parquet文件格式是当前Hadoop生态中最流行的列式存储格式。Parquet支持的类型有BOOLEAN、INT32、INT64、INT96、FLOAT、DOUBLE、BYTE_ARRAY，所以Timestamp其实是一种逻辑类型。由于Impala存储的时间精度达到纳秒的级别，所以在Parquet文件中用INT96来存储时间。其他的数据处理引擎也跟进该精度，所以也用INT96来存储，但是在时区兼容性方面做得并不好。

#### 总结
不同的计算引擎之间的时间兼容性还是有一定的问题的，Hive和Impala之间有现成的解决方案来解决时间兼容性问题，但是Impala和Spark还有Presto之间暂时还没有好的解决方案，一个比较好的解决方法使用string来存储时间，这样不同的数据处理引擎之间就不会有时区问题了。
