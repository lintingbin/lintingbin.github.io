---
title: 使用spark进行流处理
date: 2018-05-04 23:34:53
tags: spark
categories: 流处理
---
> 最近在做一个假量检测的项目，主要是用来检测是否有一些伪造的广告点击之类的，然后该项目使用了spark来做在线的流处理

#### spark的使用场景
spark主要用来读取kafka里面的一些点击、安装、登入和登出等数据，然后使用spark的流处理模块对这些数据进行处理，最后把处理完的数据存储到相应的数据库中，供后面的数据分析使用。

#### 使用的spark流处理模块
spark的流处理模块有两个：
* Spark Streaming(Dstream) 老的接口
* Structured Streaming 新的接口

我们的项目使用了Dstream实现流处理，一个主要的原因是在新的Structured Streaming中我们不能获取到读取的kafka的offset，这样当我们有数据处理失败的时候就不能从相应的offset中恢复继续运行，虽然可以设置checkpoint来恢复失败的任务，但是checkpoint的恢复是基于任务的，不能对该任务进行修改，然后再重新运行。  
对于怎么在Structured Streaming中获取offset，我查了一些资料，如果实在是想获取offset的话也可以通过读取checkpoint文件夹下面的offset文件夹来获取当前的offset，不过这种方法比较奇怪。还有一个方法是使用StreamingQueryListener类里面的onQueryProgress回调来获取当前执行的状态，其中包括offset的信息，但是非常遗憾这种方法只支持scala和java，而我们的开发语言是python。下面的链接是该问题的具体讨论：[如何从Structed streaming中获取offset的问题。](https://stackoverflow.com/questions/46153105/how-to-get-kafka-offsets-for-structured-query-for-manual-and-reliable-offset-man/46174353)

#### 在一个流中处理多个topic
感觉spark的api设置的非常不友好，想要在一个流中处理多个topic也挺麻烦的，主要的问题如下：
* 如果使用Dstream，在创建Dstream的时候可以传入多个topic，这样貌似可以解决读取多个topic的问题，但是有一个很严重的问题，读取到的内容你不知道是属于哪个topic，这样你就不能对不同的topic执行不同的处理了。
* 如果使用Structured Streaming，也可以在DataStreamReader中指定多个topic，而且传入的每行数据中也有相应的topic信息，是可以根据不同的topic来调用不同的处理方法的。但是如上面所说的，Structured Streaming不支持获取offset让我们放弃了它。

最后我们的处理方法是在一个流中建立多个Dstream，在每个Dstream中拉取和处理同一个topic的数据，这样一个流就可以处理多个topic了，示例代码如下所示：
```python
for topic in topic_info:
    from_offsets = restore_off_sets(topic)
    DStream = KafkaUtils.createDirectStream(ssc, [topic], kafka_params, from_offsets)
    DStream.transform((lambda t: lambda rdd: get_offset_ranges(t, rdd))(topic))\
        .map(lambda x: x[1])\
        .foreachRDD((lambda t: lambda rdd: process_rdd(t, rdd))(topic))
```

#### 总结
第一次使用spark，感觉spark的接口设置不是很友好，而且文档写的也不是很友好。比如foreachRDD的回调函数如果是两个参数的函数的话，第一个参数就是时间，这个在文档中没有提及，一不注意就有奇怪的bug了。总之自己还是一个菜鸟，还要多多学习。