---
title: airflow使用总结
date: 2019-07-20 18:48:29
tags: airflow
categories: 任务调度
---
> etl定时任务通常会使用一些任务调度的工具，我们之前使用的是azkaban，现在已经切换到airflow了。总的来说airflow的功能强大不少，但是坑也不少，以下是对airflow的一些总结。

#### 对比azkaban，airflow有哪些优势？
* airflow使用python编写，源代码比较容易理解，而且方便进行二次开发。比如可以很方便的定制自己的Operator。
* dag(也就是任务流)使用python代码实现，可以根据复杂的条件生成相应的dag，非常的灵活。
* 提供连接管理的功能，可以事先在airflow的web页面上配置各种连接，而使用者只要指定连接的名字就可以了。该功能方便让配置和实现进行解耦，以后修改连接也只需要在web页面上操作，不需要修改代码。
* 提供资源池的功能，能够让不同的任务使用同一资源池，从而通过资源池来控制任务的并发度，方便进行资源限制。
* 任务重跑也非常方便，在web页面上只要clear相应的任务，airflow的调度器就会自动重跑该任务。

当然，airflow的优势不仅仅如此，只有自己真正使用了，才能体会airflow的强大之处。不过功能越强大，也意味着使用起来也越复杂，下面是我总结的一些airflow的使用的注意事项。

#### airflow使用注意事项
* airflow默认使用sqlite数据库，使用sqlite的时候airflow的调度器使用的是SequentialExecutor，该Executor只能顺序运行任务，同一时间只能有一个任务执行。
* 如果想让一个dag同一时间只能运行一个dag实例的话，需要在配置文件里面把max_active_runs_per_dag配置成1.
* airflow默认会载入example的那些dag，如果不想载入的话，设置load_examples = False.
* airflow的api接口默认没有要求权限验证，需要配置权限验证的话，需要在[api]配置块下配置auth_backend，比如可以配置成auth_backend = airflow.contrib.auth.backends.password_auth。
* airflow的web页面如果想启用用户权限验证的话，可以配置rbac=True。注意：这边的权限和api的权限是分开配置的。
* airflow的dag需要配置一个start_date，dag执行的时候默认会从start_date开始一次次执行，直到当前时间。如果不想要这个功能的话，可以配置catchup_by_default = False，把该功能关掉。