---
title: 使用k8s部署Django项目
date: 2019-01-13 15:51:19
tags: [k8s, docker, django]
categories: python
---
> 接触了一下docker和k8s，感觉是非常不错的东西。能够方便的部署线上环境，而且还能够更好的利用机器的资源，感觉是以后的大趋势。最近刚好有一个基于django的项目，所以就把这个项目打包到docker里面，放到k8是里面运行，顺便学习下k8s和docker的使用。

#### docker
##### 为什么使用docker？
我觉得docker最大的好处是部署的时候比较方便，一个预先打包好的docker镜像，可以在任何安装有docker的机器上面直接运行，不用再安装其他任何的依赖环境。不管是在开发、测试、还是发布阶段，都能节省很多安装依赖和配置文件的时间，真正做到了Build once, Run anywhere。
##### docker在我的项目中怎么使用？
在我的项目中，我主要使用dockerfile来生成项目的镜像。我们都知道docker是按照层的思想来构建一个镜像的，我的镜像的最底层的操作系统使用的是centos7，再接着安装python相关的工具和库，然后安装项目所需求的库，最后再把项目拷到镜像中。
```dockerfile
FROM centos:7

ENV LC_ALL=en_US.utf-8 LANG=en_US.utf-8
RUN yum install -y https://centos7.iuscommunity.org/ius-release.rpm && \
    yum install -y python36u python36u-libs python36u-devel python36u-pip mysql-devel gcc which && \
    pip3.6 install pipenv

COPY Pipfile Pipfile.lock /my_app/
WORKDIR /my_app
RUN pipenv sync

COPY my_app_site /my_app/my_app_site
COPY gunicorn_config.py /my_app/gunicorn_config.py
COPY resource/nginx.conf /my_app/resource/nginx.conf

RUN mkdir /static/ && \
    cd my_app_site && \
    pipenv run python manage.py collectstatic && \
    cd my_app_site && \
    rm -f local_settings.py

WORKDIR /my_app

EXPOSE 8000
CMD pipenv run gunicorn my_app_site.wsgi -c gunicorn_config.py --log-file logs/gunicorn.log
```
在dockerfile中，每个RUN命令都会构建新的层，我这边之所以在dockerfile中使用三个RUN命令，是为了能够尽量的减少重复的构建过程。每次在构建镜像的时候docker都会判断每层的内容是否有修改，如果没有修改的话，就不需要重复的构建。所以在应用开发的过程中，上面的dockerfile最多也就重新构建最后一层和倒数第二层（在项目有新包加入的时候才重新构建倒数第二层，不然正常情况下就只会重新构建最后一层）。

#### k8s
##### 为什么使用k8s？
k8s的功能非常强大。不过简单的来说，k8s是用来管理容器的一个工具。有了k8s以后我们就能让k8s自动的去拉取docker镜像，并且根据需要来启动、关闭、调度docker容器，实现一些牛逼的自动化运维操作。
##### k8s在我的项目中怎么使用？
我使用了yaml文件定义了一个k8s部署，下面是具体的文件示例：
```yaml
# ------------------- MyApp Deployment ------------------- #

kind: Deployment
apiVersion: apps/v1beta2
metadata:
  labels:
    k8s-app: my_app
  name: my_app
spec:
  replicas: 2
  revisionHistoryLimit: 10
  selector:
    matchLabels:
      k8s-app: my_app
  template:
    metadata:
      labels:
        k8s-app: my_app
    spec:
      volumes:
      - name: nginx-config
        emptyDir: {}
      - name: static-dir
        emptyDir: {}
      containers:
      - name: my_app-web
        image: my_app:latest
        ports:
        - containerPort: 8000
          protocol: TCP
        volumeMounts:
        - name: nginx-config
          mountPath: /etc/nginx/conf.d/
        - name: static-dir
          mountPath: /usr/share/nginx/html/my_app/static
        command: ["/bin/sh"]
        args: ["-c", "cp resource/nginx.conf /etc/nginx/conf.d/ && \
              cp /static/* /usr/share/nginx/html/my_app/static -rf && \
              pipenv run gunicorn my_app.wsgi -c gunicorn_config.py"]
      - name: my_app-nginx
        image: nginx:1.15.8
        ports:
          - containerPort: 8899
            protocol: TCP
        volumeMounts:
          - name: nginx-config
            mountPath: /etc/nginx/conf.d/
          - name: static-dir
            mountPath: /usr/share/nginx/html/my_app/static
        livenessProbe:
          httpGet:
            scheme: HTTP
            path: /
            port: 8899
          initialDelaySeconds: 30
          timeoutSeconds: 30
---

# ------------------- MyApp Service ------------------- #

kind: Service
apiVersion: v1
metadata:
  labels:
    k8s-app: my_app
  name: my_app
spec:
  ports:
    - port: 8899
      targetPort: 8899
  selector:
    k8s-app: my_app
  type: NodePort
```
django项目在部署的时候需要用到nginx服务器，所以在部署的时候我在同一个pod里面也加入了一个nginx镜像，不过为了少打包一个新的nginx镜像（带有nginx配置文件和静态文件的nginx镜像），我在两个容器之间使用volumes来共享django静态文件和nginx配置文件。
#### 总结
以上就是我使用docker和k8s来部署django项目的一个示例，文章里面没有包含一些具体的docker和k8s的介绍，是因为这些内容比较多，在这篇小文章里面放不下，感兴趣的同学可以去他们官网详细了解。最后我只想说，docker和k8s真是好东西，没有用过的同学赶紧去试试吧！