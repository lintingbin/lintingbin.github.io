---
title: 使用HEXO在Github上搭建个人博客
date: 2017-04-29 15:40:51
tags: [Hexo, Github]
categories: 教程
---
在平时的工作中经常会遇到一些问题，在解决问题的时候如果能够及时记录下来是最好不过的，所以一直想维护一个自己的博客。虽然国内有各种技术博客（比如：CSDN，博客园）之类的第三方博客平台，但是作为一个程序员，不搭建一个自己的博客感觉不够酷。所以我就选择使用HEXO在Github上面搭建自己的个人博客。

下面的安装教程都是在Window x64的环境下进行的

#### 安装步骤

> 申请Github账户

由于博客是要搭建在Github上面的，所有必须要有一个Github账号来上传代码，这样才能最终显示自己的博客内容。在建立完Github账号后，需要创建一个Repositories，这个Repositories的名字的格式是:your_user_name.github.io这样的。

> 安装Git软件

有了Github账号后还需要有软件能把本地的代码上传到Github上面，所就安装Git软件，安装Git也非常简单，直接下一步就行了。

> 安装NodeJs

由于Hexo是基于NodeJs的框架，所以使用Hexo前要先安装NodeJs，安装NodeJs也非常简单，只需要下载软件，点下一步就行了。现在新的版本的NodeJs，会同时安装npm（Node包管理软件），所以安装起来非常简单。

> 安装Hexo

把上面的软件都安装好了之后就可以开始安装Hexo了，打开window的终端，在终端中输入下面的命令开始安装Hexo

{% codeblock %}
npm install -g hexo
{% endcodeblock %}

#### 使用步骤

> 初始化

创建一个文件夹，如：MyBlog之类，然后进到MyBlog文件夹下执行以下初始化命令

{% codeblock %}
hexo init
{% endcodeblock %}

到了这一步之后，Hexo算初始化完成，可以正常的使用了。

> 生成静态页面

继续在MyBlog目录下执行如下命令，生成静态页面

{% codeblock %}
hexo generate // 简写 hexo g
{% endcodeblock %}

> 本地启动

启动本地服务，进行文章预览调试，命令：

{% codeblock %}
hexo server   // 动态启动，有修改发生会自动检测，简写 hexo s
{% endcodeblock %}

然后在浏览器输入 http://localhost:4000 就可以看到博客的页面，当然也在服务器启动的时候加上-p来指定自己想要的端口

#### 部署步骤

> 安装 hexo-deployer-git

{% codeblock %}
npm install hexo-deployer-git --save
{% endcodeblock %}

> 配置部署环境

在MyBlog的目录下会有一个_config.yml的文件，该文件为Hexo项目的配置文件，打开该文件然后把deploy部分改成下列格式

{% codeblock %}
deploy:
  type: git
  repository: https://github.com/lintingbin2009/lintingbin2009.github.io.git  // lintingbin2009替换成你自己的名字
  branch: master
{% endcodeblock %}

> 开始部署

{% codeblock %}
hexo deploy
{% endcodeblock %}

部署完成之后就可以使用your_username.github.io来访问你的个人博客了, 之后的部署命令应该是

{% codeblock %}
hexo clean
hexo generate
hexo deploy
{% endcodeblock %}

#### 总结

总的来说用Hexo在Github上搭建个人博客还是比较简单的，当然这边只是涉及到最简单的搭建，还没有涉及到主题的更换、评论系统，统计系统。更多关于Hexo的使用文档可以浏览Hexo的中文官网，里面有详细的使用教程和很多可选的精美主题。