---
title: python标准开发环境
date: 2018-06-23 13:44:23
tags: python
---
> 很多初学python的同学可能都知道pip，通过pip可以安装自己的需要的python第三方库。但是有很多同学可能不知道pipenv，在这篇文章我将介绍如何用pipenv来构建自己的开发环境，以及用pipenv快速部署生产环境

#### 什么是pipenv？
pipenv是一个把包管理工具和虚拟环境结合的工具，使用pipenv install和使用pip install一样，可以安装任何你想安装的第三方库，不同的是pipenv会记录你安装的库和使用的python版本，在部署生产环境的时候直接使用pipenv install就可以安装所有的依赖、甚至特定的python版本。[pipenv项目地址](https://github.com/pypa/pipenv)

#### 如何使用pipenv？
1. 开发环境使用  
项目开发的时候使用pip install在项目的目录下面安装所需要的库，pipenv会自动在项目的根目录下面产生Pipfile，Pipfile.lock这两个文件，这两个文件记录了当前项目的使用第三方库和python版本的信息，同时pipenv会为这个项目构建一个虚拟环境(pipenv --venv可以查看虚拟环境的位置)。pipenv shell可以把当前的命令行环境切换到当前项目的虚拟环境下执行。要用虚拟环境运行一个python文件的话，则使用pipenv run python a.py。
2. 生产环境使用  
当项目开发完成以后，把项目上传到需要部署该项目的服务器。然后在项目的根目录下运行pipenv install，这时候pipenv会自动为该项目创建所依赖的python的版本的虚拟环境，同时在该虚拟环境下安装所有需要的第三方库。有一个问题需要注意的是：如果机器上没有安装项目所需要的python版本的话，这时候虚拟环境就会创建失败。要解决这个失败的话有两个办法：1.安装项目所依赖的python版本。2.安装pyenv，让pipenv在创建虚拟环境的时候使用pyenv自动安装所需要的python版本。

#### 使用pyenv
pyenv是一个python包管理工具，可以在一台机器上面安装多个python版本。我推荐至少在生产环境上面安装pyenv，这样在项目代码部署到生产环境的时候根据项目的实际需要安装不同的python版本。[pyenv项目地址](https://github.com/pyenv/pyenv)

#### 总结
python程序员在开发项目的时候推荐使用pipenv，这样在项目分享给别人或者部署的时候都可以无比的轻松。同时推荐使用pyenv，pipenv和pyenv配合使用不要太爽了。这篇文章只是简单的介绍pipenv和pyenv，具体的使用方法请查阅他们的官方文档。