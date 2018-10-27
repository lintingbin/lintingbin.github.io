---
title: 使用django-guardian实现django-admin的行级权限控制
date: 2018-10-27 20:03:29
tags: [django, guardian]
categories: python
---
> 用django框架来做一些后台管理的web页面简直太方便了，django自带模块级的权限系统，用来做一些内部的系统非常合适，可以大大的减少开发量。但是django自带的权限系统还不支持行级的权限控制，如果要实现行级的权限控制，需要依赖第三方的app来开发，或者自己重新写一个。

#### 需求描述
我们项目组开发的一些系统通常会用mysql数据库来存储一些配置，但是如果每次有配置修改的时候都去手动修改mysql数据的话，会挺麻烦的，同时也比较容易出错。django-admin能够根据定义的model自动的生成相应的页面，同时还能提供权限的管理，所以我们就把一些系统到的配置放到django中。但是到现在，随着系统的需求越来越多，该系统已经不止我们自己项目组的人员使用，也要开放给其他项目组的同事使用，所以就产生了一些更细粒度的权限需求。因此，我们要在现有的系统上支持行级的权限控制。

#### 解决方案
当然可以自己写一套权限系统了，但是自己写的成本比较高，而且自己写的不一定比较好。所以我就先在网上找了一些现成的解决方案，https://djangopackages.org/grids/g/perms/ 该链接列出了现有的一些第三方的权限系统解决方案。从该页面来看，django-guardian是最受欢迎的第三方权限系统，而且支持行级的权限系统，同时还可以整合到django-admin里面，所以我就选择了django-guardian。

#### 关键步骤
##### 安装配置django-guardian
安装配置django-guardian比较简单，按照她项目提供的[文档](https://django-guardian.readthedocs.io/en/stable/)进行安装就可以了，安装完成后会在数据库里面创建两张权限相关的表。
##### 把django-guardian整合到django-admin
首先把admin.py文件里面需要用到行级权限的类，由原来的继承admin.ModelAdmin，改成继承GuardedModelAdmin，这时候打开某个数据行的页面的时候，在该页面的右上角的历史旁边会显示编辑对象权限的按钮，点击该按钮进去相应的页面就可以编辑该行数据的具体权限。  
配置完权限的时候，用一个新的用户测试的话，会发现该用户没有权限来访问任何的数据，这是因为GuardedModelAdmin还有很多事情没有帮我们做，我们还需要重写一些函数来实现admin后台页面的显示。具体的信息看下面的代码注释。
```python
from guardian.admin import GuardedModelAdmin
from guardian.shortcuts import get_objects_for_user, assign_perm, remove_perm, get_users_with_perms, \
    get_groups_with_perms
    
# 需改前
@admin.register(DataAssistantJob)
class DataAssistantJobAdmin(admin.ModelAdmin):
    pass

# 修改后
@admin.register(DataAssistantJob)
class DataAssistantJobAdmin(GuardedModelAdmin):
    # app是否在主页面中显示的话由该函数决定
    def has_module_permission(self, request):
        if super().has_module_permission(request):
            return True
        return self.get_model_objs(request).exists()

    # 在显示数据列表额时候，哪些数据显示，哪些不显示，由该函数控制
    def get_queryset(self, request):
        if request.user.is_superuser:
            return super().get_queryset(request)

        data = self.get_model_objs(request)
        return data
        
    # 内部用来获取某个用户有权限访问的数据行
    def get_model_objs(self, request, action=None, klass=None):
        opts = self.opts
        actions = [action] if action else ['view', 'change', 'delete']
        klass = klass if klass else opts.model
        model_name = klass._meta.model_name
        return get_objects_for_user(user=request.user, perms=[f'{perm}_{model_name}' for perm in actions],
                                    klass=klass, any_perm=True)

    # 用来判断某个用户是否有某个数据行的权限
    def has_perm(self, request, obj, action):
        opts = self.opts
        codename = f'{action}_{opts.model_name}'
        if obj:
            return request.user.has_perm(f'{opts.app_label}.{codename}', obj)
        else:
            return self.get_model_objs(request, action).exists()

    # 是否有查看某个数据行的权限
    def has_view_permission(self, request, obj=None):
        return self.has_perm(request, obj, 'view')

    # 是否有修改某个数据行的权限
    def has_change_permission(self, request, obj=None):
        return self.has_perm(request, obj, 'change')

    # 是否有删除某个数据行的权限
    def has_delete_permission(self, request, obj=None):
        return self.has_perm(request, obj, 'delete')

    # 用户应该拥有他新增的数据行的所有权限
    def save_model(self, request, obj, form, change):
        result = super().save_model(request, obj, form, change)
        if not request.user.is_superuser and not change:
            opts = self.opts
            actions = ['view', 'add', 'change', 'delete']
            [assign_perm(f'{opts.app_label}.{action}_{opts.model_name}', request.user, obj) for action in actions]
        return result
```
通过上面的修改，django-admin中的模块就能够支持行级的权限，并能够正确的在后台页面中显示出来，当然如果有很多的模块需要支持行级的权限控制，则可以把上面的这些修改写到一个新的类中，然后其他想支持行级权限的模块再从该模块继承就可以了。

#### 总结
感觉django-guardian和django-admin整合，实现的不是很好。如果开发者对django内部的代码不怎么了解，那么用django-guardian来实现行级权限控制的话会挺麻烦的，个人认为django-guardian完全可以把和django-admin的整合做到开箱即用的效果，就像django自带的权限系统一样。