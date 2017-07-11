---
layout: post
title: 动态生成 Java 对象 
date: 2017-07-11T11:03:06+08:00
author: habens_chen
modified:
categories: blog
excerpt:
tags: [dynamic, object]
image:
  feature:
comments: true
share: true
---

Java 是一个静态的对象世界， 这导致即使是在数据转换过程中产生的临时数据，也必须写好一个 Class 去处理。

若临时数据的结构固定，可以通过事先写好的 Class 去处理，但若临时数据的结构需要根据用户输入来`动态生成`时，就没法事先写好一个 Class 出来了。

以下是一种生成动态 object 的一种实现：
1. 利用 `javassist` 动态构造一个 Class
2. 根据 Class 生成一个 object 实例，并利用 SpringBoot 的工具类 `ReflectionUtils` 设置 object 实例的各个属性。

```java
public class DynamicObjectUtils {
    public static Class prepareClass(String className, List<String> fields) throws CannotCompileException {
        ClassPool pool = ClassPool.getDefault();
        CtClass evalClass = pool.makeClass(className + UUID.randomUUID().toString());
        for ( String field : fields) {
            CtField ctField = CtField.make("public java.lang.String " + field + ";", evalClass);
            evalClass.addField(ctField);
            evalClass.addMethod(CtNewMethod.getter("get" + capitalize(field), ctField));
        }
        return evalClass.toClass();
    }
    
    public static Object createObject(Class clazz, List<String> fields, Map<String, Object> values) {
        try {
            Object obj = clazz.newInstance();
            for (String field : fields) {
                    ReflectionUtils.setField(clazz.getField(field), obj, values.get(field));
            }
            return obj;
        } catch (Exception e) {
            return new BeanCreationException(String.format("class name: %s", clazz));
        }
    }
}
```

这里用一个测试来验证这种方式可以正常工作：

```java
public class DynamicObjectUtilsTest {
    @Test
    public void shouldGenerateDynamicObject() throws CannotCompileException, NoSuchFieldException, IllegalAccessException {
        // prepare the class's fields
        List<String> fields = Lists.newArrayList();
        fields.add("firstName");
        fields.add("lastName");

        // prepare the object's values
        Map<String, Object> values = Maps.newHashMap();
        values.put("firstName", "Chen");
        values.put("lastName", "Ranger");

        // create class
        Class clazz = prepareClass("Name", fields);

        // create object
        Object object = createObject(clazz, fields, values);

        // verification
        assertThat(object.getClass().getField("firstName").get(object), Is.<Object>is("Chen"));
        assertThat(object.getClass().getField("lastName").get(object), Is.<Object>is("Ranger"));
    }
}
```

测试通过，看来这种方式是可以工作的。
