.source Foo.java
.class public foo/Foo
.super java/lang/Object

.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public method()I
.limit stack 2
.limit locals 2
new foo/List
dup
invokespecial foo/List/<init>()V
astore_1
aload_1
invokevirtual foo/List/method()I
ireturn
.end method
