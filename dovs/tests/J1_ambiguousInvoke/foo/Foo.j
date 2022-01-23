.source Foo.java
.class public foo/Foo
.super java/lang/Object

.method public static bar()V
.limit stack 2
.limit locals 0
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "bar"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method

.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public test()V
.limit stack 0
.limit locals 1
invokestatic foo/Foo/bar()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 2
.limit locals 1
new foo/Foo
dup
invokespecial foo/Foo/<init>()V
invokevirtual foo/Foo/test()V
return
.end method
