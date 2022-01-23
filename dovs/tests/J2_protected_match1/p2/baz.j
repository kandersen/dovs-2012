.source baz.java
.class public p2/baz
.super java/lang/Object

.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public test(Lp1/foo;)V
.limit stack 3
.limit locals 2
aload_1
ldc "a"
ldc "b"
invokevirtual p1/foo/method(Ljava/lang/String;Ljava/lang/Object;)V
return
.end method
