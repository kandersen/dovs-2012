.source Bar.java
.class public foo/Bar
.super java/lang/Object
.field public "baz" Lfoo/Baz;

.method public <init>()V
.limit stack 4
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
aload_0
new foo/Baz
dup
bipush 123
invokespecial foo/Baz/<init>(I)V
dup_x1
putfield foo/Bar/baz Lfoo/Baz;
pop
return
.end method
