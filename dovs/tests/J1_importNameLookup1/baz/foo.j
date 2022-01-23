.source foo.java
.class public baz/foo
.super java/lang/Object
.field public "x" I

.method public <init>()V
.limit stack 3
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
aload_0
bipush 123
dup_x1
putfield baz/foo/x I
pop
return
.end method
