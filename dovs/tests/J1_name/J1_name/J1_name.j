.source J1_name.java
.class public J1_name/J1_name
.super java/lang/Object
.field public "J1_name" I

.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public test()I
.limit stack 3
.limit locals 1
aload_0
bipush 7
dup_x1
putfield J1_name/J1_name/J1_name I
pop
aload_0
bipush 9
dup_x1
putfield J1_name/J1_name/J1_name I
pop
new J1_name/J1_name
dup
invokespecial J1_name/J1_name/<init>()V
bipush 42
dup_x1
putfield J1_name/J1_name/J1_name I
pop
new J1_name/J1_name
dup
invokespecial J1_name/J1_name/<init>()V
iconst_2
dup_x1
putfield J1_name/J1_name/J1_name I
pop
bipush 123
ireturn
.end method
