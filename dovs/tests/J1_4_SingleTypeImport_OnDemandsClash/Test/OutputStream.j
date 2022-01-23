.source OutputStream.java
.class public Test/OutputStream
.super java/lang/Object

.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public write(I)Ljava/lang/String;
.limit stack 2
.limit locals 3
ldc ""
astore_2
goto cond1
loop0:
aload_2
invokestatic java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;
ldc "All fun and no play makes Jack a dull boy"
invokevirtual java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;
dup
astore_2
pop
iload_1
bipush 41
isub
dup
istore_1
pop
cond1:
iload_1
bipush 41
if_icmpge false3
iconst_0
goto end2
false3:
iconst_1
end2:
nop
ifne loop0
goto cond5
loop4:
aload_2
invokestatic java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;
ldc "."
invokevirtual java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;
dup
astore_2
pop
iload_1
iconst_1
isub
dup
istore_1
pop
cond5:
iload_1
iconst_0
if_icmpgt false7
iconst_0
goto end6
false7:
iconst_1
end6:
nop
ifne loop4
aload_2
areturn
.end method
