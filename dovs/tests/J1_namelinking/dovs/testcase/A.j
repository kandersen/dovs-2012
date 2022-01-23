.source A.java
.class public dovs/testcase/A
.super java/lang/Object
.field public "f" I

.method public <init>()V
.limit stack 2
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
aload_0
getstatic org/omg/CORBA/ARG_IN/value I
putfield dovs/testcase/A/f I
return
.end method

.method public m()I
.limit stack 1
.limit locals 1
getstatic org/omg/CORBA/ARG_IN/value I
ireturn
.end method

.method public static m(I)I
.limit stack 2
.limit locals 1
iload_0
getstatic org/omg/CORBA/ARG_IN/value I
iadd
ireturn
.end method

.method public static test()I
.limit stack 3
.limit locals 1
new dovs/testcase/A
dup
invokespecial dovs/testcase/A/<init>()V
astore_0
aload_0
aload_0
getfield dovs/testcase/A/f I
aload_0
invokevirtual dovs/testcase/A/m()I
iadd
aload_0
getfield dovs/testcase/A/f I
invokestatic dovs/testcase/A/m(I)I
iadd
getstatic org/omg/CORBA/ARG_IN/value I
iadd
dup_x1
putfield dovs/testcase/A/f I
pop
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "qualified"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "unqualified"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
bipush 118
aload_0
getfield dovs/testcase/A/f I
iadd
ireturn
.end method
