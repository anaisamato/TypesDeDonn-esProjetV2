.class MyClass
.super java/lang/Object


.method static even(II)I
  .limit stack 10
  .limit locals 10
  lab_0:
     sipush 3
  lab_1:
     sipush 3
  lab_2:
     if_icmpeq lab_6
  lab_3:
     iload 1
  lab_4:
     sipush 1
  lab_5:
     iadd
  lab_6:
     sipush 0
     ireturn
.end method

