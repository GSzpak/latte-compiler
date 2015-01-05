@.str0 = private constant [3 x i8] c"OK\00"

declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8* , i8*)
declare i8* @strcat(i8* , i8*)

define i32 @main() {
   label1:
      %r0 = alloca i1
      store i1 true, i1* %r0
      %r4 = load i1* %r0
      %r3 = icmp sgt i1 %r5, false
      br i1 %r3, label %label2, label %label3

   label2:
      %r2 = getelementptr inbounds [3 x i8]* @.str0, i32 0, i32 0
      call void @printString(i8* %r2)
      br label %label3

   label3:
      ret void

   label4:
      br label %label5

   label5:
      %r5 = phi i1 [ true, %label1 ] , [ false, %label4 ]
      ret i32 0

}

