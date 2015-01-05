@dnl = internal constant [4 x i8] c"%d\0A\00"
@d = internal constant [3 x i8] c"%d\00"	
@s = internal constant [3 x i8] c"%s\00" 

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...)* @printf(i8* %t0, i32 %x) 
	ret void
}

define void @printString(i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

define i32 @readInt() {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)
	%t2 = load i32* %res
	ret i32 %t2
}

define i8* @readString() {
entry: %res = alloca i8*
       %t1 = getelementptr [3 x i8]* @s, i32 0, i32 0
       %t2 = load i8** %res
	call i32 (i8*, ...)* @scanf(i8* %t1, i8* %t2)
    %t3 = load i8** %res
	ret i8* %t3
}

define void @error() {
    call void @exit(i32 1)
    ret void
}
