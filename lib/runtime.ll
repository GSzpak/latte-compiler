@dnl = internal constant [4 x i8] c"%d\0A\00"
@d = internal constant [3 x i8] c"%d\00"	
@s = internal constant [8 x i8] c"%[^\09\0A]s\00" 
@err = internal constant [14 x i8] c"RUNTIME ERROR\00"

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
    %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...)* @printf(i8* %t0, i32 %x) 
	ret void
}

define void @printString(i8* %s) {
    call i32 @puts(i8* %s)
	ret void
}

define i32 @readInt() {
    %res = alloca i32
    %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)
	%t2 = load i32* %res
	ret i32 %t2
}

define i8* @readString() {
    %res = alloca i8*
    %t1 = call i8* @malloc(i32 256)
    store i8* %t1, i8** %res
    %t2 = getelementptr [8 x i8]* @s, i32 0, i32 0
    %t3 = load i8** %res
	call i32 (i8*, ...)* @scanf(i8* %t2, i8* %t3)
    %t4 = load i8** %res
	ret i8* %t4
}

define void @error() {
    %t1 = getelementptr [14 x i8]* @err, i32 0, i32 0
    call void @printString(i8* %t1)
    call void @exit(i32 1)
    ret void
}

define i8* @concat_(i8* %s1, i8* %s2) {
    %1 = call i32 @strlen(i8* %s1)
    %2 = call i32 @strlen(i8* %s2)
    %3 = add i32 %1, 1
    %4 = add i32 %3, %2
    %5 = call i8* @malloc(i32 %4)
    %6 = call i8* @strcpy(i8* %5, i8* %s1)
    %7 = call i8* @strcat(i8* %6, i8* %s2)
    ret i8* %7
}
