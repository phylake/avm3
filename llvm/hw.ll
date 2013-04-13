;opt -std-compile-opts -print-before-all hw.ll -o hw.o && time ./hw.o


; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"
@.percentD = private unnamed_addr constant [3 x i8] c"%d\00"
@.mil = constant i32 1000000
@.zero = constant i32 0

; External declaration of the puts function

; int puts ( const char * str );
declare i32 @puts(i8* nocapture) nounwind

; int printf ( const char * format, ... );
;declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @printf(i8* noalias nocapture, i32)

; Definition of main function
define i32 @main() {   ; i32()*
entry:
  %0 = alloca i32
  %1 = alloca i32
  %2 = alloca i32
  store i32 1000000, i32* %2
  %3 = alloca i32
  store i32 0, i32* %3
  br label %L1
L1:
  %b1 = load i32* %2
  %b2 = load i32* %3
  %cond = icmp ugt i32 %b1, %b2
  br i1 %cond, label %L2, label %L3
L2:
  %T21 = load i32* %2       ; load the stack value %X from the stack.
  %T22 = sub i32 %T21, 1    ; decrement it
  store i32 %T22, i32* %2
  
  %T31 = load i32* %3       ; load the stack value %X from the stack.
  %T32 = add i32 %T31, 1    ; decrement it
  store i32 %T32, i32* %3
  br label %L1
L3:
  ;// local_count=4 max_scope=1 max_stack=2 framesize=7 code_len=27 code_offset=412
    ;0       getlocal0       
    ;1       pushscope       
    ;2       pushbyte        4
    ;4       setlocal2       
    ;5       pushbyte        0
    ;7       setlocal3       
    ;8       jump            L1
    ;
    ;L2: 
    ;12      label           
    ;13      getlocal2       
    ;14      decrement_i     
    ;15      setlocal2       
    ;16      inclocal_i      3
    ;
    ;L1: 
    ;18      getlocal2       
    ;19      getlocal3       
    ;20      ifgt            L2
    ;
    ;24      pushstring      "hi"
    ;26      returnvalue     

  ; Convert [13 x i8]* to i8  *...
  ;%cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  ;call i32 @puts(i8* %cast210)

  %fin = load i32* %2
  %d = getelementptr [3 x i8]* @.percentD, i64 0, i64 0
  call i32 @printf(i8* %d, i32 %fin)

  ret i32 0
}

define i32 @test () {
  ;0       getlocal0       
  ;1       pushscope       
  ;2       pushdouble      1.209178234
  ;4       convert_d       
  ;5       setlocal1       
  ;6       getlex          BAR
  ;8       pushint         123456789 // 0x75bcd15
  ;10      add             
  ;11      convert_i       
  ;12      setlocal2       
  ;13      pushstring      "a"
  ;15      getlex          int
  ;17      getproperty     MAX_VALUE
  ;19      pushstring      "b"
  ;21      getlocal2       
  ;22      newobject       {2}
  ;24      returnvalue     

}

; Named metadata
!1 = metadata !{i32 42}
;!foo = metadata !{!1, null}
