;opt -std-compile-opts -print-before-all bench.ll -o bench.o && time ./bench.o


; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"
;@.percentD = private unnamed_addr constant [3 x i8] c"%d\00"
@.percentD = private unnamed_addr constant [4 x i8] c"%d\0A\00"

; External declaration of the puts function

; int puts ( const char * str );
declare i32 @puts(i8* nocapture) nounwind

; int printf ( const char * format, ... );
declare i32 @printf(i8* noalias nocapture, i32)
declare i8* @getMethodStatic(i8*, i32)
declare i8* @malloc(i32)

;// local_count=4 max_scope=1 max_stack=2 framesize=7 code_len=27 code_offset=412
;0       getlocal0       %reg_0 = alloca {  }
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
define i32 @main() {
entry:
  call void @boxed()
  ret i32 0
}

define i1 @sgt(i8*, i8*)
{
  %cast0 = bitcast i8* %0 to { i8*, i32 } *
  %cast1 = bitcast i8* %1 to { i8*, i32 } *
  %ptr0 = getelementptr { i8*, i32 } * %cast0, i32 0, i32 1
  %ptr1 = getelementptr { i8*, i32 } * %cast1, i32 0, i32 1
  %val0 = load i32* %ptr0
  %val1 = load i32* %ptr1
  %cond = icmp sgt i32 %val0, %val1
  ret i1 %cond
}

define i8* @subtract(i8*, i8*)
{
  %cast0 = bitcast i8* %0 to { i8*, i32 } *
  %cast1 = bitcast i8* %1 to { i8*, i32 } *
  %ptr0 = getelementptr { i8*, i32 } * %cast0, i32 0, i32 1
  %ptr1 = getelementptr { i8*, i32 } * %cast1, i32 0, i32 1
  %val0 = load i32* %ptr0
  %val1 = load i32* %ptr1
  %val  = sub i32 %val0, %val1
  %retP = call i8* @newInt(i32 %val)
  ret i8* %retP
}

define i8* @add(i8*, i8*)
{
  %cast0 = bitcast i8* %0 to { i8*, i32 } *
  %cast1 = bitcast i8* %1 to { i8*, i32 } *
  %ptr0 = getelementptr { i8*, i32 } * %cast0, i32 0, i32 1
  %ptr1 = getelementptr { i8*, i32 } * %cast1, i32 0, i32 1
  %val0 = load i32* %ptr0
  %val1 = load i32* %ptr1
  %val  = sub i32 %val0, %val1
  %retP = call i8* @newInt(i32 %val)
  ret i8* %retP
}

;define { {i8*, i32, i32*, i8**} *, i32 } * @newInt(i32) {
define i8* @newInt(i32) {
  %class = alloca {i8*, i32, i32*, i8**}
  ;%class_ = call i8* @malloc(i32 16)
  ;%class = bitcast i8* %class_ to {i8*, i32, i32*, i8**}*

  
  ; map length 3
  ; =========================
  %mapLenPtr = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 1
  store i32 3, i32* %mapLenPtr
  
  
  ; multinames [101, 102, 200]
  ; =========================
  %multinames = alloca [3 x i32]
  ;%multinames_ = call i8* @malloc(i32 12)
  ;%multinames = bitcast i8* %multinames_ to [3 x i32]*
  %mn0 = getelementptr [3 x i32]* %multinames, i32 0, i32 0
  %mn1 = getelementptr [3 x i32]* %multinames, i32 0, i32 1
  %mn2 = getelementptr [3 x i32]* %multinames, i32 0, i32 2
  store i32 101, i32* %mn0
  store i32 102, i32* %mn1
  store i32 200, i32* %mn2
  %class.multinames = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 2
  %multinamesCast = bitcast [3 x i32]* %multinames to i32*
  store i32* %multinamesCast, i32** %class.multinames
  
  
  ; functions [&@valueOf]
  ; =========================
  %functions = alloca [3 x i8*]
  ;%functions_ = call i8* @malloc(i32 12)
  ;%functions = bitcast i8* %functions_ to [3 x i8*]*
  %fn0 = getelementptr [3 x i8*]* %functions, i32 0, i32 0
  %fn1 = getelementptr [3 x i8*]* %functions, i32 0, i32 1
  %fn2 = getelementptr [3 x i8*]* %functions, i32 0, i32 2
  
  %class.functions = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 3
  %functionsCast = bitcast [3 x i8*]* %functions to i8**
  store i8** %functionsCast, i8*** %class.functions
  
  %addPtr = getelementptr i8* (i8*, i8*) * @add
  %addPtrCast = bitcast i8* (i8*, i8*) * %addPtr to i8*
  store i8* %addPtrCast, i8** %fn0
  
  %subtractPtr = getelementptr i8* (i8*, i8*) * @subtract
  %subtractPtrCast = bitcast i8* (i8*, i8*) * %subtractPtr to i8*
  store i8* %subtractPtrCast, i8** %fn1
  
  %sgtPtr = getelementptr i1 (i8*, i8*) * @sgt
  %sgtPtrCast = bitcast i1 (i8*, i8*) * %sgtPtr to i8*
  store i8* %sgtPtrCast, i8** %fn2
  
  
  ; store class and value
  ; =========================
  %boxedInt = alloca { {i8*, i32, i32*, i8**} *, i32 }
  ;%boxedInt_ = call i8* @malloc(i32 8)
  ;%boxedInt = bitcast i8* %boxedInt_ to { {i8*, i32, i32*, i8**} *, i32 }*
  %boxedInt.class = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt, i32 0, i32 0
  %boxedInt.value = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt, i32 0, i32 1
  store {i8*, i32, i32*, i8**} * %class, {i8*, i32, i32*, i8**} ** %boxedInt.class
  store i32 %0, i32* %boxedInt.value
  ;call void @printInt(i32 %0)
  

  ; cast
  ; =========================
  %boxedIntCast = bitcast { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt to i8*

  ; testing recast
  ; =========================
  ;%int.cast = bitcast i8* %boxedIntCast to { i8*, i32 } *
  ;%int.val.ptr = getelementptr { i8*, i32 } * %int.cast, i32 0, i32 1
  ;%int.val = load i32* %int.val.ptr
  ;call void @printInt(i32 %int.val)

  ret i8* %boxedIntCast
}

define void @boxed() {
entry:
  ; calling twice is what messed things up probably due to alignment
  ; TODO malloc and hopefully it goes away
  %int1 = call i8* @newInt(i32 1000000)
  %int2 = call i8* @newInt(i32 1)

  %int1.cast = bitcast i8* %int1 to { i8*, i32 } *
  %int1.val.ptr = getelementptr { i8*, i32 } * %int1.cast, i32 0, i32 1
  %int1.val = load i32* %int1.val.ptr
  call void @printInt(i32 %int1.val)

  %int2.cast = bitcast i8* %int2 to { i8*, i32 } *
  %int2.val.ptr = getelementptr { i8*, i32 } * %int2.cast, i32 0, i32 1
  %int2.val = load i32* %int2.val.ptr
  call void @printInt(i32 %int2.val)

  br label %IF
IF:
  call void @printInt(i32 4)
  %int1SGTRaw = call i8* @getMethodStatic(i8* %int1, i32 200)
  call void @printInt(i32 5)
  %int1SGT = bitcast i8* %int1SGTRaw to i1 (i8*, i8*) *
  call void @printInt(i32 6)
  %cond = call i1 %int1SGT(i8* %int1, i8* %int2)
  br i1 %cond, label %IF_BODY, label %L3
IF_BODY:
  %int1SubRaw = call i8* @getMethodStatic(i8* %int1, i32 102)
  %int1Sub = bitcast i8* %int1SubRaw to void (i8*, i8*) *
  %int1T = call i8* @newInt(i32 1)
  call void %int1Sub(i8* %int1, i8* %int1T)

  %int2AddRaw = call i8* @getMethodStatic(i8* %int2, i32 101)
  %int2Add = bitcast i8* %int2AddRaw to void (i8*, i8*) *
  %int2T = call i8* @newInt(i32 1)
  call void %int2Add(i8* %int2, i8* %int2T)
  
  br label %IF
L3:

  ; Convert [13 x i8]* to i8  *...
  ;%cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  ;call i32 @puts(i8* %cast210)

  ;%fin = load i32* %2
  ;%d = getelementptr [3 x i8]* @.percentD, i64 0, i64 0
  ;call i32 @printf(i8* %d, i32 %fin)

  ret void
}

define void @printInt(i32) {
  %d = getelementptr [4 x i8]* @.percentD, i64 0, i64 0
  call i32 @printf(i8* %d, i32 %0)
  ret void
}

define void @unboxed() {
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
  %T21 = load i32* %2
  %T22 = sub i32 %T21, 1
  store i32 %T22, i32* %2
  
  %T31 = load i32* %3
  %T32 = add i32 %T31, 1
  store i32 %T32, i32* %3
  br label %L1
L3:

  ; Convert [13 x i8]* to i8  *...
  ;%cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  ;call i32 @puts(i8* %cast210)

  ;%fin = load i32* %2
  ;%d = getelementptr [3 x i8]* @.percentD, i64 0, i64 0
  ;call i32 @printf(i8* %d, i32 %fin)

  ret void
}
