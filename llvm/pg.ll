;llvm-link hc.ll pg.ll | opt -o pg.o && ./pg.o

; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"
@.percentD = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@.test = constant {i32, [2 x i32]} {i32 42, [2 x i32] [i32 43, i32 84]}

;@.class_as3_int = private unnamed_addr constant {i8*, i32, i32*, i8**}
;@.class_as3_int = global {i8*, i32, i32*, i8**}

%int = type { %int*, { {i8*, i32, i32*, i8**} *, i32 } * }

define
{i32, i8 * (i8 *, ...) *} *
@baseClass () {
entry:
  %class = alloca {i32, i8 * (i8 *, ...) *}
  ret {i32, i8 * (i8 *, ...) *} * %class
}

; int puts ( const char * str );
declare i32 @puts(i8* nocapture) nounwind
declare i32 @bsearch(i32 *, i32, i32)
declare i8* @getMethodStatic(i8*, i32)

; int printf ( const char * format, ... );
;declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @printf(i8* noalias nocapture, i32)

define void @printInt(i32) {
  %d = getelementptr [4 x i8]* @.percentD, i64 0, i64 0
  call i32 @printf(i8* %d, i32 %0)
  ret void
}

define void @helloworld() {
  ; Convert [13 x i8]* to i8  *...
  %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  call i32 @puts(i8* %cast210)
  ret void
}

define void @class_as3_int_ctor() {
  ;%T1 = getelementptr
  ret void
}

define { {i8*, i32, i32*, i8**} *, i32 } * @new_int (i32) {
  ; do this on the stack since all ints are pass-by-value
  %T1 = alloca { {i8*, i32, i32*, i8**} *, i32 }
  ; hook up global int class
  ; set value
  ret { {i8*, i32, i32*, i8**} *, i32 } * %T1
}

; http://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
; http://llvm.org/docs/doxygen/html/Constants_8cpp_source.html#l01742
define i64 @sizeof (i8*) {
entry:
  ;%Size = getelementptr i8* %0, i32 1
  ;%SizeI = bitcast %0 %Size to i64
  ;%T1 = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * null, i32 1
  ;%T2 = bitcast { {i8*, i32, i32*, i8**} *, i32 } * %T1 to i64
  ;ret i64 %SizeI
  ret i64 0
}

define i32 @valueOf({ {i8*, i32, i32*, i8**} *, i32 } *)
{
  %valuePtr = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * %0, i32 0, i32 1
  %value = load i32* %valuePtr
  ret i32 %value
}

define i32 @main() {
  ;call void @boxedIntTest()
  ret i32 0
}

define void @boxedIntTest() {
  %class = alloca {i8*, i32, i32*, i8**}

  
  ; map length 1
  %mapLenPtr = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 1
  store i32 2, i32* %mapLenPtr
  
  
  ; multinames [99]
  %multinames = alloca [2 x i32]
  %mn0 = getelementptr [2 x i32]* %multinames, i32 0, i32 0
  %mn1 = getelementptr [2 x i32]* %multinames, i32 0, i32 1
  store i32 99, i32* %mn0
  store i32 101, i32* %mn1
  %class.multinames = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 2
  %multinamesCast = bitcast [2 x i32]* %multinames to i32*
  store i32* %multinamesCast, i32** %class.multinames
  
  
  ; functions [&@valueOf]
  %functions = alloca [2 x i8*]
  %fn0 = getelementptr [2 x i8*]* %functions, i32 0, i32 0
  %fn1 = getelementptr [2 x i8*]* %functions, i32 0, i32 1
  
  %class.functions = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 3
  %functionsCast = bitcast [2 x i8*]* %functions to i8**
  store i8** %functionsCast, i8*** %class.functions
  
  %valueOfPtr = getelementptr i32 ({ {i8*, i32, i32*, i8**} *, i32 } *) * @valueOf
  %valueOfPtrCast = bitcast i32 ({ {i8*, i32, i32*, i8**} *, i32 } *) * %valueOfPtr to i8*
  store i8* %valueOfPtrCast, i8** %fn0
  store i8* %valueOfPtrCast, i8** %fn1
  
  %boxedInt = alloca { {i8*, i32, i32*, i8**} *, i32 }
  %boxedInt.class = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt, i32 0, i32 0
  %boxedInt.value = getelementptr { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt, i32 0, i32 1
  store {i8*, i32, i32*, i8**} * %class, {i8*, i32, i32*, i8**} ** %boxedInt.class
  store i32 44, i32* %boxedInt.value

  
  ;%value = call i32 @valueOf({ {i8*, i32, i32*, i8**} *, i32 } * %boxedInt)
  ;call void @printInt(i32 %value)

  %generic = bitcast { {i8*, i32, i32*, i8**} *, i32 } * %boxedInt to i8*
  %function = call i8* @getMethodStatic(i8* %generic, i32 99)
  ;%valueOf = bitcast i8* %function to i32 (...) *
  %valueOf = bitcast i8* %function to i32 ({ {i8*, i32, i32*, i8**} *, i32 } *) *
  %value = call i32 %valueOf({ {i8*, i32, i32*, i8**} *, i32 } * %boxedInt)
  call void @printInt(i32 %value)

  ret void
}
