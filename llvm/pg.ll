; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"
@.percentD = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@.i0 = constant i32 1000000
@.i1 = constant i32 0
@.ui0 = constant i32 0
@.test = constant {i32, [2 x i32]} {i32 42, [2 x i32] [i32 43, i32 84]}

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

; int printf ( const char * format, ... );
;declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @printf(i8* noalias nocapture, i32)

define void @printInt(i32) {
  %d = getelementptr [4 x i8]* @.percentD, i64 0, i64 0
  call i32 @printf(i8* %d, i32 %0)
  ret void
}

@.bsearchArray = constant [4 x i32] [i32 2, i32 13, i32 104, i32 105]
define i32 @main() {
  ;%class = call {i32, i8 * (i8 *, ...) *} * @baseClass()
  %ar = getelementptr [4 x i32] * @.bsearchArray, i32 0, i32 0
  %idx = call i32 @bsearch(i32 * %ar, i32 4, i32 105)
  call void @printInt(i32 %idx)
  ;%idx_addr = getelementptr [3 x i32] * @.bsearchArray, i32 0, i32 %idx
  ;%val = load i32 * %idx_addr
  ret i32 0
  
  %class = alloca {
    i32                ; size
  , void (i8 *, ...) * ; void ctor (void * self, ...)
  , void (i8 *) *      ; void dtor (void * self)
  , i8 * (i8 *) *      ; void * clone (void * self)
  }
  %string_class = alloca {
    {
      i32                ; size
    , void (i8 *, ...) * ; void ctor (void * self, ...)
    , void (i8 *) *      ; void dtor (void * self)
    , i8 * (i8 *) *      ; void * clone (void * self)
    } *
  , i8 *
  }

  %test_class = alloca {i32, void (i32) *}
  
  ;%fst = getelementptr {i32, [2 x i32]} * @.test, i32 0, i32 1, i32 1
  
  %func = getelementptr {i32, void (i32) *} * %test_class, i32 0, i32 1
  store void (i32) * @printInt, void (i32) ** %func
  %printInt = load void (i32) ** %func
  call void %printInt(i32 123)

  ;%printInt_ptr = getelementptr void () * @printInt, i32 0
  ;store void () * @printInt, void () * %func2
  
  ret i32 0
}

define i32 @global.Test.main() {
entry:
  %reg_0 = alloca i32
  %reg_1 = alloca i32
  %reg_2 = alloca i32
  %reg_3 = alloca i32
  store i32 1000000, i32* %reg_2
  store i32 0, i32* %reg_3
  br label %L1
L2:
  %T21 = load i32* %reg_2
  %T22 = sub i32 %T21, 1
  store i32 %T22, i32* %reg_2

  %T31 = load i32* %reg_3
  %T32 = add i32 %T31, 1
  store i32 %T32, i32* %reg_3
  br label %L1
L1:
  %b1 = load i32* %reg_2
  %b2 = load i32* %reg_3
  %cond = icmp ugt i32 %b1, %b2
  br i1 %cond, label %L2, label %L3
L3:
  %fin = load i32* %reg_2
  %d = getelementptr [4 x i8]* @.percentD, i64 0, i64 0
  call i32 @printf(i8* %d, i32 %fin)

  ret i32 0
}

define {void () *} * @setupTestClass () {
  %class = alloca {
    i32                ; size
  , void (i8 *, ...) * ; void ctor (void * self, ...)
  , void (i8 *) *      ; void dtor (void * self)
  , i8 * (i8 *) *      ; void * clone (void * self)
  }
  %class.Test = alloca {
    i32 *
  , void (i8 *, ...) *
  , void (i8 *) *
  , i8 * (i8 *) *
  }

  ; map these to index 0,1,2
  ;{
  ;  2,
  ;  13,
  ;  104
  ;}


  %struct = alloca {void () *}
  ret {void () *} * %struct
; public function parallelize():void
; public function test1():Object
; public function Test()
; public function baz():Boolean { return false; }*/

}

define void @helloworld() {
  ; Convert [13 x i8]* to i8  *...
  %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  call i32 @puts(i8* %cast210)
  ret void
}
