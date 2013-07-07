; [i32], length, key
define i32 @bsearch(i32*, i32, i32) {
entry:
  %min = alloca i32
  store i32 0, i32* %min
  %max = alloca i32
  ; N - 1
  %T1 = add i32 -1, %1
  store i32 %T1, i32* %max

  br label %WHILE
WHILE:
  %min_idx = load i32* %min
  %max_idx = load i32* %max
  %while = icmp uge i32 %max_idx, %min_idx
  br i1 %while, label %WHILE_BODY, label %NOT_FOUND
WHILE_BODY:
  ; max - min
  %T2 = sub i32 %max_idx, %min_idx
  ; (max - min) / 2
  %T3 = udiv i32 %T2, 2
  ; midpoint = min + ((max - min) / 2)
  %mid_idx = add i32 %min_idx, %T3
  ; %0 + midpoint
  %mid_adr = getelementptr i32* %0, i32 %mid_idx
  %mid_val = load i32* %mid_adr
  br label %CHK_GT
CHK_GT:
  %gt = icmp ugt i32 %mid_val, %2
  br i1 %gt, label %GT, label %CHK_LT
GT:
  %mid_idx_gt1 = add i32 -1, %mid_idx
  store i32 %mid_idx_gt1, i32* %max
  br label %WHILE
CHK_LT:
  %lt = icmp ult i32 %mid_val, %2
  br i1 %lt, label %LT, label %CHK_EQ
LT:
  %mid_idx_lt1 = add i32 1, %mid_idx
  store i32 %mid_idx_lt1, i32* %min
  br label %WHILE
CHK_EQ:
  %eq = icmp ueq i32 %mid_val, %2
  br i1 %eq, label %EQ, label %WHILE
EQ:
  ret i32 %mid_idx
NOT_FOUND:
  ret i32 -1
}

; i8* instance like this
; {
;   {
;     i8*   pointer to parent
;   , i32   following array lengths
;   , i32*  multinames
;   , i8**  array of pointer to function
;   } *
; } *
;
; i32 the multiname index
; returns a pointer to function or null
define i8* @getMethodStatic(i8*, i32) {
  %instance = bitcast i8* %0 to { {i8*, i32, i32*, i8**} * } *
  
  %instance.class = getelementptr { {i8*, i32, i32*, i8**} * } * %instance, i32 0, i32 0
  %class = load {i8*, i32, i32*, i8**} ** %instance.class

  %class.mapLen = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 1
  %mapLen = load i32* %class.mapLen
  
  %class.multinames = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 2
  %multinames = load i32** %class.multinames
  
  %class.functions = getelementptr {i8*, i32, i32*, i8**} * %class, i32 0, i32 3
  %functions = load i8*** %class.functions
  %functionsCast = bitcast i8** %functions to [0 x i8*]*

  %functionIdx = call i32 @bsearch(i32* %multinames, i32 %mapLen, i32 %1)

  %functionPtr = getelementptr [0 x i8*]* %functionsCast, i32 0, i32 %functionIdx
  %function = load i8** %functionPtr
  
  ret i8* %function
}
