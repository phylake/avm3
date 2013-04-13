; [i32], length, key
define i32 @bsearch(i32 *, i32, i32) {
entry:
  %min = alloca i32
  store i32 0, i32 * %min
  %max = alloca i32
  ; N - 1
  %T1 = add i32 -1, %1
  store i32 %T1, i32 * %max

  br label %LOOP
LOOP:
  %min_idx = load i32 * %min
  %max_idx = load i32 * %max
  %while = icmp uge i32 %max_idx, %min_idx
  br i1 %while, label %LOOP_P2, label %NOT_FOUND
LOOP_P2:
  ; max - min
  %T2 = sub i32 %max_idx, %min_idx
  ; (max - min) / 2
  %T3 = udiv i32 %T2, 2
  ; midpoint = min + ((max - min) / 2)
  %mid_idx = add i32 %min_idx, %T3
  ; %0 + midpoint
  %mid_adr = getelementptr i32 * %0, i32 %mid_idx
  %mid_val = load i32 * %mid_adr
  br label %CHK_GT
CHK_GT:
  %gt = icmp ugt i32 %mid_val, %2
  br i1 %gt, label %GT, label %CHK_LT
GT:
  %mid_idx_gt1 = add i32 -1, %mid_idx
  store i32 %mid_idx_gt1, i32 * %max
  br label %LOOP
CHK_LT:
  %lt = icmp ult i32 %mid_val, %2
  br i1 %lt, label %LT, label %CHK_EQ
LT:
  %mid_idx_lt1 = add i32 1, %mid_idx
  store i32 %mid_idx_lt1, i32 * %min
  br label %LOOP
CHK_EQ:
  %eq = icmp ueq i32 %mid_val, %2
  br i1 %eq, label %EQ, label %LOOP
EQ:
  ret i32 %mid_idx
NOT_FOUND:
  ret i32 -1
}
