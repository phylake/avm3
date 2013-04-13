-- ghc -O2 -threaded executeVm && time ./executeVm -N1
-- PATH=$PATH:~/Downloads/llvm_clang_macosx/bin ghc -O2 -threaded -fllvm executeVm && time ./executeVm -N1
-- ghc -O2 -prof -fprof-auto -rtsopts executeVm && ./executeVm -N1 +RTS -p
module Main where
import Vm.Execute
--main = test_file
main = foo

{-
  2699ms (looping 10^6)
    with name resolution 
    ghc -O2 -threaded executeVm && time ./executeVm -N1
    AVM2 160ms
    Ruby 216ms
  458ms (looping 10^6)
    no name resolution. only using local registers
    ghc -O2 -threaded -fllvm executeVm && time ./executeVm -N1
    Ruby 162ms
-}
