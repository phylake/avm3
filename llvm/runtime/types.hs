module LLVM.Runtime.Types where

import           LLVM.Lang
{-
all i really need out of a class is
  1. STATIC
    - a map of multiname to function pointer
    - inheritance
  2. DYNAMIC
    - a bag for dynamic properties a la: public dynamic class {}
-}

klass :: D
klass = Struct [
    P Void,    -- pointer to parent
    I32,       -- following array lengths
    P I32,     -- multinames
    P (P Void) -- array of pointer to function
  ]

int :: D
int = Struct [P klass, I32]
