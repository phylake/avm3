module Vm.Ecma where

import           Ecma.Prims
import           Data.Word
import           Data.Int
import           Vm.Def
import qualified Data.HashTable.IO as H
import qualified MonadLib as ML

{-
data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Boolean Bool
          | VmRt_Int Int32
          | VmRt_Uint Word32
          | VmRt_Number Double
          | VmRt_String String
          | VmRt_Object VmObject
          deriving (Show)-}

-- 9
instance Coerce VmRt where
  to_boolean VmRt_Undefined = False
  to_boolean VmRt_Null = False
  to_boolean (VmRt_Boolean a) = a
  to_boolean (VmRt_Int a) = a /= 0
  to_boolean (VmRt_Uint a) = a /= 0
  to_boolean (VmRt_Number a)
    | a == 0 = False
    | isNaN a = False
    | otherwise = True
  to_boolean (VmRt_String a) = length a > 0
  to_boolean (VmRt_Object _) = True

  to_number VmRt_Undefined = nan
  to_number VmRt_Null = 0
  to_number (VmRt_Boolean True) = 1
  to_number (VmRt_Boolean False) = 0
  to_number (VmRt_Int a) = fromIntegral a
  to_number (VmRt_Uint a) = fromIntegral a
  to_number (VmRt_Number a) = a
  to_number (VmRt_String a) = read a
  to_number (VmRt_Object _) = undefined

  to_string VmRt_Undefined = "undefined"
  to_string VmRt_Null = "null"
  to_string (VmRt_Boolean True) = "true"
  to_string (VmRt_Boolean False) = "false"
  to_string (VmRt_Int a) = show a
  to_string (VmRt_Uint a) = show a
  to_string (VmRt_Number a) = show a
  to_string (VmRt_String a) = a
  to_string (VmRt_Object _) = "[Object]"
