module Ecma.Prims where

import           Data.Int
import           Data.Word
import           Data.Binary.IEEE754 (doubleToWord)

nan :: Double
nan = read "NaN"

infinity :: Double
infinity = read "Infinity"

-- 8.6.1
class Value a where
  --value :: (Typeable b) => a -> b
  value :: a -> a

class Writable a where
  writable :: a -> Bool

class Enumerable a where
  enumerable :: a -> Bool

class Configurable a where
  configurable :: a -> Bool

{-class Get a where
  get :: a -> a

class Set a where
  set :: a -> a

class (Enumerable a, Configurable a) => CommonProperties a where

class (CommonProperties a, Writable a) => NamedDataProperties a where

class (CommonProperties a, Get a, Set a) => NamedDataAccessorProperties a where-}

-- 8.10.2

isDataDescriptor :: (Coerce a, Value a, Writable a, Queryable a) => a -> Bool
isDataDescriptor desc = not (is_undefined desc) && (to_boolean (value desc) || writable desc)


-- 8.12.1

getOwnProperty = undefined

-- 9

class Coerce a where
  -- 9.1
  to_primitive :: a -> Maybe a -> a
  to_primitive = undefined
  -- 9.2
  to_boolean :: a -> Bool
  to_boolean a = False
  -- 9.3
  to_number :: a -> Double
  to_number a = 0
  -- 9.4
  to_integer :: a -> Integer
  to_integer a
    | num == nan = 0
    | num == infinity = to_integer' num
    | otherwise = to_integer' num
    where
      num = to_number a
      to_integer' = fromIntegral. doubleToWord
  -- 9.5
  to_int32 :: a -> Int32
  to_int32 = fromIntegral. to_integer
  -- 9.6
  to_uint32 :: a -> Word32
  to_uint32 = fromIntegral. to_integer
  -- 9.8
  to_string :: a -> String
  to_string a = ""


{-
not sure what to do with these so far

it seems that types need to be inspectable in a number of
ways in addition to inspecting what's in them

-}
class (Coerce a) => Typeable a where

-- basically need to cast to the type and return if it succeeds
class (Coerce a) => Queryable a where
  is_undefined :: a -> Bool
  is_null :: a -> Bool
