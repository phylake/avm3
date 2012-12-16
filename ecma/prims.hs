module Ecma.Prims where

--import ECMA.Types

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

class Get a where
  get :: a -> a

class Set a where
  set :: a -> a

class (Enumerable a, Configurable a) => CommonProperties a where

class (CommonProperties a, Writable a) => NamedDataProperties a where

class (CommonProperties a, Get a, Set a) => NamedDataAccessorProperties a where

-- 8.10.2

isDataDescriptor :: (Coerce a, Value a, Writable a, Queryable a) => a -> Bool
isDataDescriptor desc = not (isUndefined desc) && (toBoolean (value desc) || writable desc)

-- 8.12.1

getOwnProperty = undefined

-- 9

class Coerce a where
  -- 9.1
  toPrimitive :: a -> Maybe a -> a
  -- 9.2
  toBoolean :: a -> Bool
  -- 9.3
  toNumber :: a -> Integer


{-
not sure what to do with these so far

it seems that types need to be inspectable in a number of
ways in addition to inspecting what's in them

-}
class (Coerce a) => Typeable a where

-- basically need to cast to the type and return if it succeeds
class (Coerce a) => Queryable a where
  isUndefined :: a -> Bool
  isNull :: a -> Bool

data ECMA_Property = NamedData ECMA_Type Bool Bool Bool {- 8.6.1 [[Value]] [[Writable]] [[Enumerable]] [[Configurable]] -}
                   | NamedAccessor ECMA_Type ECMA_Type Bool Bool {- 8.6.1 [[Get]] [[Set]] [[Enumerable]] [[Configurable]] -}
                   | Internal ECMA_Type String Bool {- 8.6.2 [[Prototype]] [[Class]] [[Extensible]] -}

-- TODO table 9 internal properties only existing on some objects

instance Enumerable ECMA_Property where
  enumerable (NamedData _ _ a _) = a
  enumerable (NamedAccessor _ _ a _) = a
  enumerable (Internal _ _ _) = False

instance Writable ECMA_Property where
  writable (NamedData _ a _ _) = a
  writable (NamedAccessor _ _ _ _) = False
  writable (Internal _ _ _) = False

instance Configurable ECMA_Property where
  configurable (NamedData _ _ _ a) = a
  configurable (NamedAccessor _ _ a _) = a
  configurable (Internal _ _ _) = False

instance Get ECMA_Property where
  get (NamedData _ _ _ _) = undefined
  get (NamedAccessor _ _ _ _) = undefined
  get (Internal _ _ _) = undefined

instance Set ECMA_Property where
  set (NamedData _ _ _ _) = undefined
  set (NamedAccessor _ _ _ _) = undefined
  set (Internal _ _ _) = undefined

data ECMA_Type = ECMA_Undefined
               | ECMA_Null
               | ECMA_Boolean Bool
               | ECMA_String String
               | ECMA_Number Integer
               | ECMA_Object [ECMA_Property] (Maybe ECMA_Property)
               --deriving (Show, Eq)

{-instance Num ECMA_Type where
  (ECMA_Int a) + (ECMA_Int b) = ECMA_Int (a+b)
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined-}

{---class (Num a) => ToInt32 a where
class ToInt32 a where
  toInt32 :: a -> Int
  toInt32 = undefined-}
