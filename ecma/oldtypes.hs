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
               | ECMA_Number Double
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
