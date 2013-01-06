{-
don't forget -Wall

http://www.haskell.org/ghc/docs/7.0.4/html/users_guide/flag-reference.html

ghc -prof -auto-all -rtsopts=all -fforce-recomp amf.hs
./amf +RTS -p -K100M -RTS
-}

{-
Enumeratee == |
Iteratee == Consumer
Enumerator == Producer
Enumerator | Iteratee
-}

module Amf.Deserialize where

import           Amf.Def
import           Control.DeepSeq
import           Control.Monad.State
import           Data.Binary.IEEE754 (wordToDouble)
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.Char (digitToInt, intToDigit)
import           Data.Int
import           Data.Word
import           Util.Misc
import           Util.Words
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

{-

BEGIN test code

-}
testFile = do
    bs <- BS.readFile "amf/file.amf"
    case allFromAmf $ (,) (Acc (BS.unpack bs) 0 nilTable []) [] of
        Left err -> putStrLn err
        Right (acc, as) -> do
            putStrLn "AMFS"
            putStrLn $ unlines $ map show as
            putStrLn "TABLES"
            putStrLn "Strings"
            putStrLn $ unlines $ map show $ t31 $ accTs acc
            putStrLn "Complex Objects"
            putStrLn $ unlines $ map show $ t32 $ accTs acc
            putStrLn "Traits"
            putStrLn $ unlines $ map show $ t33 $ accTs acc

allFromAmf :: AmfT -> Either AmfErr AmfT
allFromAmf amft@(acc, amfs) = if (length $ accAs acc) > 0
    then fromAmfImpl amft >>= allFromAmf
    else Right amft
{-

END test code

-}

fromAmfImpl :: AmfT -> Either AmfErr AmfT
fromAmfImpl ((Acc [] bs ts log), as) = Right ((Acc [] bs ts log), as)
fromAmfImpl ((Acc (w:ws) bs ts log), as)
    | w == 0x0 = Right (acc, AmfUndefined : as)
    | w == 0x1 = Right (acc, AmfNull      : as)
    | w == 0x2 = Right (acc, AmfFalse     : as)
    | w == 0x3 = Right (acc, AmfTrue      : as)
    | w == 0x4 = fromIntegerType amft
    | w == 0x5 = fromDoubleType  amft
    | w == 0x6 = fromStringType  amft
    | w == 0x7 = fromXmlDocType  amft
    | w == 0x8 = Left "Date unsupported"
    | w == 0x9 = fromArrayType   amft
    | w == 0xA = fromObjectType  amft
    | w == 0xB = fromXmlType     amft
    | w == 0xC = fromByteArray   amft
    | otherwise = Left "encountered unknown marker"
    where
        acc = Acc ws (bs+1) ts log
        amft = (acc, as)

--toAmfImpl :: ([Amf], Tables) -> Either AmfErr [Word8]
--toAmfImpl (AmfUndefined:amfs)       = undefined
--toAmfImpl (AmfNull:amfs)            = undefined
--toAmfImpl (AmfFalse:amfs)           = undefined
--toAmfImpl (AmfTrue:amfs)            = undefined
--toAmfImpl ((AmfInt u29):amfs)       = undefined
--toAmfImpl ((AmfDouble double):amfs) = undefined
--toAmfImpl ((AmfString utf8vr):amfs) = undefined
--toAmfImpl ((AmfXmlDoc string):amfs) = undefined
--toAmfImpl ((AmfDate string):amfs)   = undefined
--toAmfImpl ((AmfArray u29a):amfs)    = undefined
--toAmfImpl ((AmfObject u29o):amfs)   = undefined
--toAmfImpl ((AmfXml string):amfs)    = undefined
--toAmfImpl ((AmfByteArray ba):amfs)  = undefined

{-
    AmfByteArray

    U29O-ref |
    U29B-value U8*
-}

fromByteArray :: AmfT -> Either AmfErr AmfT
fromByteArray = refOrValue fromU29BRef fromU29BValue

fromU29BRef :: Int -> AmfT -> Either AmfErr AmfT
fromU29BRef idx (acc, amfs) = Right $ (,) acc $ (AmfByteArray $ U29B_Ref idx) : amfs

fromU29BValue :: Int -> AmfT -> Either AmfErr AmfT
fromU29BValue len (acc, amfs) = Right $ (,) acc' $ (toByteArray acc) : amfs
    where
        acc' = acc
            `fAs` drop len
            `fBs` (+len)
        toByteArray = AmfByteArray . U29B_Value . take len . accAs

{-
    AmfXml and AmfXmlDoc

    U29O-ref |
    U29X-value UTF8-char*
-}

fromXmlType :: AmfT -> Either AmfErr AmfT
fromXmlType = fromCommonXml AmfXml

fromXmlDocType :: AmfT -> Either AmfErr AmfT
fromXmlDocType = fromCommonXml AmfXmlDoc

fromCommonXml :: (U29X -> Amf) -> AmfT -> Either AmfErr AmfT
fromCommonXml c (acc, amfs) = do
    (utf8Acc, utf8vr) <- fromAmf acc
    Right (utf8Acc, c utf8vr : amfs)

{-
    AmfObject

    (
        U29O-ref |
        U29O-traits-ref |
        U29O-traits-ext class-name U8* |
        U29O-traits class-name UTF-8-vr*
    )
    value-type* dynamic-member*
-}

fromObjectType :: AmfT -> Either AmfErr AmfT
fromObjectType (acc, amfs) = do
    (u29Acc, u29) <- fromAmf acc
    u29OFunc u29 (u29Acc, amfs)
    
u29OFunc :: Int -> AmfT -> Either AmfErr AmfT
u29OFunc u29
    -- U29O-ref
    | u29 .&. 0x1 == 0x0 = fromU29ORef u29
    -- U29O-traits-ref
    | u29 .&. 0x3 == 0x1 = fromU29OTraitsRef u29
    -- U29O-traits-ext
    | u29 .&. 0x7 == 0x7 = fromU29OTraitsExt u29
    -- U29O-traits
    | u29 .&. 0x7 == 0x3 = fromU29OTraits u29
    | otherwise = \(acc, _) -> Left $ "u29OFunc - unrecognized u29 " ++ (show $ accAs acc)

fromU29ORef :: Int -> AmfT -> Either AmfErr AmfT
fromU29ORef u29 (acc, amfs) = Right (acc, amfObject : amfs)
    where
        amfObject = AmfObject $ U29O_Ref $ u29 `shiftR` 1

fromU29OTraitsRef :: Int -> AmfT -> Either AmfErr AmfT
fromU29OTraitsRef u29 (acc, amfs) =
    let
        traits :: Traits
        traits = getTT (accTs acc) !! (u29 `shiftR` 2)
    in do
    (assocAcc, assocs) <- commonU29O traits acc
    Right $ (,) assocAcc $ (AmfObject $ U29O_TraitsRef assocs) : amfs

fromU29OTraitsExt :: Int -> AmfT -> Either AmfErr AmfT
fromU29OTraitsExt u29 acc = Left "fromU29OTraitsExt not implemented"

fromU29OTraits :: Int -> AmfT -> Either AmfErr AmfT
fromU29OTraits u29 (acc, amfs) = do
    (classAcc, utf8vr) <- fromUtf8 acc
    (propsAcc, props) <- forN fromUtf8Loop (classAcc, []) (u29 `shiftR` 4)
    let traits = (utf8vr, props, u29 .&. 0x8 == 0x8)
    (assocAcc, assocAmfs) <- commonU29O traits $ propsAcc `fTs` pushTT traits
    Right $ (,) assocAcc $ (AmfObject $ U29O_Traits $ assocAmfs) : amfs
        where
            -- need to force calling instance AmfPrim UTF_8_vr
            -- don't know a better way
            fromUtf8 :: Acc Word8 -> Either AmfErr (Acc Word8, UTF_8_vr)
            fromUtf8 = fromAmf

            fromUtf8Loop :: (Acc Word8, [UTF_8_vr]) -> Either AmfErr (Acc Word8, [UTF_8_vr])
            fromUtf8Loop (acc, utf8vrs) = do
                (acc', utf8vr) <- fromUtf8 acc
                str <- getString utf8vr
                if str == utf8_empty
                    then Left "fromUtf8Loop - empty string"
                    else Right (acc', utf8vr : utf8vrs)

commonU29O :: Traits
           -> Acc Word8
           -> Either AmfErr (Acc Word8, [Assoc_Value])
commonU29O (_, props, dynamic) acc = do
    strictNT <- strictProps acc props
    if dynamic
        then Right strictNT >>= fromAssoc
        else Right strictNT

strictProps :: Acc Word8
            -> [UTF_8_vr]
            -> Either AmfErr (Acc Word8, [Assoc_Value])
strictProps assocAcc props = do
    (valuesAcc, valueAmfs) <- forN fromAmfImpl (assocAcc, []) (length props)
    if length valueAmfs /= length props
        then Left $ "strictProps - length mismatch"
            ++ "\n\t props: " ++ show props
            ++ "\n\tvalues: " ++ show valueAmfs
            ++ "\n\t   log: " ++ unlines (accLog valuesAcc)
        else Right (valuesAcc, zip props valueAmfs)

{-
    AmfArray

        U29O-ref |
    (
        U29A-value
        (UTF-8-empty | assoc-value* UTF-8-empty)
        value-type*
    )
-}

fromArrayType :: AmfT -> Either AmfErr AmfT
fromArrayType = refOrValue fromU29ARef fromU29AValue

fromU29ARef :: Int -> AmfT -> Either AmfErr AmfT
fromU29ARef index (acc, amfs) = Right (acc, amfArray : amfs)
    where
        amfArray = AmfArray $ U29A_Ref index

fromU29AValue :: Int -> AmfT -> Either AmfErr AmfT
fromU29AValue len (acc, amfs) = do
    (assocAcc, assocAmfs) <- fromAssoc (acc, [])
    (denseAcc, denseAmfs) <- forN fromAmfImpl (assocAcc, []) len
    let
        amfArray = AmfArray $ U29A_Value assocAmfs denseAmfs
        acc' = denseAcc `fTs` pushCOT amfArray
    Right (acc', amfArray : amfs)

fromAssoc :: (Acc Word8, [Assoc_Value]) -> Either AmfErr (Acc Word8, [Assoc_Value])
fromAssoc (acc, kvs) = do
    (keyAcc, key) <- fromAmf acc
    str <- getString key
    if str == utf8_empty
        {- nothing to do except preserve state of keyAcc -}
        then Right (keyAcc, kvs)
        else do
            (valueAcc, values) <- fromAmfImpl (keyAcc, [])
            if length values > 1
                then Left "fromAssoc - values length > 1"
                else fromAssoc $ (,) valueAcc $ (key, head values) : kvs

{-
    Char
-}

instance AmfPrim UTF_8_vr where
    {-fromAmf = BSC.unpack . BS.pack
    toAmf = BS.unpack . BSC.pack-}
    fromAmf acc = do
        (u29Acc, u29) <- fromAmf acc
        let lenOrIdx = u29 `shiftR` 1
        if u29 .&. 1 == 0
            {- ref - lenOrIdx used as index -}
            then Right (u29Acc, U29S_Ref lenOrIdx)
            {- value - lenOrIdx used as length -}
            else fromU29SValue lenOrIdx u29Acc

fromU29SValue :: Int -> Acc Word8 -> Either AmfErr (Acc Word8, UTF_8_vr)
fromU29SValue len acc = Right (acc', U29S_Value str)
    where
        accToStr = BSC.unpack . BS.pack . take len . accAs
        str = accToStr acc
        acc' = acc
            `fAs` drop len
            `fBs` (+len)-- +1 for utf8_empty?
            `fTs` pushST str

fromStringType :: AmfT -> Either AmfErr AmfT
fromStringType = fromAmfableType AmfString

utf8_empty :: String
utf8_empty = ""

{-
    Double
-}

instance AmfPrim Double where
    fromAmf acc = Right (acc', accToDouble acc)
        where
            accToDouble = wordToDouble . toWord64 . accAs
            acc' = acc
                `fAs` drop 8
                `fBs` (+8)

fromDoubleType :: AmfT -> Either AmfErr AmfT
fromDoubleType = fromAmfableType AmfDouble

{-
    Int
-}

instance AmfPrim Int where
    fromAmf acc = Right (acc',accToInt acc)
        where
            bs = varIntLen $ accAs acc
            accToInt = fromIntegral . fromU29 . take bs . accAs
            acc' = acc
                `fAs` drop bs
                `fBs` (+bs)

fromIntegerType :: AmfT -> Either AmfErr AmfT
fromIntegerType = fromAmfableType AmfInt

{- the number of bytes an Int will occupy once serialized -}
--deserializedU29Length :: (Real a) => a -> Int
deserializedU29Length :: Int -> Int
deserializedU29Length x
    | x >= 0x00000000 && x <= 0x0000007f = 1
    | x >= 0x00000080 && x <= 0x00003fff = 2
    | x >= 0x00004000 && x <= 0x001fffff = 3
    | x >= 0x00200000 && x <= 0x3fffffff = 4
    | otherwise                          = 0

{-
    Tables
-}

getString :: UTF_8_vr -> Either AmfErr String
getString (U29S_Value v) = Right v
getString (U29S_Ref u29) = Left "getString - not handling refs"

{-
getString :: UTF_8_vr -> [Amf] -> String
getString (U29S_Value v) as = v
getString (U29S_Ref u29) as = stringFromTable as u29

stringFromTable :: [Amf] -> Int -> String
stringFromTable amfs idx = table !! idx'
    where
        table = stringTable amfs
        idx' = (length table) - idx - 1

stringTable :: [Amf] -> [String]
stringTable ((AmfStack as):bs) = (stringTable as) ++ (stringTable bs)
stringTable ((AmfString (U29S_Value a)):as) = a : stringTable as
stringTable (a:as) = stringTable as
stringTable [] = []

complexObjectTable :: [Amf] -> [Amf]
complexObjectTable ((AmfArray (U29A_Value a b)):as) = AmfArray (U29A_Value a b) : complexObjectTable as
complexObjectTable (a:as) = complexObjectTable as
complexObjectTable [] = []

traitsTable :: [Amf] -> [Traits]
traitsTable ((AmfObject (U29O_Traits cls props _ [])):as) = (cls, props, False) : traitsTable as
traitsTable ((AmfObject (U29O_Traits cls props _  _)):as) = (cls, props, True) : traitsTable as
traitsTable (a:as) = traitsTable as
traitsTable [] = []
-}
{-
    Util
-}

fAs :: Acc a -> ([a] -> [a]) -> Acc a
fAs acc f = rAs (f $ accAs acc) acc

fBs :: Acc a -> (Int -> Int) -> Acc a
fBs acc f = rBs (f $ accBs acc) acc

fTs :: Acc a -> (Tables -> Tables) -> Acc a
fTs acc f = rTs (f $ accTs acc) acc

fLog :: Acc a -> ([String] -> [String]) -> Acc a
fLog acc f = rLog (f $ accLog acc) acc

rAs :: [a] -> Acc a -> Acc a
rAs value acc = Acc
    value
    (accBs acc)
    (accTs acc)
    (accLog acc)

rBs :: Int -> Acc a -> Acc a
rBs value acc = Acc
    (accAs acc)
    value
    (accTs acc)
    (accLog acc)

rTs :: Tables -> Acc a -> Acc a
rTs value acc = Acc
    (accAs acc)
    (accBs acc)
    value
    (accLog acc)

rLog :: [String] -> Acc a -> Acc a
rLog value acc = Acc
    (accAs acc)
    (accBs acc)
    (accTs acc)
    value

pushST :: String -> Tables -> Tables
pushST a (st, cot, tt) = (a : st, cot, tt)

pushCOT :: Amf -> Tables -> Tables
pushCOT a (st, cot, tt) = (st, a : cot, tt)

pushTT :: Traits -> Tables -> Tables
pushTT a (st, cot, tt) = (st, cot, a : tt)

getST :: Tables -> [String]
getST = t31

getCOT :: Tables -> [Amf]
getCOT = t32

getTT :: Tables -> [Traits]
getTT = t33

nilTable :: Tables
nilTable = ([], [], [])

nilAcc :: Acc Word8
nilAcc = Acc [] 0 nilTable []

refOrValue :: (Int -> AmfT -> Either AmfErr AmfT) -- ref
           -> (Int -> AmfT -> Either AmfErr AmfT) -- value
           -> AmfT
           -> Either AmfErr AmfT
refOrValue ref value (acc, amfs) = do
    (u29Acc, u29) <- fromAmf acc
    let lenOrIdx = u29 `shiftR` 1
    if u29 .&. 1 == 0
        then ref   lenOrIdx (u29Acc, amfs)
        else value lenOrIdx (u29Acc, amfs)

fromAmfableType :: (AmfPrim a)
                => (a -> Amf)
                -> AmfT
                -> Either AmfErr AmfT
fromAmfableType f (acc, amfs) = do
    (acc', amfs') <- fromAmf acc
    Right (acc', f amfs' : amfs)
