{-# OPTIONS_GHC
    -XFlexibleInstances
    -XOverlappingInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances
    -XTemplateHaskell
    -cpp #-}
module Text.RJson (TranslateField,
                   TranslateFieldD,
                   translateField,
                   ToJson,
                   ToJsonD,
                   toJson,
                   exclude,
                   arrayPrepend,
                   arrayAppend,
                   objectExtras,
                   genericToJson,
                   enumToJson,
                   JsonData(..),
                   FromJson,
                   FromJsonD,
                   objectDefaults,
                   parseJsonString,
                   parseJsonByteString,
                   fromJson,
                   fromJsonString,
                   fromJsonByteString,
                   genericFromJson,
                   enumFromJson,
                   stripInitialUnderscores,
                   toJsonString,
                   firstCharToUpper,
                   firstCharToLower,
                   Union(..), Union3, Union4, Union5, Union6,
                   Union7,Union8,Union9,Union10,
                   cond)
where

import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances
import Data.Generics.SYB.WithClass.Context
import Data.Generics.SYB.WithClass.Derive
import qualified Data.Map as M
import qualified Text.Printf as Printf
import Data.Char
import Data.Ratio
import Data.Array
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Error
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.ByteString.Lazy as B
import System.IO.Unsafe
import qualified Control.Exception as E
import Codec.Text.IConv
import qualified Data.Word as W


-- | A Haskell representation of a JSON
--   data structure.
data JsonData = JDString String                   |
                JDNumber Double                   |
                JDArray [JsonData]                |
                JDBool Bool                       |
                JDNull                            |
                JDObject (M.Map String JsonData)

listJoin :: a -> [a] -> [a]
listJoin _ [] = []
listJoin _ l@[x] = l
listJoin k (x:ys) = x : k : (listJoin k ys)

concatJoin :: String -> [String] -> String
concatJoin k l = concat (listJoin k l)

alistToJsonDict :: [(String, String)] -> String
alistToJsonDict l =
    "{" ++
    concatJoin "," (map (\(k,v) -> (escapeString k) ++ ":" ++ v) l)
    ++ "}"

-- Special characters which will be pretty printed.
escapeMap :: M.Map Char String
escapeMap = M.fromList [
    ('\\', "\\"), ('"', "\""), ('\'', "'"), ('\n', "n"),
    ('\r', "r"), ('\f', "f"), ('\t', "t"), ('\b', "\b")]
escape :: Char -> Maybe String
escape c = M.lookup c escapeMap

-- Characters which can safely be printed as literals.
allowed' c o
  | o > 127 = True -- Any unicode char is OK.
  | o >= 32 && o < 127 {- exclude DEL == 127 -} && c /= '"' = True
  | True = False
allowed c = allowed' c (ord c)

hexEscape :: Char -> String
hexEscape c = Printf.printf "\\u%04x" (ord c)

escapeString' :: String -> String
escapeString' [] = "\""
escapeString' (c:cs)
    | allowed c =
        c : (escapeString' cs)
    | True =
        (maybe (hexEscape c) (\s -> "\\" ++ s) (escape c)) ++
        (escapeString' cs)

escapeString s = '"' : escapeString' s

instance Show JsonData where
    show (JDString s)   = escapeString s
    show (JDNumber n)
        -- Show as an integer if possible, otherwise as a Double.
        -- TODO: Not sure if this is the proper way of testing whether a
        --       double is an integer value +/- epsilon.
        | (fromIntegral (floor n)) == n = show (floor n)
        | True                          = show n
    show (JDBool True)  = "true"
    show (JDBool False) = "false"
    show (JDArray l)    = "[" ++ concatJoin "," (map show l) ++ "]"
    show JDNull         = "null"
    show (JDObject o)   = alistToJsonDict (map (\(k,v) -> (k, show v)) (M.toList o))


--
-- TranslateField class.
--
class TranslateField a where
    -- | This method defines the mapping from Haskell record field names
    --   to JSON object field names. The default is to strip any initial
    --   underscores. Specialize this method to define a different behavior.
    translateField :: a -> String -> String

data TranslateFieldD a = TranslateFieldD { translateFieldD :: a -> String -> String }

translateFieldProxy :: Proxy TranslateFieldD
translateFieldProxy = error "'translateFieldProxy' value should never be evaluated!"

instance (TranslateField t) => Sat (TranslateFieldD t) where
    dict = TranslateFieldD { translateFieldD = translateField }

-- | Removes initial underscores from a string.
stripInitialUnderscores ""      = ""
stripInitialUnderscores ('_':s) = stripInitialUnderscores s
stripInitialUnderscores s       = s

instance Typeable a => TranslateField a where
    translateField _ x = stripInitialUnderscores x


--
-- ToJson class plus SYB boilerplate.
--
-- | New instances can be added to this class to customize certain aspects
--   of the way in which Haskell types are serialized to JSON.
class TranslateField a => ToJson a where
    toJson :: a -> JsonData

    -- For lists (same trick used by the Prelude to allow special
    -- handling of list types for Show).
    lToJson  :: [a] -> JsonData
    lToJson l = JDArray (map toJson l)

    -- | Applies to record types only. You can specialize this method to
    --   prevent certain fields from being serialized.
    --   Given a Haskell field name, it should return True if that field is
    --   to be serialized, and False otherwise.
    exclude  :: a -> String -> Bool
    exclude _ _ = False

    -- | Types that will be converted to JSON arrays can override
    --   this method to specify additional elements to be prepended to the array.
    arrayPrepend :: a -> [JsonData]
    arrayPrepend _ = []

    -- | Types that will be converted to JSON arrays can override
    --   this method to specify additional elements to be appended to the array.
    arrayAppend :: a -> [JsonData]
    arrayAppend _ = []

    -- | Types that will be converted to JSON objects can override
    --   this method to specify additional fields of the object.
    objectExtras :: a -> [(String, JsonData)]
    objectExtras _ = []

-- Note the inclusion of translateField from TranslateField.
data ToJsonD a = ToJsonD { toJsonD          :: a -> JsonData,
                           excludeD         :: a -> String -> Bool,
                           arrayPrependD    :: a -> [JsonData],
                           arrayAppendD     :: a -> [JsonData],
                           objectExtrasD    :: a -> [(String, JsonData)],
                           translateFieldD' :: a -> String -> String }

toJsonProxy :: Proxy ToJsonD
toJsonProxy = error "'toJsonProxy' value should never be evaluated!"

-- Again, note inclusion of translateField from TranslateField.
instance ToJson t => Sat (ToJsonD t) where
    dict = ToJsonD { toJsonD          = toJson,
                     excludeD         = exclude,
                     arrayPrependD    = arrayPrepend,
                     arrayAppendD     = arrayAppend,
                     objectExtrasD    = objectExtras,
                     translateFieldD' = translateField }


--
-- Implementations of toJson for different data types.
--
instance ToJson Bool where
    toJson b = JDBool b

instance ToJson Int where
    toJson i = JDNumber (fromIntegral i)
instance ToJson Integer where
    toJson i = JDNumber (fromIntegral i)
--instance Json Float where
--    toJson i = JDNumber (floatToDouble i)
instance ToJson Double where
    toJson i = JDNumber i
instance (Integral a, TranslateField a, Typeable a) => ToJson (Ratio a) where
    toJson i = JDNumber $ (fromIntegral (numerator i)) / (fromIntegral (denominator i))
                        
instance ToJson Char where
    lToJson s = JDString s
    toJson c = JDString [c]

instance (Typeable a, ToJson a) => ToJson (Maybe a) where
    toJson (Just c) = toJson c
    toJson Nothing  = JDNull

instance (ToJson a, TranslateField a, Data TranslateFieldD (M.Map String a))
         => ToJson (M.Map String a) where
    toJson x = JDObject (M.map toJson x)

instance (ToJson a, TranslateField a, Typeable a) => ToJson [a] where
    toJson = lToJson

-- TODO: Add instances for the other array types supported by GHC.
instance (ToJson a, TranslateField a, Typeable a, Typeable i, Ix i) => ToJson (Array i a) where
    toJson a = toJson (elems a)

-- | This type can be used for merging two or more records together into a single
--   JSON object. By default, a structure such as (Union X Y) is serialized as follows.
--   First, X and Y are serialized, and a runtime error is signalled if the result of
--   serialization is not a JSON object in both cases. The key/value pairs of the
--   two JSON objects are then merged to form a single object.
data Union a b = Union a b deriving Show
$(derive[''Union]) -- In order to derive (Typeable2 Union).
                   -- It seems that we get away with overwriting the instance
                   -- of Data that this creates (if we didn't, we could always
                   -- instantiate Typeable manually for Union).
-- | Nested Unions are left-branching by convention (since this is what you get
--   by using the constructor as an infix operator).
type Union3 a b c = (Union (Union a b) c)
type Union4 a b c d = (Union (Union3 a b c) d)
type Union5 a b c d e = (Union (Union4 a b c d) e)
type Union6 a b c d e f = (Union (Union5 a b c d e) f)
type Union7 a b c d e f g = (Union (Union6 a b c d e f) g)
type Union8 a b c d e f g h = (Union (Union7 a b c d e f g) h)
type Union9 a b c d e f g h i = (Union (Union8 a b c d e f g h) i)
type Union10 a b c d e f g h i j = (Union (Union9 a b c d e f g h i) j)

-- Used by the (ToJson Union) instance below.
isJDObject (JDObject _) = True
isJDObject _            = False
jdObjectMap (JDObject m) = m

instance (ToJson a, ToJson b, TranslateField a, TranslateField b, Typeable a, Typeable b, Typeable2 Union) => ToJson (Union a b) where
    toJson (Union x y) =
        let jx = toJson x
            jy = toJson y
        in
          if isJDObject jx && isJDObject jy
              then JDObject (M.union (jdObjectMap jx) (jdObjectMap jy))
              else error "Bad toJson conversion: Attempt to unify JSON values which aren't both objects"

getFields :: Data ToJsonD a => a -> [String]
getFields = constrFields . (toConstr toJsonProxy)

typename x = dataTypeName (dataTypeOf toJsonProxy x)

-- | This function is used as the the implementation of 'toJson' for the
--   generic instance declaration.
--   It's useful to be able to use the same implentation for
--   other instance declarations which override the default implementations
--   of other methods of the ToJson class.
genericToJson :: (Data ToJsonD a, ToJson a, TranslateField a) => a -> JsonData
genericToJson x
    | isAlgType (dataTypeOf toJsonProxy x) =
        case getFields x of
          [] ->
              case gmapQ toJsonProxy (toJsonD dict) x of
                [v] -> v -- Special default behavior for algebraic constructors with one field.
                vs -> JDArray $ (arrayPrependD dict x) ++ vs ++ (arrayAppendD dict x)
          fs ->
              let
                translatedFsToInclude =
                  map (translateFieldD' dict x) (filter (not . (excludeD dict x)) (getFields x))
              in
                JDObject $ M.fromList (objectExtrasD dict x ++ (zip translatedFsToInclude (gmapQ toJsonProxy (toJsonD dict) x)))
    | True =
        error $ "Unable to serialize the primitive type '" ++ typename x ++ "'"

-- | This function can be used as an implementation of 'toJson' for simple enums.
--   It converts an enum value to a string determined by the name of the constructor,
--   after being fed through the (String -> String) function given as the first argument.
enumToJson :: (Data ToJsonD a, ToJson a, TranslateField a) => (String -> String) -> a -> JsonData
enumToJson transform x
    | isAlgType (dataTypeOf toJsonProxy x) = JDString (transform (showConstr (toConstr toJsonProxy x)))
    | True = error "Passed non-algebraic type to enumToJson"

instance (Data ToJsonD t, TranslateField t) => ToJson t where
    toJson = genericToJson

-- Instances for tuples up to n=7 (this limit it is set by the non-existence of Typeable8).
-- Tuples are converted to (heterogenous) JSON lists.
#define I(x) ToJson x, Typeable x
instance (I(a), I(b)) => ToJson (a, b) where
    toJson (a,b) = JDArray [toJson a, toJson b]
instance (I(a), I(b), I(c)) => ToJson (a,b,c) where
    toJson (a,b,c) = JDArray [toJson a, toJson b, toJson c]
instance (I(a), I(b), I(c), I(d)) => ToJson (a,b,c,d) where
    toJson (a,b,c,d) = JDArray [toJson a, toJson b, toJson c, toJson d]
instance (I(a), I(b), I(c), I(d), I(e)) => ToJson (a,b,c,d,e) where
    toJson (a,b,c,d,e) = JDArray [toJson a, toJson b, toJson c, toJson d, toJson e]
instance (I(a), I(b), I(c), I(d), I(e), I(f)) =>
         ToJson (a,b,c,d,e,f) where
    toJson (a,b,c,d,e,f) = JDArray [toJson a, toJson b, toJson c, toJson d, toJson e,
                                   toJson f]
instance (I(a), I(b), I(c), I(d), I(e), I(f), I(g)) =>
         ToJson (a,b,c,d,e,f,g) where
    toJson (a,b,c,d,e,f,g) = JDArray [toJson a, toJson b, toJson c, toJson d, toJson e,
                                     toJson f, toJson g]
#undef I

--
-- FromJson
--
class TranslateField a => FromJson a where
    fromJson :: a -> JsonData -> Either String a

    -- For lists (same trick used by the Prelude to allow special
    -- handling of list types for Show).
    lFromJson :: a -> JsonData -> Either String [a]
    lFromJson dummy (JDArray l) = mapM (fromJson dummy) l

    -- | To specify default values for the required fields of a JSON object,
    --   specialize this method in the instance definition for the relevant
    --   datatype.
    objectDefaults :: a -> M.Map String JsonData
    objectDefaults _ = M.empty

data FromJsonD a = FromJsonD { fromJsonD         :: a -> JsonData -> Either String a,
                               objectDefaultsD   :: a -> M.Map String JsonData,
                               translateFieldD'' :: a -> String -> String }

fromJsonProxy :: Proxy FromJsonD
fromJsonProxy = error "'fromJsonProxy' should never be evaluated!"

-- Note inclusion of translateField from TranslateField.
instance FromJson t => Sat (FromJsonD t) where
    dict = FromJsonD { fromJsonD = fromJson,
                       objectDefaultsD = objectDefaults,
                       translateFieldD'' = translateField }

instance FromJson Char where
    fromJson _ (JDString [c]) = Right c
    fromJson _ _              = Left "Bad fromJson conversion: JSON string not of length 1 to 'Char'"

    lFromJson _ (JDString s) = Right s
    lFromJson _ _            = Left "Bad fromJson conversion: Non-string to 'String'"

instance (FromJson a, TranslateField a, Typeable a) => FromJson (Maybe a) where
    fromJson _ JDNull = Right Nothing
    fromJson _ y =
      case fromJson undefined y of
           Left err -> Left err
           Right v -> Right $ Just v

instance (FromJson a, TranslateField a, Typeable a) => FromJson [a] where
    fromJson _ x = lFromJson undefined x

instance FromJson Int where
    fromJson _ (JDNumber n)
      | (fromIntegral (floor n)) == n = Right (floor n)
      | True =
          Left "Bad fromJson conversion: number does not approximate an integer ('Int')"
    fromJson _ _ = Left "Bad fromJson conversion: Non-numeric to 'Int'"

instance FromJson Integer where
    fromJson _ (JDNumber n)
      | (fromIntegral (floor n)) == n = Right (floor n)
      | True =
          Left "Bad fromJson conversion: number does not approximate an integer ('Integer')"
    fromJson _ _ = Left "Bad fromJson conversion: Non-numeric to 'Integer'"

instance FromJson Double where
    fromJson _ (JDNumber d) = Right d
    fromJson _ _            = Left "Bad fromJson conversion: Non-numeric to 'Double'"

instance (Typeable a, Integral a) => FromJson (Ratio a) where
    fromJson _ (JDNumber i) = Right (fromRational (toRational i))
    fromJson _ _            = Left "Bad fromJson conversion: Non-numeric to instance of 'Ratio'"

instance FromJson Bool where
    fromJson _ (JDBool b) = Right b
    fromJson _ _          = Left "Bad fromJson conversion: Non-boolean to 'Bool'"

-- TODO: Use monads instead of 'ifs' if possible (funky type errors
-- which I haven't figured out yet, something to do with monomorphism
-- in let bindings vs. lambda abstraction?).
instance (FromJson a, FromJson b, Typeable a, Typeable b, TranslateField a, TranslateField b) => FromJson (Union a b) where
    fromJson _ o@(JDObject _) =
        let r1 = fromJson undefined o
            r2 = fromJson undefined o
        in
          if isRight r1 && isRight r2
             then Right $ Union (fromRight r1) (fromRight r2)
             else Left "Bad fromJson conversion: error constructing subpart of union (did not serialize to object)"
    fromJson _ _ = Left "Bad fromJson conversion: attempt to convert non-object to Union"

tuperror :: Int -> Either String a
tuperror n = Left $ Printf.printf "Bad fromJson conversion: attempt to convert something that was not a list of length %i to a %i-tuple" n n

#define I(x) FromJson x, Typeable x, TranslateField x
instance (I(a), I(b)) => FromJson (a,b) where
    fromJson _ (JDArray [x1,x2]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      return (r1,r2)
    fromJson _ _ = tuperror 2
instance (I(a), I(b), I(c)) => FromJson (a,b,c) where
    fromJson _ (JDArray [x1,x2,x3]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      r3 <- fromJson undefined x3
      return (r1,r2,r3)
    fromJson _ _ = tuperror 3
instance (I(a), I(b), I(c), I(d)) => FromJson(a,b,c,d) where
    fromJson _ (JDArray [x1,x2,x3,x4]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      r3 <- fromJson undefined x3
      r4 <- fromJson undefined x4
      return (r1,r2,r3,r4)
    fromJson _ _ = tuperror 4
instance (I(a), I(b), I(c), I(d), I(e)) => FromJson (a,b,c,d,e) where
    fromJson _ (JDArray [x1,x2,x3,x4,x5]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      r3 <- fromJson undefined x3
      r4 <- fromJson undefined x4
      r5 <- fromJson undefined x5
      return (r1,r2,r3,r4,r5)
    fromJson _ _ = tuperror 5
instance (I(a), I(b), I(c), I(d), I(e), I(f)) =>
         FromJson (a,b,c,d,e,f) where
    fromJson _ (JDArray [x1,x2,x3,x4,x5,x6]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      r3 <- fromJson undefined x3
      r4 <- fromJson undefined x4
      r5 <- fromJson undefined x5
      r6 <- fromJson undefined x6
      return (r1,r2,r3,r4,r5,r6)
    fromJson _ _ = tuperror 6
instance (I(a), I(b), I(c), I(d), I(e), I(f), I(g)) =>
         FromJson (a,b,c,d,e,f,g) where
    fromJson _ (JDArray [x1,x2,x3,x4,x5,x6,x7]) = do
      r1 <- fromJson undefined x1
      r2 <- fromJson undefined x2
      r3 <- fromJson undefined x3
      r4 <- fromJson undefined x4
      r5 <- fromJson undefined x5
      r6 <- fromJson undefined x6
      r7 <- fromJson undefined x7
      return (r1,r2,r3,r4,r5,r6,r7)
    fromJson _ _ = tuperror 7
#undef I

elemsOfMap :: Ord k => M.Map k v -> [k] -> Maybe [v]
elemsOfMap _ [] = Just []
elemsOfMap m (x:xs) = do
  r <- M.lookup x m
  rs <- elemsOfMap m xs
  return (r : rs)

type ErrorWithState e s a = ErrorT e (State s) a

-- TODO: Not a very descriptive name. Oh well...
m1 :: (Data FromJsonD a) => ErrorWithState String [JsonData] a
m1 = do
  jvl <- lift get
  (case jvl of
     []       -> throwError "Bad fromJson conversion: Not enough elements in JSON array to satisfy constructor"
     (jv:jvs) -> do
       lift $ put jvs
       (case fromJsonD dict (undefined :: a) jv of
          Left e  -> throwError e
          Right x -> return x))

-- TODO: Again, uninformative name.
-- TODO: Some code duplication here.
m2 :: (Data FromJsonD a, TranslateField a) => M.Map String JsonData -> (String -> String) -> a -> ErrorWithState String (M.Map String JsonData, [String]) a
m2 defaults transFunc dummy = do
  (m, sl) <- lift get
  (case sl of
     []     -> throwError "Bad fromJson conversion: Not enough fields in JSON object to satisfy constructor"
     (f:fs) -> do
       lift $ put (m, fs)
       let stripped = transFunc f
       (case M.lookup stripped m of
          Nothing ->
            case M.lookup stripped defaults of
              Nothing -> throwError $  "Bad fromJson conversion: Required field not present in JSON object: " ++ stripped
              Just v  ->
                case fromJsonD dict dummy v of
                  Left e  -> throwError e
                  Right x -> return x
          Just v ->
            case fromJsonD dict dummy v of
              Left e  -> throwError e
              Right x -> return x))

-- TODO: Another uninformative name.
m3 :: (Data FromJsonD a, TranslateField a) => JsonData -> a -> ErrorWithState String Int a
m3 jsondata dummy = do
    s <- get
    if s > 0
       then throwError "Bad fromJson conversion: Expecting JSON object or array; did not attempt automatic boxing because constructor takes more than one argument."
       else do
         put (s + 1)
         case fromJsonD dict dummy jsondata of
           Left e -> throwError e
           Right x -> return x


genericFromJson :: (Data FromJsonD a, FromJson a, TranslateField a) => a -> JsonData -> Either String a
genericFromJson dummy (JDArray l) =
    case datarep (dataTypeOf fromJsonProxy dummy) of
      AlgRep ccs@(c:cs) -> evalArrayConstr ccs
			where
				evalArrayConstr = tryHead err . dropWhile isLeft . map es
				es :: (Data FromJsonD a, FromJson a) => Constr -> Either String a
				es c = evalState (runErrorT (fromConstrM fromJsonProxy m1 c)) (tryTail l)
				tryTail = cond null (const []) tail
				tryHead def = cond null (const def) head
				err = Left "Bad fromJson conversion: Type with no constructors!"
      AlgRep _     -> Left "Bad fromJson conversion: Type with no constructors!"
      _            -> Left "Bad fromJson conversion: Non-algebraic datatype given to 'genericFromJson'"
genericFromJson dummy (JDObject m) =
    case datarep (dataTypeOf fromJsonProxy dummy) of
      AlgRep cs@(_:_) -> evalConstrs dummy m cs
      _			-> Left "Bad fromJson conversion: Non-algebraic datatype given to 'genericFromJson'"
genericFromJson dummy jsondata =
    case datarep (dataTypeOf fromJsonProxy dummy) of
        AlgRep [c] -> evalState (runErrorT (gmapM fromJsonProxy (m3 jsondata) (fromConstr fromJsonProxy c))) 0
        AlgRep _ -> Left "Bad fromJson conversion: Expecting JSON object or array; did not attempt automatic boxing because type has more than one constructor."
genericFromJson _ _ = Left "Bad fromJson conversion: Expecting JSON object or array"

evalConstrs :: (Data FromJsonD a, FromJson a) => a -> M.Map String JsonData -> [Constr] -> Either [Char] a
evalConstrs dummy m = tryHead err . dropWhile isLeft . map (evalConstr dummy m)
	where
		tryHead def = cond null (const def) head
		err = Left "Bad fromJson conversion: Type with no constructors!"

evalConstr :: (Data FromJsonD a, FromJson a) => a -> M.Map String JsonData -> Constr -> Either [Char] a
evalConstr dummy m c = case constrFields c of
    [] -> Left $ "Bad fromJson conversion: Attempt to convert JDObect to a non-record algebraic type"
    -- TODO:
    -- Can't use fromConstrM because we need to get dummy values of the
    -- appropriate type for each argument of the constructor. This is unfortunate,
    -- becuase it means that we get runtime errors for records with strict fields.
    fs -> evalState (runErrorT (gmapM fromJsonProxy (m2 (objectDefaultsD dict dummy) (translateFieldD'' dict dummy)) (fromConstr fromJsonProxy c))) (m, fs)

constrNames :: (Data FromJsonD a, Data TranslateFieldD a) => a -> [String]
constrNames x = map showConstr (dataTypeConstrs (dataTypeOf fromJsonProxy x))

-- | The counterpart of 'enumToJson'.
enumFromJson :: (Data FromJsonD a, Data TranslateFieldD a) => (String -> String) -> a -> JsonData -> Either String a
enumFromJson transform dummy (JDString s) =
    let cname = (transform s) in
    if elem cname (constrNames dummy)
       then
           case fromConstrM fromJsonProxy Nothing (mkConstr (dataTypeOf fromJsonProxy dummy) cname [] Prefix ) of
             Nothing -> Left "Error in enumFromJson"
             Just x -> Right x
       else Left "Constructor name not recognized in enumFromJson"
enumFromJson _ _ _ = Left "Non-string given to enumFromJson"
          
instance (Data FromJsonD t, TranslateField t) => FromJson t where
    fromJson = genericFromJson


--
-- JSON parser.
--

-- Determine the unicode encoding of a byte stream
-- on the assumption that it begins with two ASCII characters.
getEncoding :: B.ByteString -> EncodingName
getEncoding s
  | B.length s < 4 = "UTF-8" -- If the string is shorter than 4 bytes,
                             -- we have no way of determining the encoding.
  | True =
      let bs1 = B.index s 0
          bs2 = B.index s 1
          bs3 = B.index s 2
          bs4 = B.index s 3
      in
        -- Little endian UTF 32/16.
        if bs1 /= 0 && bs2 == 0 && bs3 == 0 && bs4 == 0
        then "UTF-32LE"
        else if bs1 /= 0 && bs2 == 0 && bs3 /= 0 && bs4 == 0
        then "UTF-16LE"
        -- Big endian UTF 32/16.
        else if bs1 == 0 && bs2 == 0 && bs3 == 0 && bs4 /= 0
        then "UTF-32BE"
        else if bs1 == 0 && bs2 /= 0 && bs3 == 0 && bs4 /= 0
        then "UTF-16BE"
        -- UTF-8
        else if bs1 /= 0 && bs2 /= 0 && bs3 /= 0 && bs4 /= 0
        then "UTF-8" -- BOM allowed but not required for UTF-8.
        -- If we can't figure it out, guess at UTF-8.
        else "UTF-8"

-- Converts a ByteString to a String of unicode code points.
toHaskellString :: EncodingName -> B.ByteString -> String
toHaskellString enc source =
    stripBOM $ map chr (pairBytes (B.unpack bs))
    where
      pairBytes :: [W.Word8] -> [Int]
      pairBytes [] = []
      pairBytes (c:c':cs) = ((fromIntegral c) + (fromIntegral c')*256) : (pairBytes cs)
      bs = convertFuzzy Discard enc "UTF-16LE" source

stripBOM :: String -> String
stripBOM ('\0':'\0':'\xFE':'\xFF':cs) = cs
stripBOM ('\xFF':'\xFE':'\0':'\0':cs) = cs
stripBOM ('\xFE':'\xFF':cs)           = cs
stripBOM ('\xFF':'\xFE':cs)           = cs
stripBOM ('\xEF':'\xBB':'\xBF':cs)    = cs
stripBOM cs                           = cs

(<|>) = (P.<|>)

-- | Converts a ByteString to an instance of JsonData (unicode encoding
--   is detected automatically).
parseJsonByteString :: B.ByteString -> Either String JsonData
parseJsonByteString bs =
    let
        decoded = toHaskellString (getEncoding bs) bs
    in
      case P.runParser (ws >> jsonValue) () "" decoded of
        Left e -> Left (show e)
        Right x -> Right x

-- | Converts a String (interpreted as a true unicode String) to an instance
--   of JsonData.
parseJsonString :: String -> Either String JsonData
parseJsonString s =
    case P.runParser (ws >> jsonValue) () "" s of
      Left e -> Left (show e)
      Right x -> Right x

apply f p = do
  r <- p
  return (f r)

pconcat p1 p2 = do
  l1 <- p1
  l2 <- p2
  return $ l1 ++ l2

listify :: P.Parser x -> P.Parser [x]
listify = apply (:[])

ws = P.many (P.oneOf [' ','\r','\n','\t','\f','\v'])

-- Could use the ParsecToken module, but trying a floating point number
-- then an integer is a bit inefficient (especially since integers will
-- be more common).
number :: P.Parser JsonData
number = do
  neg <- (P.char '-' >> return True) <|> return False
  i <- P.many1 P.digit
  point <- P.option Nothing (apply Just (P.char '.' >> P.many1 P.digit))
  exponent <- P.option Nothing (apply Just (P.char 'e' >> pconcat (P.option "" (listify (P.char '-'))) (P.many1 P.digit)))
  let n = if point == Nothing && exponent == Nothing
            then read i :: Double
            else read (i ++ (if point == Nothing then "" else "." ++ fromJust point) ++
                        (if exponent == Nothing then "" else "e" ++ fromJust exponent)) :: Double
  return . JDNumber $ if neg then negate n else n
   
stringChar :: Char -> P.Parser Char
stringChar opener = do
  -- Fail immediately on either single or double quotes or
  -- on control characters.
  c <- P.satisfy (\c -> c /= opener && (ord c) > 31)
  (case c of
     '\\' ->
         (P.char '"' >> return '"')   <|>
         (P.char '\'' >> return '\'') <|>
         (P.char 'b' >> return '\b')  <|>
         (P.char 'f' >> return '\f')  <|>
         (P.char 'n' >> return '\n')  <|>
         (P.char 'r' >> return '\r')  <|>
         (P.char 't' >> return '\t')  <|>
         (do
           P.char 'u'
           ds <- P.count 4 P.hexDigit
           return $ chr (read ("0x" ++ ds) :: Int)) <|>
         (P.satisfy allowed >>= return) -- "\X" == "X" by default.
     c -> return c)

string :: P.Parser String
string = do
  opener <- P.char '"' <|> P.char '\'' -- JSON spec requires double quotes, but we'll be lenient.
  cs <- P.many (stringChar opener)
  P.char opener
  return cs

jsonString = apply JDString string

kvp :: P.Parser (String, JsonData)
kvp = do
  s <- string
  ws
  P.char ':'
  ws
  v <- jsonValue
  return (s, v)

lexeme :: P.Parser a -> P.Parser a
lexeme p = do
    r <- p
    ws
    return r

jsonArray :: P.Parser JsonData
jsonArray = do
  P.char '['
  ws
  vs <- P.sepBy (lexeme jsonValue) (P.char ',' >> ws)
  ws
  P.char ']'
  return $ JDArray vs

object :: P.Parser JsonData
object = do
    P.char '{'
    ws
    kvps <- P.sepBy (lexeme kvp) (P.char ',' >> ws)
    ws
    P.char '}'
    return $ JDObject $ M.fromList kvps

boolean :: P.Parser JsonData
boolean = (P.try (P.string "true") >> return (JDBool True)) <|>
          (P.string "false" >> return (JDBool False))

jsonNull :: P.Parser JsonData
jsonNull = P.string "null" >> return JDNull

jsonValue = number <|> jsonString <|> jsonArray <|> object <|> boolean <|> jsonNull


--
-- Some other utilities.
--
-- | Converts a JSON String (interpreted as a true unicode string) to
--   a value of the type given by the first (dummy) argument.
fromJsonString :: FromJson a => a -> String -> Either String a
fromJsonString dummy s =
    case parseJsonString s of
      Left e -> Left (show e)
      Right js ->
          case fromJson dummy js of
            Left e -> Left e
            Right js -> Right js

-- | Converts a JSON ByteString (with unicode encoding automatically detected)
--   to a value of the type given by the first (dummy) argument.
fromJsonByteString :: FromJson a => a -> B.ByteString -> Either String a
fromJsonByteString dummy s =
    case parseJsonByteString s of
      Left e -> Left (show e)
      Right js ->
          case fromJson dummy js of
            Left e -> Left e
            Right js -> Right js

-- | Converts a value to an ASCII-only JSON String.
toJsonString :: ToJson a => a -> String
toJsonString = show . toJson


--
-- A couple of utility functions.
--
-- | Converts the first character of a string to upper case.
firstCharToUpper :: String -> String
firstCharToUpper "" = ""
firstCharToUpper (c:cs) = (toUpper c) : cs

-- | Converts the first character of a string to lower case.
firstCharToLower :: String -> String
firstCharToLower "" = ""
firstCharToLower (c:cs) = (toLower c) : cs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromRight :: Either a b -> b
fromRight (Right x) = x

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p th el a = if p a then th a else el a
 