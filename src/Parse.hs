module Parse (parseProgs) where

import Types

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Bits (Bits, clearBit, complement, testBit)
import Data.List
import Data.Word

import Control.Monad

type SectionParser = Progs -> Int -> Get Progs

getNullTerminatedString :: Progs -> Int -> B.ByteString
getNullTerminatedString progs offset =
  B.takeWhile (/= 0) $ B.drop offset (progsStrings progs)

parseString :: Progs -> Get B.ByteString
parseString progs = do
  offset <- getWord32le
  return $! getNullTerminatedString progs $ fromIntegral offset

readOffNum :: Get (Int, Int)
readOffNum = do
  off <- fmap fromIntegral getWord32le
  num <- fmap fromIntegral getWord32le
  return (off, num)

parseOps :: SectionParser
parseOps progs num = do
  ops <- replicateM num $ mkOp <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le
  return progs{progsOps = ops}

parseDefs :: Progs -> Int -> Get [Def]
parseDefs progs num =
  replicateM num $ mkDef <$> getWord16le <*> getWord16le <*> parseString progs

parseGlobals :: SectionParser
parseGlobals progs num = do
  defs <- parseDefs progs num
  return progs{progsGlobals = defs}

parseFields :: SectionParser
parseFields progs num = do
  defs <- parseDefs progs num
  return progs{progsFields = defs}

parseFuncs :: SectionParser
parseFuncs progs num = do
  funcs <- replicateM num $ do
    off <- getWord32le
    localsStart <- fmap fromIntegral getWord32le
    numLocals <- fmap fromIntegral getWord32le
    skip 4 -- profile
    name <- parseString progs
    skip 4 -- file name
    numParams <- fmap fromIntegral getWord32le
    paramSizes_ <- fmap B.unpack $ getByteString 8
    let paramSizes = take numParams $ map fromIntegral paramSizes_
    return $ case off `testBit` 31 of
      False -> Func{ funcName = name
                   , funcStart = fromIntegral off
                   , funcLocals = take numLocals $ iterate' succ localsStart
                   , funcParamSizes = paramSizes
                   }
      True -> Builtin{ builtinName = name
                     , builtinId = fromIntegral . complement $ pred off
                     , builtinNumParams = numParams
                     }
  return progs{progsFuncs = funcs}

parseStrings :: SectionParser
parseStrings progs size = do
  strings <- getByteString $ fromIntegral size
  return progs{ progsStrings = strings }

parseGlobalValues :: SectionParser
parseGlobalValues progs num = do
  globalValues <- getByteString $ num * 4
  return progs{progsGlobalValues = globalValues}

parseSections :: Progs -> [((Int, Int), Progs -> Int -> Get Progs)] -> Get Progs
parseSections progs [] = return progs
parseSections progs (e:parsers) = do
  p <- parseSection progs e
  parseSections p parsers

parseSection :: Progs -> ((Int, Int), Progs -> Int -> Get Progs) -> Get Progs
parseSection progs ((off, num), parser) = do
  soFar <- fmap fromIntegral bytesRead
  skip $ off - soFar
  parser progs num

decoder :: Get (Either String Progs)
decoder = do
  skip 4 -- version
  skip 4 -- crc
  let sections = sortBy (\((x, _), _) ((y, _), _) -> compare x y) <$>
                 mapM (\pf -> readOffNum >>= \offNum -> return (offNum, pf))
                 [ parseOps
                 , parseGlobals
                 , parseFields
                 , parseFuncs
                 , parseStrings
                 , parseGlobalValues
                 ]
  p <- sections >>= parseSections progs
  return $ Right p
  where
    progs = Progs{ progsOps = []
                 , progsGlobals = []
                 , progsFields = []
                 , progsFuncs = []
                 , progsStrings = B.empty
                 , progsGlobalValues = B.empty
                 }

parseProgs :: L.ByteString -> (Either String Progs)
parseProgs contents = do
  case runGetOrFail decoder contents of
    Left (_, _, e) -> Left e
    Right (_, _, p) -> p

mkOp :: Integral a => a -> Word16 -> Word16 -> Word16 -> Op
mkOp n a b c =
  case n of
    0 -> OpReturn a
    1 -> OpMulF a b c
    2 -> OpMulV a b c
    3 -> OpMulFV a b c
    4 -> OpMulVF a b c
    5 -> OpDivF a b c
    6 -> OpAddF a b c
    7 -> OpAddV a b c
    8 -> OpSubF a b c
    9 -> OpSubV a b c
    10 -> OpEqF a b c
    11 -> OpEqV a b c
    12 -> OpEqS a b c
    13 -> OpEqE a b c
    14 -> OpEqFunc a b c
    15 -> OpNeF a b c
    16 -> OpNeV a b c
    17 -> OpNeS a b c
    18 -> OpNeE a b c
    19 -> OpNeFunc a b c
    20 -> OpLE a b c
    21 -> OpGE a b c
    22 -> OpLT a b c
    23 -> OpGT a b c
    24 -> OpLoadF a b c
    25 -> OpLoadV a b c
    26 -> OpLoadS a b c
    27 -> OpLoadE a b c
    28 -> OpLoadField a b c
    29 -> OpLoadFunc a b c
    30 -> OpAddress a b c
    31 -> OpStoreF a b
    32 -> OpStoreV a b
    33 -> OpStoreS a b
    34 -> OpStoreE a b
    35 -> OpStoreField a b
    36 -> OpStoreFunc a b
    37 -> OpStorePtrF a b
    38 -> OpStorePtrV a b
    39 -> OpStorePtrS a b
    40 -> OpStorePtrE a b
    41 -> OpStorePtrField a b
    42 -> OpStorePtrFunc a b
    43 -> OpReturn a
    44 -> OpNotF a c
    45 -> OpNotV a c
    46 -> OpNotS a c
    47 -> OpNotE a c
    48 -> OpNotFunc a c
    49 -> OpIf a b
    50 -> OpIfNot a b
    51 -> OpCall 0 a
    52 -> OpCall 1 a
    53 -> OpCall 2 a
    54 -> OpCall 3 a
    55 -> OpCall 4 a
    56 -> OpCall 5 a
    57 -> OpCall 6 a
    58 -> OpCall 7 a
    59 -> OpCall 8 a
    60 -> OpState a b
    61 -> OpGoto a
    62 -> OpAnd a b c
    63 -> OpOr a b c
    64 -> OpBitAnd a b c
    65 -> OpBitOr a b c
    _ -> error "invalid opcode"

mkDef :: (Bits a, Integral a) => a -> Word16 -> B.ByteString -> Def
mkDef t =
  case t `clearBit` 15 of
    0 -> DefVoid
    1 -> DefS
    2 -> DefF
    3 -> DefV
    4 -> DefE
    5 -> DefField
    6 -> DefFunc
    7 -> DefPtr
    _ -> error "invalid def type"
  $ t `testBit` 15
