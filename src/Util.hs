module Util (funcNumParams, findFunc, dumpOps, globalAtOffset) where

import Parse
import Types

import Control.Monad

import Data.Bits (complement, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Vector hiding ((++), foldM)
import Data.Word (Word16, Word32)
import Data.Binary.IEEE754 (getFloat32le)
import Data.Binary.Get (runGet, getWord32le)

funcNumParams :: Func -> Int
funcNumParams =
  Prelude.length . funcParamSizes

findFunc :: Progs -> B.ByteString -> Maybe Func
findFunc p name =
  find (\x -> case x of
                  Builtin{} -> False
                  Func{funcName = n} -> name == n
         ) $ progsFuncs p

globalAtOffset :: Progs -> Word16 -> Maybe Def
globalAtOffset p off =
  maybe Nothing id $ progsGlobals p !? fromIntegral off

fieldAtOffset :: Progs -> Word16 -> Maybe Def
fieldAtOffset p off =
  maybe Nothing id $ progsFields p !? fromIntegral off

assign :: Word16 -> Word16 -> Word16 -> (Word16 -> String) -> String -> String
assign a b c at o =
  at c ++ " = " ++ at a ++ " " ++ o ++ " " ++ at b

word16ToSigned :: Word16 -> Int
word16ToSigned w =
  case w `testBit` 15 of
      False -> fromIntegral w
      True -> 0 - (fromIntegral . complement $ pred w)

word32At :: Word16 -> BS.ByteString -> Word32
word32At offset bs =
  runGet getWord32le $ BL.fromStrict $ BS.take 4 $ BS.drop (fromIntegral offset * 4) bs

float32At :: Word16 -> BS.ByteString -> Float
float32At offset bs =
  runGet getFloat32le $ BL.fromStrict $ BS.take 4 $ BS.drop (fromIntegral offset * 4) bs

globalValueS :: Progs -> Word16 -> B.ByteString
globalValueS progs offset =
  getNullTerminatedString progs (fromIntegral . word32At offset $ progsGlobalValues progs)

globalValueF :: Progs -> Word16 -> Float
globalValueF progs offset =
  float32At offset $ progsGlobalValues progs

globalValueV :: Progs -> Word16 -> (Float, Float, Float)
globalValueV progs offset =
  (float32At offset v, float32At (offset + 4) v, float32At (offset + 8) v)
  where
    v = progsGlobalValues progs

globalAsValue :: Progs -> Word16 -> String
globalAsValue progs offset =
  case globalAtOffset progs offset of
    Just d -> case B.unpack $ defName d of
                "IMMEDIATE" -> case d of
                                 DefVoid _ _ -> "void"
                                 DefS _ _ -> show $ globalValueS progs offset
                                 DefF _ _ -> show $ globalValueF progs offset
                                 DefV _ _ -> show $ globalValueV progs offset
                                 DefE _ _ -> "entity&" ++ show offset
                                 DefField _ _ -> "field&" ++ show offset
                                 DefFunc _ _ ->  "func&" ++ show offset
                                 DefPtr _ _ -> "ptr&" ++ show offset
                name -> name
    Nothing -> "&" ++ show offset

prettyOp :: Progs -> Int -> Op -> String
prettyOp progs opIndex op =
  case op of
    OpReturn r -> "return " ++ globalAsValue progs r
    OpMulF a b c -> assign a b c fAt "*"
    OpMulV a b c -> assign a b c vAt "*"
    OpMulFV a b c -> vAt c ++ " = {" ++ fAt a ++ "*" ++ vAtInd b 0 ++ ", " ++
                                        fAt a ++ "*" ++ vAtInd b 1 ++ ", " ++
                                        fAt a ++ "*" ++ vAtInd b 2 ++
                              "}"
    OpMulVF a b c -> vAt c ++ " = {" ++ fAt b ++ "*" ++ vAtInd a 0 ++ ", " ++
                                        fAt b ++ "*" ++ vAtInd a 1 ++ ", " ++
                                        fAt b ++ "*" ++ vAtInd a 2 ++
                              "}"
    OpDivF a b c -> assign a b c fAt "/"
    OpAddF a b c -> assign a b c fAt "+"
    OpAddV a b c -> assign a b c vAt "+"
    OpSubF a b c -> assign a b c fAt "-"
    OpSubV a b c -> assign a b c vAt "-"
    OpEqF a b c -> assign a b c fAt "=="
    OpEqV a b c -> assign a b c vAt "=="
    OpEqS a b c -> assign a b c sAt "=="
    OpEqE a b c -> assign a b c eAt "=="
    OpEqFunc a b c -> assign a b c funcAt "=="
    OpNeF a b c -> assign a b c fAt "!="
    OpNeV a b c -> assign a b c vAt "!="
    OpNeS a b c -> assign a b c sAt "!="
    OpNeE a b c -> assign a b c eAt "!="
    OpNeFunc a b c -> assign a b c funcAt "!="
    OpLE a b c -> assign a b c fAt "<="
    OpGE a b c -> assign a b c fAt ">="
    OpLT a b c -> assign a b c fAt "<"
    OpGT a b c -> assign a b c fAt ">"
    OpLoadF a b c -> assignField a b c fAt
    OpLoadV a b c -> assignField a b c vAt
    OpLoadS a b c -> assignField a b c sAt
    OpLoadE a b c -> assignField a b c eAt
    OpLoadField a b c -> assignField a b c fieldAt
    OpLoadFunc a b c -> assignField a b c funcAt
    OpAddress _ _ _ -> "FIXME address"
    OpStoreF a c -> fAt c ++ " = " ++ fAt a
    OpStoreV a c -> vAt c ++ " = " ++ vAt a
    OpStoreS a c -> sAt c ++ " = " ++ sAt a
    OpStoreE a c -> eAt c ++ " = " ++ eAt a
    OpStoreField a c -> fAt c ++ " = " ++ fieldAt a
    OpStoreFunc a c -> funcAt c ++ " = " ++ funcAt a
    OpStorePtrF _ _ -> "FIXME store ptr f"
    OpStorePtrV _ _ -> "FIXME store ptr v"
    OpStorePtrS _ _ -> "FIXME store ptr s"
    OpStorePtrE _ _ -> "FIXME store ptr e"
    OpStorePtrField _ _ -> "FIXME store ptr field"
    OpStorePtrFunc _ _ -> "FIXME store ptr func"
    OpNotF a c -> fAt c ++ " = !" ++ fAt a
    OpNotV a c -> fAt c ++ " = !" ++ vAt a
    OpNotS a c -> fAt c ++ " = !" ++ sAt a
    OpNotE a c -> fAt c ++ " = !" ++ eAt a
    OpNotFunc a c -> fAt c ++ " = !" ++ funcAt a
    OpIf a c -> "if(" ++ fAt a ++ ") goto " ++ show (opIndex + word16ToSigned c)
    OpIfNot a c -> "if(!" ++ fAt a ++ ") goto " ++ show (opIndex + word16ToSigned c)
    OpCall _ f ->
      case globalAtOffset progs f of
        Just def -> B.unpack $ defName def
        Nothing -> "!" ++ show f
    OpState a b -> "self.frame = " ++ fAt a ++ "; self.think = " ++ funcAt b
    OpGoto a -> "goto " ++ show (opIndex + word16ToSigned a)
    OpAnd a b c -> assign a b c fAt "&&"
    OpOr a b c -> assign a b c fAt "||"
    OpBitAnd a b c -> assign a b c fAt "&"
    OpBitOr a b c -> assign a b c fAt "|"
  where
    fAt :: Word16 -> String
    fAt = globalAsValue progs

    vAt :: Word16 -> String
    vAt = globalAsValue progs

    sAt :: Word16 -> String
    sAt =  globalAsValue progs

    eAt :: Word16 -> String
    eAt = globalAsValue progs

    funcAt :: Word16 -> String
    funcAt = globalAsValue progs

    fieldAt :: Word16 -> String
    fieldAt a =
      case fieldAtOffset progs a of
        Just f -> "." ++ B.unpack (defName f)
        Nothing -> "field&." ++ show a

    vAtInd :: Word16 -> Int -> String
    vAtInd a i = vAt a ++ "[" ++ show i ++ "]"

    assignField :: Word16 -> Word16 -> Word16 -> (Word16 -> String) -> String
    assignField a b c at =
      at c ++ " = " ++ eAt a ++ "." ++ case fieldAtOffset progs (fromIntegral $ word32At b (progsGlobalValues progs)) of
                                         Just f -> B.unpack (defName f)
                                         Nothing -> "&" ++ show b

dumpOps :: Progs -> Int -> IO ()
dumpOps progs off = do
  _ <- dump (Data.Vector.replicate 8 B.empty) off [] []
  putStrLn ""
  where
    dump :: Vector B.ByteString -> Int -> [Int] -> [Int] -> IO [Int]
    dump _ offset _ visited | offset `L.elem` visited = return visited
    dump args offset toVisit visited = do
      putStr $ show offset ++ ": " ++ prettyOp progs offset op
      case op of
        OpStoreF a c -> next $ updateArgs a c
        OpStoreV a c -> next $ updateArgs a c
        OpStoreS a c -> next $ updateArgs a c
        OpStoreE a c -> next $ updateArgs a c

        OpCall n _ -> do
          putStr $ "(" ++ B.unpack (B.intercalate (B.pack ", ") (toList (Data.Vector.take n args))) ++ ")"
          next args

        OpGoto a | succ offset `Prelude.elem` toVisit -> do
          putStrLn ""
          dump args (succ offset) ((offset + word16ToSigned a):toVisit) (offset:visited)

        OpGoto a | not ((offset + word16ToSigned a) `Prelude.elem` visited) -> do
          putStrLn ""
          dump args (offset + word16ToSigned a) toVisit (offset:visited)

        OpIf _ c -> do
          putStrLn ""
          dump args (succ offset) (offset + word16ToSigned c : toVisit) (offset:visited)

        OpIfNot _ c -> do
          putStrLn ""
          dump args (succ offset) (offset + word16ToSigned c : toVisit) (offset:visited)

        OpReturn _ -> do
          putStrLn ""
          foldM (\allVisited toV -> dump args toV [] allVisited) (offset:visited) toVisit

        _ ->
          next args
      where
        op = progsOps progs ! offset
        next args' = do
          putStrLn ""
          dump args' (succ offset) toVisit (offset:visited)
        at = B.pack . globalAsValue progs
        updateArgs a c | c `L.elem` [4, 7, 10, 13, 16, 19, 22, 25] = args // [((fromIntegral c - 4) `div` 3, at a)]
                       | otherwise = args
