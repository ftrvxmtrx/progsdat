module Types where

import Data.Word (Word16)
import Data.ByteString (ByteString)
import Data.Vector (Vector)

data Op = OpReturn !Word16
        | OpMulF !Word16 !Word16 !Word16
        | OpMulV !Word16 !Word16 !Word16
        | OpMulFV !Word16 !Word16 !Word16
        | OpMulVF !Word16 !Word16 !Word16
        | OpDivF !Word16 !Word16 !Word16
        | OpAddF !Word16 !Word16 !Word16
        | OpAddV !Word16 !Word16 !Word16
        | OpSubF !Word16 !Word16 !Word16
        | OpSubV !Word16 !Word16 !Word16
        | OpEqF !Word16 !Word16 !Word16
        | OpEqV !Word16 !Word16 !Word16
        | OpEqS !Word16 !Word16 !Word16
        | OpEqE !Word16 !Word16 !Word16
        | OpEqFunc !Word16 !Word16 !Word16
        | OpNeF !Word16 !Word16 !Word16
        | OpNeV !Word16 !Word16 !Word16
        | OpNeS !Word16 !Word16 !Word16
        | OpNeE !Word16 !Word16 !Word16
        | OpNeFunc !Word16 !Word16 !Word16
        | OpLE !Word16 !Word16 !Word16
        | OpGE !Word16 !Word16 !Word16
        | OpLT !Word16 !Word16 !Word16
        | OpGT !Word16 !Word16 !Word16
        | OpLoadF !Word16 !Word16 !Word16
        | OpLoadV !Word16 !Word16 !Word16
        | OpLoadS !Word16 !Word16 !Word16
        | OpLoadE !Word16 !Word16 !Word16
        | OpLoadField !Word16 !Word16 !Word16
        | OpLoadFunc !Word16 !Word16 !Word16
        | OpAddress !Word16 !Word16 !Word16
        | OpStoreF !Word16 !Word16
        | OpStoreV !Word16 !Word16
        | OpStoreS !Word16 !Word16
        | OpStoreE !Word16 !Word16
        | OpStoreField !Word16 !Word16
        | OpStoreFunc !Word16 !Word16
        | OpStorePtrF !Word16 !Word16
        | OpStorePtrV !Word16 !Word16
        | OpStorePtrS !Word16 !Word16
        | OpStorePtrE !Word16 !Word16
        | OpStorePtrField !Word16 !Word16
        | OpStorePtrFunc !Word16 !Word16
        | OpNotF !Word16 !Word16
        | OpNotV !Word16 !Word16
        | OpNotS !Word16 !Word16
        | OpNotE !Word16 !Word16
        | OpNotFunc !Word16 !Word16
        | OpIf !Word16 !Word16
        | OpIfNot !Word16 !Word16
        | OpCall !Int !Word16
        | OpState !Word16 !Word16
        | OpGoto !Word16
        | OpAnd !Word16 !Word16 !Word16
        | OpOr !Word16 !Word16 !Word16
        | OpBitAnd !Word16 !Word16 !Word16
        | OpBitOr !Word16 !Word16 !Word16
        deriving Show

data Def = DefVoid !Bool !Word16 !ByteString
         | DefS !Bool !Word16 !ByteString
         | DefF !Bool !Word16 !ByteString
         | DefV !Bool !Word16 !ByteString
         | DefE !Bool !Word16 !ByteString
         | DefField !Bool !Word16 !ByteString
         | DefFunc !Bool !Word16 !ByteString
         | DefPtr !Bool !Word16 !ByteString
         deriving Show

data Func = Func { funcName       :: !ByteString
                 , funcStart      :: !Int
                 , funcLocals     :: ![Int]
                 , funcParamSizes :: ![Int]
                 }
          | Builtin { builtinName      :: !ByteString
                    , builtinId        :: !Int
                    , builtinNumParams :: !Int
                    }
          deriving Show

data Progs = Progs { progsOps          :: Vector Op
                   , progsGlobals      :: Vector Def
                   , progsFields       :: Vector Def
                   , progsFuncs        :: Vector Func
                   , progsStrings      :: ByteString
                   , progsGlobalValues :: ByteString
                   } deriving Show
