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

data Def = DefVoid !Bool !ByteString
         | DefS !Bool !ByteString
         | DefF !Bool !ByteString
         | DefV !Bool !ByteString
         | DefE !Bool !ByteString
         | DefField !Bool !ByteString
         | DefFunc !Bool !ByteString
         | DefPtr !Bool !ByteString
         deriving Show

defName :: Def -> ByteString
defName (DefVoid _ name) = name
defName (DefS _ name) = name
defName (DefF _ name) = name
defName (DefV _ name) = name
defName (DefE _ name) = name
defName (DefField _ name) = name
defName (DefFunc _ name) = name
defName (DefPtr _ name) = name

data Local = LocalAt !Word16
           | Local Def
           deriving Show

data Func = Func { funcName       :: !ByteString
                 , funcStart      :: !Int
                 , funcParamSizes :: ![Int]
                 , funcLocals     :: [Local]
                 }
          | Builtin { builtinName      :: !ByteString
                    , builtinId        :: !Int
                    , builtinNumParams :: !Int
                    }
          deriving Show

data Progs = Progs { progsOps          :: Vector Op
                   , progsGlobals      :: Vector (Maybe Def)
                   , progsFields       :: Vector (Maybe Def)
                   , progsFuncs        :: Vector Func
                   , progsStrings      :: ByteString
                   , progsGlobalValues :: ByteString
                   } deriving Show
