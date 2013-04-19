{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as S
import           Data.Hashable
import           Debug.Trace
import           GHC.Generics               (Generic)

import           Grammar


data Verbosity = V0 | V1
instance Eq Verbosity where
    V0 == V0 = True
    V1 == V1 = True
    _  == _  = False

data CmdOptions = CmdOpts
    { dest      :: FilePath
    , hdr       :: FilePath
    , verbosity :: Verbosity
    }

data Action
    = Shift SID (Prec, Assoc)
    | Reduce RID Prec
    | Accept
    | Error
    deriving (Show, Generic)

type SID             = Int
type Core            = (RID, Int)
type Context         = S.Set Terminal
type LR0State        = S.Set Core
type LR0StateTable   = M.HashMap SID (LR0State, Symbol, M.HashMap Symbol SID)
type LALR1State      = M.HashMap Core Context
type LALR1StateTable = M.HashMap SID (LALR1State, Symbol, M.HashMap Symbol SID)
type LookaheadTable  = M.HashMap SID (M.HashMap Core (Context, S.Set (SID, Core)))
type GotoTable       = M.HashMap NonTerminal SID
type ActionTable     = M.HashMap Terminal Action
type LALR1Table      = M.HashMap SID (Symbol, ActionTable, GotoTable)


instance Hashable Action

instance Eq Action where
    Shift  sid1 (p1, a1) == Shift  sid2 (p2, a2) = sid1 == sid2 && p1 == p2 && a1 == a2
    Reduce rid1 p1       == Reduce rid2 p2       = rid1 == rid2 && p1 == p2
    Accept               == Accept               = True
    Error                == Error                = True
    _                    == _                    = False

instance Hashable a => Hashable (S.Set a) where
    hashWithSalt s set = s `hashWithSalt` S.foldl' hashWithSalt 0 set


trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a





