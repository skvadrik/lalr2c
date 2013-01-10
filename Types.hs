module Types where


import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as S
import           Data.List                 (foldl')
import           Data.Hashable
import           Debug.Trace

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
    = Shift  SID
    | Reduce NonTerminal [Symbol]
    | Accept
    | Error
    deriving (Show)

type SID         = Int
type RID         = Int
type Core        = (NonTerminal, [Symbol], [Symbol])
type Context     = S.Set Terminal
type State       = M.HashMap Core Context
type StateTable  = M.HashMap SID (State, Symbol, M.HashMap Symbol SID)
type GotoTable   = M.HashMap NonTerminal SID
type ActionTable = M.HashMap Terminal Action
type LALRTable   = M.HashMap SID (Symbol, ActionTable, GotoTable)


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h


instance Hashable Action where
    hash (Shift sid)   = 2 * hash sid + 3
    hash (Reduce n ss) = 2 * hash ss * hash n + 2
    hash Accept        = 1
    hash Error         = 0


instance Eq Action where
    Shift sid1    == Shift sid2    = sid1 == sid2
    Reduce n1 ss1 == Reduce n2 ss2 = n1 == n2 && ss1 == ss2
    Accept        == Accept        = True
    Error         == Error         = True
    _             == _             = False


instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


instance (Hashable k, Hashable v) => Hashable (M.HashMap k v) where
    hash m = foldl' hashAndCombine 0 (M.toList m)


trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a





