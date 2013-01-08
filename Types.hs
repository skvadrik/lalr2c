module Types where


import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as S
import           Data.List                 (foldl')
import           Data.Hashable

import Debug.Trace


import Grammar


data Verbosity = V0 | V1
instance Eq Verbosity where
    V0 == V0 = True
    V1 == V1 = True
    _  == _  = False


data CmdOptions = CmdOpts
    { dest      :: FilePath
    , verbosity :: Verbosity
    }


data Action
    = Shift  SID
    | Reduce NonTerminal [Symbol]
    | Accept
    | Error
    deriving (Show)
type Core        = (NonTerminal, [Symbol], [Symbol])
type Context     = S.Set Terminal
type SID         = Int
type State       = M.HashMap Core Context
type StateTable  = M.HashMap SID (State, Symbol, M.HashMap Symbol SID)
type GotoTable   = M.HashMap NonTerminal SID
type ActionTable = M.HashMap Terminal Action
type LALRTable   = M.HashMap SID (Symbol, ActionTable, GotoTable)


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h


instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


instance (Hashable k, Hashable v) => Hashable (M.HashMap k v) where
    hash m = foldl' hashAndCombine 0 (M.toList m)


trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a





