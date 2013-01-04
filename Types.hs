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


type Core        = (NonTerminal, [Symbol], [Symbol])
type Context     = S.Set Terminal
type State       = M.HashMap Core Context
data Action
    = Shift
    | Reduce NonTerminal [Symbol]
    | Accept
    | Error
    deriving (Show)
type StateTable  = M.HashMap State (M.HashMap Symbol State)
type GotoTable   = M.HashMap NonTerminal State
type ActionTable = M.HashMap Terminal Action
type LALRTable   = M.HashMap State (ActionTable, GotoTable)


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h


instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


instance (Hashable k, Hashable v) => Hashable (M.HashMap k v) where
    hash m = foldl' hashAndCombine 0 (M.toList m)


instance (Eq a, Eq b, Ord a, Ord b) => Ord (M.HashMap a b) where
    compare m1 m2 | M.size m1 < M.size m2 = LT
    compare m1 m2 | M.size m1 > M.size m2 = GT
    compare m1 m2 = compare (M.toList m1) (M.toList m2)


trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a





