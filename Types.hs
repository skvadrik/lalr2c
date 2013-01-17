module Types where


import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as S
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
    | Reduce RID
    | Accept
    | Error
    deriving (Show)

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


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h


instance Hashable Action where
    hash (Shift  sid) = 3 + 2 * hash sid
    hash (Reduce rid) = 2 + 2 * hash rid
    hash Accept       = 1
    hash Error        = 0


instance Eq Action where
    Shift  sid1 == Shift  sid2 = sid1 == sid2
    Reduce rid1 == Reduce rid2 = rid1 == rid2
    Accept      == Accept      = True
    Error       == Error       = True
    _           == _           = False


instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a





