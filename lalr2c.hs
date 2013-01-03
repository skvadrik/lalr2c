import Control.Monad (mapM_)
import Data.List (foldl')
import qualified Data.Set as S
import System.Environment (getArgs)
import Control.Monad (when)
import System.Console.GetOpt

import Types
import LALR
import Codegen


options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
    [ Option "d" ["debug"]  (NoArg  (\ opts -> opts{verbosity = V1})                ) "generate gebuggable output"
    , Option "o" ["output"] (ReqArg (\ f opts -> opts{dest    = f})   "<c/cpp-file>") "destination file"
    ]


parse_args :: [String] -> IO (CmdOptions, [String], [String])
parse_args argv =
    let usage    = "Usage: ./lalr2c [OPTIONS]"
        def_opts = CmdOpts "1.cpp" V0
    in  case getOpt' Permute options argv of
            (o, n, u, []  ) -> return (foldl' (flip id) def_opts o, n, u)
            (_, _, _, errs) -> error $ concat errs ++ usageInfo usage options


main :: IO ()
main = do
    ((CmdOpts fdest v), non_opts, unknown_opts) <- getArgs >>= parse_args
    when (non_opts /= []) $
        error $ "unparsed cmd arguments: " ++ unwords non_opts
    when (unknown_opts /= []) $
        error $ "unknown cmd-options: " ++ unwords unknown_opts

    let tbl  = lalr1_table
        code = codegen tbl v

    mapM_ print (S.toList tbl)
    writeFile fdest code



