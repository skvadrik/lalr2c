import           Data.List                (foldl')
import qualified Data.HashMap.Strict as M
import           System.Environment       (getArgs)
import           Control.Monad            (when)
import           System.Console.GetOpt

import           Types
import           LALR
import           Codegen


options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
    [ Option "d" ["debug"]  (NoArg  (\ opts -> opts{verbosity = V1})               ) "generate gebuggable output"
    , Option "o" ["output"] (ReqArg (\ f opts -> opts{dest = f})     "<c/cpp-file>") "destination file"
    , Option "h" ["header"] (ReqArg (\ f opts -> opts{hdr  = f})     "<h-file>"    ) "header file"
    ]


parse_args :: [String] -> IO (CmdOptions, [String], [String])
parse_args argv =
    let usage    = "Usage: ./lalr2c [OPTIONS]"
        def_opts = CmdOpts "1.cpp" "1.h" V0
    in  case getOpt' Permute options argv of
            (o, n, u, []  ) -> return (foldl' (flip id) def_opts o, n, u)
            (_, _, _, errs) -> error $ concat errs ++ usageInfo usage options


main :: IO ()
main = do
    ((CmdOpts fdest fhdr v), non_opts, unknown_opts) <- getArgs >>= parse_args
    when (non_opts /= []) $
        error $ "unparsed cmd arguments: " ++ unwords non_opts
    when (unknown_opts /= []) $
        error $ "unknown cmd-options: " ++ unwords unknown_opts

    codegen lalr1_table v fdest fhdr

    print $ M.size lalr1_table






