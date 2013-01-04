#!/usr/bin/env runghc

import           Control.Applicative        ((<$>))
import           Control.DeepSeq
import qualified Data.Set             as S
import qualified Data.HashMap.Strict  as M
import           Data.Char                  (isSpace, isAlphaNum)
import           Data.List                  (foldl', delete, isPrefixOf)
import           Data.Hashable
import           Debug.Trace
import qualified Text.PrettyPrint     as PP
import           Text.PrettyPrint           ((<>), ($$), Doc)
import           System.Environment         (getArgs)


trace' :: Show a => a -> a
trace' a = trace (show a) a


data Grammar     = G (S.Set Terminal) (S.Set NonTerminal) (M.HashMap NonTerminal (S.Set [Symbol])) NonTerminal deriving (Show)
data Terminal    = T String deriving (Show, Ord, Eq)
data NonTerminal = N String deriving (Show, Ord, Eq)
data Symbol      = ST Terminal | SN NonTerminal deriving (Show, Ord, Eq)


instance Hashable NonTerminal where
    hash (N n) = hash n


check_name :: String -> String
check_name s = if filter (\ c -> isAlphaNum c || c == '_') s == s
    then s
    else error "bad name"


split_at :: Char -> String -> [String]
split_at _ "" = []
split_at c  s =
    let (s1, s2) = break (== c) s
    in  case s2 of
            ""                                         -> [s1]
            x : s3 | x == c && filter isSpace s3 == s3 -> [s1]
            x : s3 | x == c                            -> s1 : split_at c s3
            _                                          -> error "error while spliting"


parse_grammar :: String -> Grammar
parse_grammar s =
    let ss = split_at ';' s
    in  case ss of
            [v, w, r, a] ->
                let terminals        = parse_terminals    v
                    nonterminals     = parse_nonterminals w
                    rules            = parse_rules        terminals nonterminals r
                    axiom            = parse_axiom        nonterminals a
                in  G terminals nonterminals rules axiom
            _            -> error "bad grammar file"


parse_terminals :: String -> S.Set Terminal
parse_terminals =
    ( S.fromList
    . map T
    . split_at ','
    . init
    . tail
    . filter (not . isSpace)
    )


parse_nonterminals :: String -> S.Set NonTerminal
parse_nonterminals =
    ( S.fromList
    . map (N . check_name)
    . split_at ','
    . init
    . tail
    . filter (not . isSpace)
    )


parse_rules :: S.Set Terminal -> S.Set NonTerminal -> String -> M.HashMap NonTerminal (S.Set [Symbol])
parse_rules ts ns =
    ( foldl' (\ m (n, r) -> M.insertWith S.union n r m) M.empty
    . map (parse_rule ts ns)
    . split_at ','
    . takeWhile (/= '}')
    . tail
    . dropWhile (/= '{')
    )


parse_rule :: S.Set Terminal -> S.Set NonTerminal -> String -> (NonTerminal, S.Set [Symbol])
parse_rule ts ns s =
    let to_symbol w = case w of
            _ | S.member (T w) ts -> (ST . T) w
            _ | S.member (N w) ns -> (SN . N) w
            _                     -> error $ "bad symbol" ++ show w
        to_nonterminal w = case w of
            _ | S.member (N w) ns -> N w
            _                     -> error "bad nonterminal"
        (s1, s2) = break (== '=') s
        nt = case words s1 of
            [w] -> to_nonterminal w
            _   -> error "bad rule lhs"
        rs =
            ( S.fromList
            . map
                ( filter (/= ST (T "lambda"))
                . map to_symbol
                . words
                )
            . split_at '|'
            . tail
            ) s2
    in  (nt, rs)


parse_axiom :: S.Set NonTerminal -> String -> NonTerminal
parse_axiom ns =
    ( (\ w -> if S.member w ns then w else error "bad nonterminal")
    . N
    . check_name
    . filter (not . isSpace)
    )


unify_grammar :: Grammar -> Grammar
unify_grammar =
    ( complement
    . exclude_unreachable
    . exclude_useless
    )


complement :: Grammar -> Grammar
complement (G ts ns rs ax@(N s)) =
    let f s = case s ++ "_" of
            s' | S.notMember (N s') ns -> s'
            s'                         -> f s'
        ax' = N (f s)
        ns' = S.insert ax' ns
        rs' = M.insert ax' (S.singleton [SN ax]) rs
    in  G ts ns' rs' ax'


exclude_useless :: Grammar -> Grammar
exclude_useless g = g


exclude_unreachable :: Grammar -> Grammar
exclude_unreachable g = g


terminal2name :: Terminal -> Doc
terminal2name (T t) =
    let f c = case c of
            _ | isAlphaNum c || c == '_' -> [c]
            '+'                          -> "Plus"
            '-'                          -> "Minus"
            '*'                          -> "Star"
            '/'                          -> "Slash"
            '\\'                         -> "Backslash"
            '|'                          -> "Vslash"
            '<'                          -> "OAngle"
            '>'                          -> "CAngle"
            '.'                          -> "Dot"
            ','                          -> "Comma"
            ';'                          -> "Semi"
            ':'                          -> "Colon"
            '?'                          -> "Question"
            '!'                          -> "Exclamation"
            '~'                          -> "Tilde"
            '('                          -> "OBrace"
            ')'                          -> "CBrace"
            '{'                          -> "OParens"
            '}'                          -> "CParens"
            '['                          -> "OBracket"
            ']'                          -> "CBracket"
            _                            -> error "unknown symbol in terminal"
    in  PP.text $ "T" ++ concatMap f t


nonterminal2name :: NonTerminal -> Doc
nonterminal2name (N n) = PP.text $ "N" ++ n


doc_rules :: M.HashMap NonTerminal (S.Set [Symbol]) -> Doc
doc_rules rs =
    let doc_alts =
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.text ", ")
            . map
                (\ cs ->
                    ( PP.brackets
                    . PP.hcat
                    . PP.punctuate (PP.text ", ")
                    . map
                        (\ c -> case c of
                            SN n            -> PP.text "N " <> nonterminal2name n
                            ST t            -> PP.text "T " <> terminal2name t
                        )
                    ) cs
                )
            . S.toList
            )
        doc_r (n, alts) = PP.text "rules " <> nonterminal2name n <> PP.text " = " <> doc_alts alts
        d1 = PP.text "rules :: Rule"
        d2 = (PP.vcat . map doc_r . M.toList) rs
    in  d1 $$ d2


doc_axiom :: NonTerminal -> Doc
doc_axiom n = PP.text "axiom :: NonTerminal" $$ PP.text "axiom = " <> nonterminal2name n


doc_terminals :: S.Set Terminal -> Doc
doc_terminals ts =
    PP.text "terminals :: S.Set Terminal"
    $$ PP.text "terminals = S.fromList " <> (PP.brackets . PP.vcat . PP.punctuate (PP.text ", ") . map terminal2name . S.toList) ts


doc_nonterminals :: S.Set NonTerminal -> Doc
doc_nonterminals ns =
    PP.text "nonterminals :: S.Set NonTerminal"
    $$ PP.text "nonterminals = S.fromList " <> (PP.brackets . PP.vcat . PP.punctuate (PP.text ", ") . map nonterminal2name . S.toList) ns


doc_symbols :: Doc
doc_symbols =
    PP.text "symbols :: S.Set Symbol"
    $$ PP.text "symbols = S.map T  terminals `S.union` S.map N nonterminals"


doc_instances :: S.Set Terminal -> S.Set NonTerminal -> Doc
doc_instances ts ns =
    let doc_hash_ts =
            ( PP.vcat
            . map (\ (t, k) -> PP.text "hash " <> terminal2name t <> PP.text " = " <> PP.int k)
            . (\ ts -> zip ts [1 .. length ts])
            . S.toList
            ) ts
        doc_hash_ns =
            ( PP.vcat
            . map (\ (n, k) -> PP.text "hash " <> nonterminal2name n <> PP.text " = " <> PP.int k)
            . (\ ns -> zip ns [1 .. length ns])
            . S.toList
            ) ns
        doc_hash_ss =
            PP.text "hash (T t) = 2 * hash t"
            $$ PP.text "hash (N n) = 2 * hash n + 1"
        doc_rnf_ts =
            ( PP.vcat
            . map (\ (t, k) -> PP.text "rnf " <> terminal2name t <> PP.text " = rnf (" <> PP.int k <> PP.text " :: Int)")
            . (\ ts -> zip ts [1 .. length ts])
            . S.toList
            ) ts
        doc_rnf_ns =
            ( PP.vcat
            . map (\ (n, k) -> PP.text "rnf " <> nonterminal2name n <> PP.text " = rnf (" <> PP.int k <> PP.text " :: Int)")
            . (\ ns -> zip ns [1 .. length ns])
            . S.toList
            ) ns
        doc_rnf_ss =
            PP.text "rnf (T t) = rnf t"
            $$ PP.text "rnf (N n) = rnf n"
        d1 = PP.text "instance Hashable Terminal where"    $$ PP.nest 4 doc_hash_ts
        d2 = PP.text "instance Hashable NonTerminal where" $$ PP.nest 4 doc_hash_ns
        d3 = PP.text "instance Hashable Symbol where"      $$ PP.nest 4 doc_hash_ss
        d4 = PP.text "instance NFData Terminal where"    $$ PP.nest 4 doc_rnf_ts
        d5 = PP.text "instance NFData NonTerminal where" $$ PP.nest 4 doc_rnf_ns
        d6 = PP.text "instance NFData Symbol where"      $$ PP.nest 4 doc_rnf_ss
    in  d1 $$$ d2 $$$ d3 $$$ d4 $$$ d5 $$$ d6


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


g2hs :: Grammar -> String
g2hs (G ts ns rs a) =
    let doc_ts = (PP.hcat . PP.punctuate (PP.text " | ") . map terminal2name . S.toList) ts
        doc_ns = (PP.hcat . PP.punctuate (PP.text " | ") . map nonterminal2name . S.toList) ns
        d0 = PP.text "module Grammar where"
            $$$ PP.text "import qualified Data.Set      as S"
            $$ PP.text "import           Data.Hashable"
            $$ PP.text "import           Control.DeepSeq"
        d1 = PP.text "data Terminal    = " <> doc_ts <> PP.text " deriving (Show, Eq, Ord)"
        d2 = PP.text "data NonTerminal = " <> doc_ns <> PP.text " deriving (Show, Eq, Ord)"
        d3 = PP.text "data Symbol      = T Terminal | N NonTerminal deriving (Show, Eq, Ord)"
        d4 = PP.text "type Rule        = NonTerminal -> [[Symbol]]"
        d5 = doc_terminals ts
        d6 = doc_nonterminals ns
        d7 = doc_symbols
        d8 = doc_rules rs
        d9 = doc_axiom a
        d10 = doc_instances ts ns
        doc = d0 $$$ d1 $$ d2 $$ d3 $$ d4 $$$ d5 $$$ d6 $$$ d7 $$$ d8 $$$ d9 $$$ d10
    in  PP.render doc


main :: IO ()
main = do
    (fg, fhs) <- getArgs >>= \ args -> case args of
        [f1, f2] -> return (f1, f2)
        _        -> error "usage: ./g2hs <grammar-file> <hs-file>"
    code <- g2hs . unify_grammar . parse_grammar <$> readFile fg
    writeFile fhs code

