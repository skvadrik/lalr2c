#!/usr/bin/env runghc


import           Control.Applicative                 ((<$>))
import           Data.Hashable
import           System.Environment                  (getArgs)
import qualified Data.HashMap.Strict           as M
import qualified Data.Set                      as S
import           Data.List                           (foldl')
import           Data.Char                           (isAlphaNum)
import           Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint                    ((<>), ($$), Doc)


import           Debug.Trace


data Grammar = Grammar (S.Set Terminal) (S.Set NonTerminal) (M.HashMap NonTerminal (S.Set [Symbol])) NonTerminal deriving (Show)
data Terminal
    = TName   String
    | TString String
    deriving (Show, Ord, Eq)
newtype NonTerminal = NonTerminal String deriving (Show, Ord, Eq)
data Symbol
    = STerminal    Terminal
    | SNonTerminal NonTerminal
    deriving (Show, Ord, Eq)


instance Hashable NonTerminal where
    hash (NonTerminal n) = hash n


parse_grammar :: GenParser Char st Grammar
parse_grammar = do
    ts <- parse_terminals
    ax <- parse_axiom
    eol >> string "%%" >> eol
    rs <- parse_rules ts
    eof
    return $ to_grammar ts rs ax


parse_terminals :: GenParser Char st (S.Set Terminal)
parse_terminals = do
    tss <- many1 $ do
        try $ spaces >> string "%token"
        ts <- many1 $ try $ spaces1 >> TName <$> parse_name
        return $ S.fromList ts
    return $ S.unions tss



parse_axiom :: GenParser Char st NonTerminal
parse_axiom = do
    spaces >> string "%start"
    ax <- spaces1 >> parse_name
    return $ NonTerminal ax


parse_symbol :: S.Set Terminal -> GenParser Char st Symbol
parse_symbol ts =
    try $ do
        s <- parse_name
        return $ if S.member (TName s) ts
            then (STerminal . TName) s
            else (SNonTerminal . NonTerminal) s
    <|> between (char '\'') (char '\'') ((STerminal . TString) <$> many1 (noneOf "'"))


parse_rules :: S.Set Terminal -> GenParser Char st [(NonTerminal, S.Set [Symbol])]
parse_rules ts =
    many1 $ do
        spaces
        n  <- NonTerminal <$> parse_name
        spaces >> char ':' >> spaces
        xs <- sepBy1 (spaces >> many1 (parse_symbol ts >>= \ s -> spaces >> return s)) (char '|')
        spaces >> char ';' >> spaces
        return (n, S.fromList xs)


parse_name :: GenParser Char st String
parse_name = many1 (alphaNum <|> char '_')


spaces1 :: GenParser Char st ()
spaces1 = skipMany1 space


eol :: GenParser Char st ()
eol = skipMany1 $ char '\n'


to_grammar :: S.Set Terminal -> [(NonTerminal, S.Set [Symbol])] -> NonTerminal -> Grammar
to_grammar ts rs ax =
    let ns = (S.fromList . fst . unzip) rs
        insert_rule (ts, n2r) (n, r) =
            let check_symbol ts (SNonTerminal n)          = if S.member n ns then ts else error $ "unknown nonterminal: " ++ show n
                check_symbol ts (STerminal t@(TString _)) = S.insert t ts
                check_symbol ts _                         = ts
                ts'  = S.foldl' (\ ts1 ss -> foldl' check_symbol ts1 ss) ts r
                n2r' = M.insertWith (error $ "double rule for " ++ show n) n r n2r
            in  (ts', n2r')
        (ts', rs') = foldl' insert_rule (ts, M.empty) rs
        ts''       = S.insert (TName "lambda") ts'
    in  Grammar ts'' ns rs' ax


complement :: Grammar -> Grammar
complement (Grammar ts ns rs ax@(NonTerminal s)) =
    let f s = case s ++ "_" of
            s' | S.notMember (NonTerminal s') ns -> s'
            s'                         -> f s'
        ax' = NonTerminal (f s)
        ns' = S.insert ax' ns
        rs' = M.insert ax' (S.singleton [SNonTerminal ax]) rs
    in  Grammar ts ns' rs' ax'


terminal2name :: Terminal -> Doc
terminal2name (TName t)   = PP.text "T" <> PP.text t
terminal2name (TString t) =
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
            '='                          -> "CEq"
            '^'                          -> "CHat"
            '&'                          -> "CAmp"
            '%'                          -> "CPercent"
            c                            -> error $ "unknown symbol in terminal: " ++ [c]
    in  PP.text $ "T" ++ concatMap f t


nonterminal2name :: NonTerminal -> Doc
nonterminal2name (NonTerminal n) = PP.text $ "N" ++ n


symbol2name :: Symbol -> Doc
symbol2name (STerminal t)    = PP.text "T " <> terminal2name t
symbol2name (SNonTerminal n) = PP.text "N " <> nonterminal2name n


doc_rules :: M.HashMap NonTerminal (S.Set [Symbol]) -> Doc
doc_rules rs =
    let doc_alts =
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.text ",")
            . map
                (\ cs ->
                    ( PP.brackets
                    . PP.hcat
                    . PP.punctuate (PP.text ", ")
                    . map symbol2name
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
    $$ PP.text "terminals = S.fromList " <> (PP.brackets . PP.vcat . PP.punctuate (PP.text ",") . map terminal2name . S.toList) ts


doc_nonterminals :: S.Set NonTerminal -> Doc
doc_nonterminals ns =
    PP.text "nonterminals :: S.Set NonTerminal"
    $$ PP.text "nonterminals = S.fromList " <> (PP.brackets . PP.vcat . PP.punctuate (PP.text ",") . map nonterminal2name . S.toList) ns


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


gen_code :: Grammar -> String
gen_code (Grammar ts ns rs a) =
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
        _        -> error "usage: ./gen_code <grammar-file> <hs-file>"

    parseFromFile parse_grammar fg >>= \ g -> case g of
        Left e   -> print e
--        Right xs  -> print g
        Right g' -> writeFile fhs $ (gen_code . complement) g'



