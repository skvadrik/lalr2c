#!/usr/bin/env runghc

{-# LANGUAGE DeriveGeneric #-}

import           Control.Applicative                 ((<$>))
import           Data.Hashable
import           System.Environment                  (getArgs)
import qualified Data.HashMap.Strict           as M
import qualified Data.Set                      as S
import           Data.List                           (foldl')
import           Data.Char                           (isAlphaNum)
import           GHC.Generics                        (Generic)
import           Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint                    ((<>), ($$), Doc)

import           Debug.Trace


data Grammar
    = Grammar
        (S.Set Terminal)
        (S.Set NonTerminal)
        [(Int, NonTerminal, [Symbol], Code)]
        NonTerminal
    deriving (Show)
data Terminal
    = TString String
    | TNew
    | TLambda
    deriving (Show, Ord, Eq, Generic)
newtype NonTerminal
    = NT String
    deriving (Show, Ord, Eq, Generic)
data Symbol
    = STerminal    Terminal
    | SNonTerminal NonTerminal
    deriving (Show, Ord, Eq, Generic)
type Code = String


instance Hashable Terminal

instance Hashable NonTerminal

instance Hashable Symbol


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
        try $ wsps >> string "%token"
        ts <- many1 $ try $ wsps1 >> TString <$> parse_name
        return $ S.fromList ts
    return $ S.unions tss



parse_axiom :: GenParser Char st NonTerminal
parse_axiom = do
    wsps >> string "%start"
    ax <- wsps1 >> parse_name
    return $ NT ax


parse_symbol :: S.Set Terminal -> GenParser Char st Symbol
parse_symbol ts = do
    s <- parse_name
    return $ case s of
        _ | S.member (TString s) ts -> (STerminal . TString) s
        _                           -> (SNonTerminal . NT) s


parse_code :: GenParser Char st Code
parse_code = do
    char '{'
    code1 <- many (noneOf "{}")
    code2 <- parse_code <|> return ""
    code3 <- many (noneOf "{}")
    char '}'
    return $ code1 ++ code2 ++ code3



parse_rules :: S.Set Terminal -> GenParser Char st [(NonTerminal, M.HashMap [Symbol] Code)]
parse_rules ts =
    many1 $ do
        wsps
        n  <- NT <$> parse_name
        wsps >> char ':' >> wsps
        xs <- sepBy1
            ( do
                wsps
                ss <- do
                    ss1 <- many (parse_symbol ts >>= \ s -> wsps >> return s)
                    case ss1 of
                        [] -> return [STerminal TLambda]
                        _  -> return ss1
                wsps
                c  <- parse_code <|> return ""
                wsps
                return (ss, c)
            ) (char '|')
        wsps >> char ';' >> wsps
        let m = foldl' (\ m (ss, c) -> M.insertWith (error "duplicate rule") ss c m) M.empty xs
        return (n, m)


parse_name :: GenParser Char st String
parse_name = many1 (alphaNum <|> char '_')


wsps :: GenParser Char st ()
wsps = skipMany wsp


wsps1 :: GenParser Char st ()
wsps1 = skipMany1 wsp


wsp :: GenParser Char st ()
wsp = try comment <|> (space >> return ())


comment :: GenParser Char st ()
comment = do
    string "/*"
    manyTill anyChar (try (string "*/"))
    return ()


eol :: GenParser Char st ()
eol = skipMany1 $ char '\n'


to_grammar :: S.Set Terminal -> [(NonTerminal, M.HashMap [Symbol] Code)] -> NonTerminal -> Grammar
to_grammar ts rs ax =
    let ns = (S.fromList . fst . unzip) rs

        add_rules (rs, max) (n, r) =
            let check_symbol s = case s of
                    SNonTerminal n -> if S.member n ns
                        then s
                        else error $ "unknown nonterminal: " ++ show n
                    STerminal    _ -> s

                add_rule (rs, max) ss c = ((max, n, ss, c) : rs, max + 1)

            in  ( M.foldlWithKey' add_rule (rs, max)
                . M.foldlWithKey' (\ m ss c -> M.insert (map check_symbol ss) c m) M.empty
                ) r

        rs' = fst $ foldl' add_rules ([], 1) rs
        ts' = (S.insert TLambda . S.insert TNew) ts
    in  Grammar ts' ns rs' ax


complement :: Grammar -> Grammar
complement (Grammar ts ns rs ax@(NT s)) =
    let f s = case s ++ "_" of
            s' | S.notMember (NT s') ns -> s'
            s'                          -> f s'
        ax' = NT (f s)
        ns' = S.insert ax' ns
        rs' = (0, ax', [SNonTerminal ax], "") : rs
    in  Grammar ts ns' rs' ax'


terminal2name :: Terminal -> Doc
terminal2name (TString t) = PP.text "T" <> PP.text t
terminal2name TNew        = PP.text "New"
terminal2name TLambda     = PP.text "Lambda"


nonterminal2name :: NonTerminal -> Doc
nonterminal2name (NT n) = PP.text $ "N" ++ n


symbol2name :: Symbol -> Doc
symbol2name (STerminal t)    = PP.text "T " <> terminal2name t
symbol2name (SNonTerminal n) = PP.text "N " <> nonterminal2name n


doc_rhs :: [Symbol] -> Doc
doc_rhs =
    ( PP.brackets
    . PP.hcat
    . PP.punctuate (PP.text ", ")
    . map symbol2name
    )


doc_rules :: [(Int, NonTerminal, [Symbol], Code)] -> Doc
doc_rules rs =
    let doc_one (i, n, ss, c) = PP.parens
            ( PP.int i
            <> PP.text ", "
            <> nonterminal2name n
            <> PP.text ", "
            <> doc_rhs ss
            <> PP.text ", "
            <> (PP.doubleQuotes . PP.braces . PP.text) c
            )
        rules =
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.char ',')
            . map doc_one
            ) rs
    in  PP.text "rules :: [(RID, NonTerminal, [Symbol], Code)]"
        $$ PP.text "rules ="
        $$ PP.nest 4 rules


doc_rid2rule :: [(Int, NonTerminal, [Symbol], Code)] -> Doc
doc_rid2rule rs =
    let doc_one (i, _, ss, _) =
            PP.text "rid2rule "
            <> PP.int i
            <> PP.text " = "
            <> doc_rhs ss
    in  PP.text "rid2rule :: RID -> [Symbol]"
        $$ (PP.vcat . map doc_one) rs
        $$ PP.text "rid2rule rid = error $ \"RID exceeding maximum possible value: \" ++ show rid"


doc_n2rules :: [(Int, NonTerminal, [Symbol], Code)] -> Doc
doc_n2rules rs =
    let n2rules = foldl' (\ n2rs (_, n, ss, _) -> M.insertWith (++) n [ss] n2rs) M.empty rs
        doc_alts =
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.char ',')
            . map doc_rhs
            )
        doc_one d n alts = d
            $$ PP.text "n2rules "
            <> nonterminal2name n
            <> PP.text " = "
            <> doc_alts alts
    in  PP.text "n2rules :: NonTerminal -> [[Symbol]]"
        $$ M.foldlWithKey' doc_one PP.empty n2rules


doc_n2rids :: [(Int, NonTerminal, [Symbol], Code)] -> Doc
doc_n2rids rs =
    let n2rids = foldl' (\ n2rs (i, n, _, _) -> M.insertWith (++) n [i] n2rs) M.empty rs
        doc_alts =
            ( PP.brackets
            . PP.hcat
            . PP.punctuate (PP.char ',')
            . map PP.int
            )
        doc_one d n alts = d
            $$ PP.text "n2rids "
            <> nonterminal2name n
            <> PP.text " = "
            <> doc_alts alts
    in  PP.text "n2rids :: NonTerminal -> [RID]"
        $$ M.foldlWithKey' doc_one PP.empty n2rids


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
    let doc_rnf f =
            ( PP.vcat
            . map (\ (x, k) -> PP.text "rnf " <> f x <> PP.text " = rnf (" <> PP.int k <> PP.text " :: Int)")
            . (\ xs -> zip xs [1 .. length xs])
            . S.toList
            )
        doc_rnf_symbols =
            PP.text "rnf (T t) = rnf t"
            $$ PP.text "rnf (N n) = rnf n"
    in  PP.text "instance Hashable Terminal"
        $$$ PP.text "instance Hashable NonTerminal"
        $$$ PP.text "instance Hashable Symbol"
        $$$ PP.text "instance NFData Terminal where"      $$ PP.nest 4 (doc_rnf terminal2name ts)
        $$$ PP.text "instance NFData NonTerminal where"   $$ PP.nest 4 (doc_rnf nonterminal2name ns)
        $$$ PP.text "instance NFData Symbol where"        $$ PP.nest 4 doc_rnf_symbols


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


gen_code :: Grammar -> String
gen_code (Grammar ts ns rs a) =
    let doc_ts = (PP.hcat . PP.punctuate (PP.text " | ") . map terminal2name . S.toList) ts
        doc_ns = (PP.hcat . PP.punctuate (PP.text " | ") . map nonterminal2name . S.toList) ns
    in  PP.render $
            PP.text "{-# LANGUAGE DeriveGeneric #-}"
            $$$ PP.text "module Grammar where"
            $$$ PP.text "import qualified Data.Set      as S"
            $$ PP.text "import           Data.Hashable"
            $$ PP.text "import           Control.DeepSeq"
            $$ PP.text "import           GHC.Generics        (Generic)"
            $$$ PP.text "type RID         = Int"
            $$$ PP.text "type Code        = String"
            $$ PP.text "data Terminal    = " <> doc_ts <> PP.text " deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data NonTerminal = " <> doc_ns <> PP.text " deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data Symbol      = T Terminal | N NonTerminal deriving (Show, Eq, Ord, Generic)"
            $$$ doc_terminals ts
            $$$ doc_nonterminals ns
            $$$ doc_symbols
            $$$ doc_rules rs
            $$$ doc_rid2rule rs
            $$$ doc_n2rules rs
            $$$ doc_n2rids rs
            $$$ doc_axiom a
            $$$ doc_instances ts ns


main :: IO ()
main = do
    (fg, fhs) <- getArgs >>= \ args -> case args of
        [f1, f2] -> return (f1, f2)
        _        -> error "usage: ./gen_code <grammar-file> <hs-file>"

    parseFromFile parse_grammar fg >>= \ g -> case g of
        Left e   -> print e
        Right g' -> writeFile fhs $ (gen_code . complement) g'



