#!/usr/bin/env runghc

{-# LANGUAGE DeriveGeneric #-}

import           Control.Applicative                        ((<$>))
import           Data.Hashable
import           System.Environment                         (getArgs)
import qualified Data.HashMap.Strict           as M  hiding (lookupDefault)
import qualified Data.HashMap.Lazy             as M         (lookupDefault)
import qualified Data.Set                      as S
import           Data.List                                  (foldl')
import           Data.Char                                  (isAlphaNum)
import           GHC.Generics                               (Generic)
import           Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint                           ((<>), ($$), Doc)

import           Debug.Trace


data Grammar
    = Grammar
        (M.HashMap Terminal (Prec, Assoc))
        (S.Set NonTerminal)
        [(Int, NonTerminal, [Symbol], Prec, Maybe Code)]
        NonTerminal
        Terminal
        (Maybe Code)
        (M.HashMap Terminal Code)
        Bool
    deriving (Show)

data Terminal
    = TString String
    | TNew
    | TLambda String
    deriving (Show, Ord, Eq, Generic)

newtype NonTerminal
    = NT String
    deriving (Show, Ord, Eq, Generic)

data Symbol
    = STerminal    Terminal
    | SNonTerminal NonTerminal
    deriving (Show, Ord, Eq, Generic)

type Code = String

data Prec
    = PrecLevel Int
    | PrecNone
    deriving (Show)

data Assoc
    = AssocLeft
    | AssocRight
    | AssocNone
    deriving (Show)

data Directive
    = DirToken     (S.Set Terminal)
    | DirLeft      (S.Set Terminal)
    | DirRight     (S.Set Terminal)
    | DirNonassoc  (S.Set Terminal)
    | DirStart     NonTerminal
    | DirStop      String
    | DirFail      Code
    | DirPhony     (Terminal, Code)
    | DirReentrant
    deriving (Show)


instance Hashable Terminal

instance Hashable NonTerminal

instance Hashable Symbol


parse_source :: GenParser Char st (Maybe Code, Grammar, Maybe Code)
parse_source = do
    entry   <- Just <$> try parse_codeblock <|> return Nothing
    grammar <- parse_grammar
    ending  <- Just <$> try parse_codeblock <|> return Nothing
    wsps >> eof
    return (entry, grammar, ending)


parse_codeblock :: GenParser Char st Code
parse_codeblock = do
    wsps
    string "%{"
    manyTill anyChar (try (string "%}"))


parse_grammar :: GenParser Char st Grammar
parse_grammar = do
    (terminals, axiom, lambda, failure, phony, reentrant) <- parse_header
    sep
    rules <- parse_body terminals lambda
    return $ normalize_grammar terminals rules axiom lambda failure phony reentrant


parse_header :: GenParser Char st (M.HashMap Terminal (Prec, Assoc), Maybe NonTerminal, Terminal, Maybe Code, M.HashMap Terminal Code, Bool)
parse_header = normalize_directives <$> many (try $ wsps >> parse_directive)


normalize_directives :: [Directive] -> (M.HashMap Terminal (Prec, Assoc), Maybe NonTerminal, Terminal, Maybe Code, M.HashMap Terminal Code, Bool)
normalize_directives dirs =
    let f (pl, ts, ax, lm, fl, ph, b) dir = case dir of
            DirToken      xs -> (pl, S.foldl' (\ m t -> M.insert t (PrecNone, AssocNone)  m) ts xs, ax, lm, fl, ph, b)
            DirLeft       xs -> (pl + 1, S.foldl' (\ m t -> M.insert t (PrecLevel pl, AssocLeft)  m) ts xs, ax, lm, fl, ph, b)
            DirRight      xs -> (pl + 1, S.foldl' (\ m t -> M.insert t (PrecLevel pl, AssocRight) m) ts xs, ax, lm, fl, ph, b)
            DirNonassoc   xs -> (pl + 1, S.foldl' (\ m t -> M.insert t (PrecLevel pl, AssocNone)  m) ts xs, ax, lm, fl, ph, b)
            DirStart       s -> (pl, ts, Just s, lm, fl, ph, b)
            DirStop        s -> (pl, ts, ax, TLambda s, fl, ph, b)
            DirFail        c -> (pl, ts, ax, lm, Just c, ph, b)
            DirPhony  (t, c) -> (pl, ts, ax, lm, fl, M.insert t c ph, b)
            DirReentrant     -> (pl, ts, ax, lm, fl, ph, True)
        ts = M.insert TNew (PrecNone, AssocNone) M.empty
        defaults = (0, ts, Nothing, TLambda "LAMBDA", Nothing, M.empty, False)
        (_, terminals, axiom, lambda, failure, phony, reentrant) = foldl' f defaults dirs
        terminals' = M.insert lambda (PrecNone, AssocNone) terminals
    in  (terminals', axiom, lambda, failure, phony, reentrant)


parse_directive :: GenParser Char st Directive
parse_directive =
    try (DirToken <$> parse_dir_token)
    <|> try (DirLeft <$> parse_dir_left)
    <|> try (DirRight <$> parse_dir_right)
    <|> try (DirNonassoc <$> parse_dir_nonassoc)
    <|> try (DirStart <$> parse_dir_start)
    <|> try (DirStop <$> parse_dir_stop)
    <|> try (DirFail <$> parse_dir_fail)
    <|> try (DirPhony <$> parse_dir_phony)
    <|> (parse_dir_reentrant >> return DirReentrant)


parse_dir_token :: GenParser Char st (S.Set Terminal)
parse_dir_token = do
    string "%token"
    parse_terminals


parse_dir_left :: GenParser Char st (S.Set Terminal)
parse_dir_left = do
    string "%left"
    parse_terminals


parse_dir_right :: GenParser Char st (S.Set Terminal)
parse_dir_right = do
    string "%right"
    parse_terminals


parse_dir_nonassoc :: GenParser Char st (S.Set Terminal)
parse_dir_nonassoc = do
    string "%nonassoc"
    parse_terminals


parse_dir_start :: GenParser Char st NonTerminal
parse_dir_start = do
    string "%start"
    wsps1
    parse_nonterminal


parse_dir_stop :: GenParser Char st String
parse_dir_stop = do
    string "%stop"
    wsps1
    parse_name


parse_dir_fail :: GenParser Char st Code
parse_dir_fail = do
    string "%fail"
    wsps1
    parse_code


parse_dir_phony :: GenParser Char st (Terminal, Code)
parse_dir_phony = do
    string "%phony"
    wsps1
    t <- parse_terminal
    wsps
    c <- parse_code
    return (t, c)


parse_dir_reentrant :: GenParser Char st ()
parse_dir_reentrant = do
    string "%reentrant"
    return ()


parse_terminals :: GenParser Char st (S.Set Terminal)
parse_terminals = do
    ts <- many1 (try $ wsps1 >> parse_terminal)
    return $ S.fromList ts


parse_terminal :: GenParser Char st Terminal
parse_terminal = do
    TString <$> parse_name


parse_nonterminal :: GenParser Char st NonTerminal
parse_nonterminal = do
    wsps >> NT <$> parse_name


sep :: GenParser Char st ()
sep = wsps >> string "%%" >> return ()


rule_start :: GenParser Char st ()
rule_start = wsps >> char ':' >> return ()


rule_sep :: GenParser Char st ()
rule_sep = char '|' >> return ()


rule_end :: GenParser Char st ()
rule_end =  wsps >> char ';' >> return ()


parse_body :: M.HashMap Terminal (Prec, Assoc) -> Terminal -> GenParser Char st [(NonTerminal, M.HashMap [Symbol] (Prec, Maybe Code))]
parse_body ts lm = many1 (parse_rule ts lm)


parse_rule :: M.HashMap Terminal (Prec, Assoc) -> Terminal -> GenParser Char st (NonTerminal, M.HashMap [Symbol] (Prec, Maybe Code))
parse_rule ts lm = do
    n  <- parse_nonterminal
    rule_start
    rs <- sepBy (parse_rule_rhs ts lm) rule_sep
    rule_end
    wsps
    return (n, M.fromList rs)


parse_rule_rhs :: M.HashMap Terminal (Prec, Assoc) -> Terminal -> GenParser Char st ([Symbol], (Prec, Maybe Code))
parse_rule_rhs ts lm = do
    symbols <- parse_symbols (M.keys ts) lm
    prec    <- try (parse_precedence ts) <|> return (default_precedence ts symbols)
    code    <- Just <$> try (wsps >> parse_code) <|> return Nothing
    wsps
    return (symbols, (prec, code))


parse_symbols :: [Terminal] -> Terminal -> GenParser Char st [Symbol]
parse_symbols ts lm = do
    ss <- many (try $ wsps >> parse_name)
    return $ case ss of
        [] -> []
        _  -> map (to_symbol ts) ss


to_symbol :: [Terminal] -> String -> Symbol
to_symbol ts s = if elem (TString s) ts
    then (STerminal . TString) s
    else (SNonTerminal . NT) s


parse_precedence :: M.HashMap Terminal (Prec, Assoc) -> GenParser Char st Prec
parse_precedence ts = do
    wsps1 >> string "%prec" >> wsps1
    t <- parse_terminal
    return $ fst $ M.lookupDefault (error "illegal terminal in %prec directive") t ts


default_precedence :: M.HashMap Terminal (Prec, Assoc) -> [Symbol] -> Prec
default_precedence ts ss =
    let f []                    = PrecNone
        f (STerminal    t : ss) = fst $ M.lookupDefault (error $ "unknown terminal in symbol chain" ++ show t) t ts
        f (SNonTerminal n : ss) = f ss
    in  f $ reverse ss


parse_code :: GenParser Char st Code
parse_code = do
    char '{'
    code1 <- many (noneOf "{}")
    code2 <- parse_code <|> return ""
    code3 <- many (noneOf "{}")
    char '}'
    return $ "{" ++ code1 ++ code2 ++ code3 ++ "}"


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


normalize_grammar
    :: M.HashMap Terminal (Prec, Assoc)
    -> [(NonTerminal, M.HashMap [Symbol] (Prec, Maybe Code))]
    -> Maybe NonTerminal
    -> Terminal
    -> Maybe Code
    -> M.HashMap Terminal Code
    -> Bool
    -> Grammar
normalize_grammar ts rs ax lm fl ph r =
    let ns = (fst . unzip) rs
        ns' = (S.fromList . fst . unzip) rs
        ax' = case ax of
            Just a  -> a
            Nothing -> head ns
        add_rules (rs, max) (n, r) =
            let check_symbol s = case s of
                    SNonTerminal n -> if S.member n ns'
                        then s
                        else error $ "unknown nonterminal: " ++ show n
                    STerminal    _ -> s

                add_rule (rs, max) ss (p, c) = ((max, n, ss, p, c) : rs, max + 1)

            in  ( M.foldlWithKey' add_rule (rs, max)
                . M.foldlWithKey' (\ m ss c -> M.insert (map check_symbol ss) c m) M.empty
                ) r
        rs' = fst $ foldl' add_rules ([], 1) rs
    in  Grammar ts ns' rs' ax' lm fl ph r


complement :: Grammar -> Grammar
complement (Grammar ts ns rs ax@(NT s) lm fl ph r) =
    let f s = case s ++ "_" of
            s' | S.notMember (NT s') ns -> s'
            s'                          -> f s'
        ax' = NT (f s)
        ns' = S.insert ax' ns
        rs' = (0, ax', [SNonTerminal ax], PrecNone, Nothing) : rs
    in  Grammar ts ns' rs' ax' lm fl ph r


terminal2name :: Terminal -> Doc
terminal2name (TString t) = PP.text "T" <> PP.text t
terminal2name TNew        = PP.text "New"
terminal2name (TLambda t) = PP.text "T" <> PP.text t


nonterminal2name :: NonTerminal -> Doc
nonterminal2name (NT n) = PP.text $ "N" ++ n


symbol2name :: Symbol -> Doc
symbol2name (STerminal t)    = PP.text "T " <> terminal2name t
symbol2name (SNonTerminal n) = PP.text "N " <> nonterminal2name n


doc_prec :: Prec -> Doc
doc_prec (PrecLevel l) = PP.text "PrecLevel " <> PP.int l
doc_prec PrecNone      = PP.text "PrecNone"


doc_assoc :: Assoc -> Doc
doc_assoc AssocLeft  = PP.text "AssocLeft"
doc_assoc AssocRight = PP.text "AssocRight"
doc_assoc AssocNone  = PP.text "AssocNone"


doc_rhs :: [Symbol] -> Doc
doc_rhs =
    ( PP.brackets
    . PP.hcat
    . PP.punctuate (PP.text ", ")
    . map symbol2name
    )


doc_rules :: [(Int, NonTerminal, [Symbol], Prec, Maybe Code)] -> Doc
doc_rules rs =
    let maybe_doc_code mc = case mc of
            Just c  -> PP.text "Just " <> (PP.text . show) c
            Nothing -> PP.text "Nothing"
        doc_one (i, n, ss, _, c) = PP.parens
            ( PP.int i
            <> PP.text ", "
            <> nonterminal2name n
            <> PP.text ", "
            <> doc_rhs ss
            <> PP.text ", "
            <> maybe_doc_code c
            )
        rules =
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.char ',')
            . map doc_one
            ) rs
    in  PP.text "rules :: [(RID, NonTerminal, [Symbol], Maybe Code)]"
        $$ PP.text "rules ="
        $$ PP.nest 4 rules


doc_rid2rule :: [(Int, NonTerminal, [Symbol], Prec, Maybe Code)] -> Doc
doc_rid2rule rs =
    let doc_one (i, _, ss, _, _) =
            PP.text "rid2rule "
            <> PP.int i
            <> PP.text " = "
            <> doc_rhs ss
    in  PP.text "rid2rule :: RID -> [Symbol]"
        $$ (PP.vcat . map doc_one) rs
        $$ PP.text "rid2rule rid = error $ \"RID exceeding maximum possible value: \" ++ show rid"


doc_rid2prec :: [(Int, NonTerminal, [Symbol], Prec, Maybe Code)] -> Doc
doc_rid2prec rs =
    let doc_one (i, _, _, p, _) =
            PP.text "rid2prec "
            <> PP.int i
            <> PP.text " = "
            <> doc_prec p
    in  PP.text "rid2prec :: RID -> Prec"
        $$ (PP.vcat . map doc_one) rs
        $$ PP.text "rid2prec rid = error $ \"RID exceeding maximum possible value: \" ++ show rid"


doc_n2rules :: [(Int, NonTerminal, [Symbol], Prec, Maybe Code)] -> Doc
doc_n2rules rs =
    let n2rules = foldl' (\ n2rs (_, n, ss, _, _) -> M.insertWith (++) n [ss] n2rs) M.empty rs
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


doc_n2rids :: [(Int, NonTerminal, [Symbol], Prec, Maybe Code)] -> Doc
doc_n2rids rs =
    let n2rids = foldl' (\ n2rs (i, n, _, _, _) -> M.insertWith (++) n [i] n2rs) M.empty rs
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
doc_axiom n =
    PP.text "axiom :: NonTerminal"
    $$ PP.text "axiom = " <> nonterminal2name n


doc_lambda :: Terminal -> Doc
doc_lambda l =
    PP.text "lambda :: Terminal"
    $$ PP.text "lambda = " <> terminal2name l


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


doc_t2prec_assoc :: M.HashMap Terminal (Prec, Assoc) -> Doc
doc_t2prec_assoc t2pa =
    let doc_one d t (p, a) = d
            $$ PP.text "t2prec_assoc "
            <> terminal2name t
            <> PP.text " = ("
            <> doc_prec p
            <> PP.text ", "
            <> doc_assoc a
            <> PP.char ')'
    in  PP.text "t2prec_assoc :: Terminal -> (Prec, Assoc)"
        $$ M.foldlWithKey' doc_one PP.empty t2pa


doc_instances :: S.Set Terminal -> S.Set NonTerminal -> Doc
doc_instances ts ns =
    PP.text "instance Hashable Terminal"
    $$$ PP.text "instance Hashable NonTerminal"
    $$$ PP.text "instance Hashable Symbol"
    $$$ PP.text "instance Hashable Prec"
    $$$ PP.text "instance Hashable Assoc"


doc_failure :: Maybe Code -> Doc
doc_failure mc =
    let d = case mc of
            Just c  -> PP.text "Just " <> PP.text (show c)
            Nothing -> PP.text "Nothing"
    in  PP.text "failure :: Maybe String"
        $$ PP.text "failure = " <> d


doc_phony :: M.HashMap Terminal Code -> Doc
doc_phony ph =
    let f (t, c) = PP.parens $
            ( (PP.doubleQuotes . PP.text . tail . PP.render . terminal2name) t
            <> PP.char ','
            <> (PP.doubleQuotes . PP.text) c
            )
    in  PP.text "phony :: [(String, String)]"
        $$ PP.text "phony =" <>
            ( PP.brackets
            . PP.vcat
            . PP.punctuate (PP.char ',')
            . map f
            . M.toList
            ) ph


doc_reentrant :: Bool -> Doc
doc_reentrant r =
    PP.text "reentrant :: Bool"
    $$ PP.text "reentrant = " <> PP.text (show r)


doc_codeblock :: String -> Maybe Code -> Doc
doc_codeblock s mc =
    let d = case mc of
            Just c  -> PP.text "Just " <> PP.text (show c)
            Nothing -> PP.text "Nothing"
    in  PP.text s <> PP.text " :: Maybe String"
        $$ PP.text s <> PP.text " = " <> d


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


gen_code :: (Maybe Code) -> Grammar -> (Maybe Code) -> String
gen_code entry (Grammar t2pa ns rs a l fl ph r) ending =
    let ts     = (S.fromList . M.keys) t2pa
        doc_ts = (PP.hcat . PP.punctuate (PP.text " | ") . map terminal2name . S.toList) ts
        doc_ns = (PP.hcat . PP.punctuate (PP.text " | ") . map nonterminal2name . S.toList) ns
    in  PP.render $
            PP.text "{-# LANGUAGE DeriveGeneric #-}"
            $$$ PP.text "module Grammar where"
            $$$ PP.text "import qualified Data.Set as S"
            $$ PP.text "import Data.Hashable"
            $$ PP.text "import GHC.Generics (Generic)"
            $$$ PP.text "type RID = Int"
            $$$ PP.text "type Code = String"
            $$ PP.text "data Terminal = " <> doc_ts <> PP.text " deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data NonTerminal = " <> doc_ns <> PP.text " deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data Symbol = T Terminal | N NonTerminal deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data Prec = PrecLevel Int | PrecNone deriving (Show, Eq, Ord, Generic)"
            $$ PP.text "data Assoc = AssocLeft | AssocRight | AssocNone deriving (Show, Eq, Ord, Generic)"
            $$$ doc_terminals ts
            $$$ doc_t2prec_assoc t2pa
            $$$ doc_nonterminals ns
            $$$ doc_symbols
            $$$ doc_rules rs
            $$$ doc_rid2rule rs
            $$$ doc_rid2prec rs
            $$$ doc_n2rules rs
            $$$ doc_n2rids rs
            $$$ doc_axiom a
            $$$ doc_lambda l
            $$$ doc_instances ts ns
            $$$ doc_failure fl
            $$$ doc_phony ph
            $$$ doc_reentrant r
            $$$ doc_codeblock "entry" entry
            $$$ doc_codeblock "ending" ending


main :: IO ()
main = do
    (fg, fhs) <- getArgs >>= \ args -> case args of
        [f1, f2] -> return (f1, f2)
        _        -> error "usage: ./gen_code <grammar-file> <hs-file>"

    parseFromFile parse_source fg >>= \ r -> case r of
        Left e            -> print e
        Right (c1, g, c2) -> writeFile fhs $ gen_code c1 (complement g) c2



