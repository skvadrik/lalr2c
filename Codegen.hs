module Codegen
    ( codegen
    ) where


import qualified Data.HashMap.Strict as M
import           Data.List                 (foldl')
import qualified Data.Set            as S
import qualified Text.PrettyPrint    as PP
import           Text.PrettyPrint          (Doc, ($$), (<>), ($+$))

import Grammar
import Types
import LALR


codegen :: LALRTable -> Verbosity -> String
codegen tbl v = "" {-
    let d0 = doc_includes
        d1 = doc_defines
        d2 = doc_symbols
        d3 = verbose v $ doc_print_stack $$$ doc_print_buffer
        d4 = doc_parse pda v
    in  PP.render $ d0 $$$ d1 $$$ d2 $$$ d3 $$$ d4


doc_includes :: Doc
doc_includes =
    PP.text "#include <stdio.h>"


doc_defines :: Doc
doc_defines =
    PP.text "#define STACK_SIZE 1024"
    $$ PP.text "#define VERBOSE false"


doc_symbols :: Doc
doc_symbols =
    let tsymbols = (map show . S.toList) terminals
        nsymbols = (map show . S.toList) nonterminals
        symbols = (PP.vcat . PP.punctuate PP.comma . map PP.text) (tsymbols ++ nsymbols)
    in  PP.text "enum Symbol" $$ wrap_in_braces symbols <> PP.semi


doc_print_stack :: Doc
doc_print_stack =
    let d0 = PP.text "void print_stack (const Symbol * bottom, Symbol * top)"
        d1 =
            PP.text "Symbol * p = top;"
            $$ PP.text "printf (\"stack:\\n\");"
            $$ doc_while (PP.text "p - bottom >= 0") (PP.text "printf (\"\\t%d\\n\", *p--);")
    in d0 $$ wrap_in_braces d1


doc_print_buffer :: Doc
doc_print_buffer =
    let d0 = PP.text "void print_buffer (Symbol * begin, Symbol * end)"
        d1 =
            PP.text "Symbol * p = begin;"
            $$ PP.text "printf (\"buffer:\\n\");"
            $$ doc_while (PP.text "end - p >= 0") (PP.text "printf (\"\\t%d\\n\", *p++);")
    in d0 $$ wrap_in_braces d1


doc_parse :: PDA -> Verbosity -> Doc
doc_parse pda v =
    let (sll1_tbl, id2cmd) = restrict_to_sll1 (commands pda)
        d0 = doc_signature
        d1 = doc_init_table sll1_tbl
        d2 = doc_init_stack
        d3 = doc_loop id2cmd v
        d4 = doc_return v
        d5 = wrap_in_braces $ d1 $$$ d2 $$$ d3 $$ d4
    in  d0 $$ d5


doc_signature :: Doc
doc_signature = PP.text "bool parse (Symbol * p, Symbol * q)"


doc_init_table :: M.HashMap (NonTerminal, Terminal) Int -> Doc
doc_init_table tbl =
    let d1 =
            PP.text "int table "
            <> PP.brackets (PP.int (S.size nonterminals))
            <> PP.brackets (PP.int (S.size terminals))
            <> PP.semi
        f doc (n, t) k = doc
            $$ PP.text "table "
            <> PP.brackets ((PP.text . show) n)
            <> PP.brackets ((PP.text . show) t)
            <> PP.text " = "
            <> PP.int k
            <> PP.semi
        d2 = M.foldlWithKey' f PP.empty tbl
    in  d1 $$$ d2


doc_init_stack :: Doc
doc_init_stack =
    let d1 =
            PP.text "Symbol * stack = new Symbol "
            <> PP.brackets (PP.text "STACK_SIZE")
            <> PP.semi
        d2 = PP.text "const Symbol * stack_bottom = &stack[0];"
        d3 =
            PP.text "*stack = "
            <> (PP.text . show) axiom
            <> PP.semi
    in  d1 $$ d2 $$$ d3


doc_loop :: M.HashMap Int [Symbol] -> Verbosity -> Doc
doc_loop id2cmd v =
    let d0         = PP.text "p != q" -- "stack != stack_bottom && p != q"
        dd0        = doc_verbose v $ PP.text "print_stack (stack_bottom, stack);" $$ PP.text "print_buffer (p, q);"
        d_if       = PP.text "*stack == *p"
        d_then     = PP.text "p++;" $$ PP.text "--stack;"
        dd_then    = doc_verbose v $ PP.text "printf (\"shift\\n\");"
        d_cases    = doc_cases id2cmd
        d_default  = doc_default (PP.text "return false;")
        dd_default = doc_verbose v $ PP.text "printf (\"FAIL\\n\");"
        d_else     = doc_switch (PP.text "table [*stack][*p]") (d_cases $$ dd_default $$ d_default)
        dd_else    = doc_verbose v $ PP.text "printf (\"rule %d\\n\", table [*stack][*p]);"
        d1         = doc_ifthenelse d_if (dd_then $$ d_then) (dd_else $$ d_else)
    in  doc_while d0 (dd0 $$ d1)


doc_cases :: M.HashMap Int [Symbol] -> Doc
doc_cases id2cmd =
    let doc_symbol (N n) = (PP.text . show) n
        doc_symbol (T t) = (PP.text . show) t
        f doc k ss =
            let doc' = case ss of
                    []      -> PP.text "--stack;"
                    s : ss' ->
                        foldl' (\ d s -> PP.text "*stack++ = " <> doc_symbol s <> PP.semi $$ d) PP.empty ss'
                        $$ PP.text "*stack = " <> doc_symbol s <> PP.semi
            in  doc $$ doc_casebreak k doc'
    in  M.foldlWithKey' f PP.empty id2cmd


doc_return :: Verbosity -> Doc
doc_return v =
    doc_verbose v (PP.text "printf (\"SUCCESS\\n\");")
    $$ PP.text "return true;"
-}

----------------------------------------------------------------------


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


verbose :: Verbosity -> Doc -> Doc
verbose v d = case v of
    V0 -> PP.empty
    V1 -> d


doc_verbose :: Verbosity -> Doc -> Doc
doc_verbose v d = verbose v $ doc_ifthen (PP.text "VERBOSE") d


wrap_in_braces :: Doc -> Doc
wrap_in_braces d =
    PP.text "{"
    $+$ PP.nest 4 d
    $$ PP.text "}"


doc_ifthen :: Doc -> Doc -> Doc
doc_ifthen d1 d2 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2


doc_ifthenelse :: Doc -> Doc -> Doc -> Doc
doc_ifthenelse d1 d2 d3 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2
    $$ PP.text "else"
    $$ wrap_in_braces d3


doc_while :: Doc -> Doc -> Doc
doc_while d1 d2 =
    PP.text "while " <> (PP.parens d1)
    $$ (wrap_in_braces d2)


doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 =
    PP.text "switch " <> (PP.parens d1)
    $$ (wrap_in_braces d2)


doc_casebreak :: Int -> Doc -> Doc
doc_casebreak k d =
    PP.text "case " <> PP.int k <> PP.colon
    $$ PP.nest 4 (d $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


