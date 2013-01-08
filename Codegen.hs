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
codegen tbl v =
    PP.render $
        doc_includes
        $$$ doc_defines
        $$$ doc_symbols
        $$$ (verbose v $ doc_print_stack $$$ doc_print_buffer)
        $$$ doc_parse tbl v


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


doc_parse :: LALRTable -> Verbosity -> Doc
doc_parse tbl v =
    doc_signature
    $$
        (wrap_in_braces $
        doc_init_stack
        $$$ doc_goto 0
        $$$ M.foldlWithKey' (\ doc sid (s, actions, gotos) -> doc $$$ doc_state sid s actions gotos) PP.empty tbl
        )


doc_signature :: Doc
doc_signature = PP.text "bool parse (Symbol * p, Symbol * q)"


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


doc_goto :: SID -> Doc
doc_goto sid = PP.text "goto state_" <> PP.int sid <> PP.semi


is_terminal :: Symbol -> Bool
is_terminal (T _) = True
is_terminal _     = False


doc_state :: SID -> Symbol -> ActionTable -> GotoTable -> Doc
doc_state sid s t2act n2sid =
    PP.text "state_" <> PP.int sid <> PP.colon
    $$ PP.nest 4
        ( PP.text "stack = " <> PP.int sid <> PP.semi
        $$ (if is_terminal s then PP.text "p++;" else PP.empty)
        $$ PP.text "stack++;"
        )
    $$ PP.text "state_action_" <> PP.int sid <> PP.colon
    $$ PP.nest 4
--        let d0 = PP.text "*p"
{-
            f t act =
                let val  = show t
                    code = case act of
                        Shift sid'  -> doc_goto sid
                        Reduce n ss ->
                        Accept      ->
                        Error       ->
                in  doc_casebreak val code
            d1 = M.foldlWithKey' (\ doc t act -> doc $$ f t act) PP.empty t2act
        in  doc_switch d0 d1
-}
          PP.empty

{-
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


doc_casebreak :: Doc -> Doc -> Doc
doc_casebreak d1 d2 =
    PP.text "case " <> d1 <> PP.colon
    $$ PP.nest 4 (d2 $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


