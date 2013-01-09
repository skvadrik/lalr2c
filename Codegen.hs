module Codegen
    ( codegen
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                 (foldl')
import qualified Text.PrettyPrint    as PP
import           Text.PrettyPrint          (Doc, ($$), (<>), ($+$))

import Grammar
import Types


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
    let d0 = PP.text "void print_stack (const int * bottom, int * top)"
        d1 =
            PP.text "int * p = top;"
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
    $$ ( wrap_in_braces $
        doc_init_stack
        $$$ doc_goto (PP.text "state_" <> PP.int 1)
        $$$ M.foldlWithKey' (\ doc sid (s, actions, gotos) -> doc $$$ (doc_state v) sid s actions gotos) PP.empty tbl
        $$$ M.foldlWithKey' (\ doc (n, r) rid -> doc $$$ (doc_rule v) rid n r) PP.empty rule2rid
        $$$ S.foldl' (\ doc n -> doc $$$ doc_nonterminal v tbl n) PP.empty nonterminals
    )


doc_signature :: Doc
doc_signature = PP.text "bool parse (Symbol * p)"


doc_init_stack :: Doc
doc_init_stack =
    let d1 =
            PP.text "int * stack = new int "
            <> PP.brackets (PP.text "STACK_SIZE")
            <> PP.semi
        d2 = PP.text "const int * stack_bottom = &stack[0];"
        d3 =
            PP.text "*stack = "
            <> (PP.text . show) axiom
            <> PP.semi
    in  d1 $$ d2 $$$ d3


doc_goto :: Doc -> Doc
doc_goto d = PP.text "goto " <> d <> PP.semi


is_terminal :: Symbol -> Bool
is_terminal (T _) = True
is_terminal _     = False


doc_state :: Verbosity -> SID -> Symbol -> ActionTable -> GotoTable -> Doc
doc_state v sid s t2act n2sid =
    PP.text "state_" <> PP.int sid <> PP.colon
    $$ PP.nest 4
        ( PP.text "*stack = " <> PP.int sid <> PP.semi
        $$ (if is_terminal s then PP.text "p++;" else PP.empty)
        $$ PP.text "stack++;"
        )
    $$ PP.text "state_action_" <> PP.int sid <> PP.colon
    $$ PP.nest 4 (
        let d0 = PP.text "*p"
            f ts act =
                let d = (map (PP.text . show) . S.toList) ts
                in  case act of
                        Shift sid'  -> doc_multicasebreak d $ doc_goto (PP.text "state_" <> PP.int sid')
                        Reduce n ss -> doc_multicasebreak d $ doc_goto (PP.text "reduce_" <> PP.int (get_rid (n, ss)))
                        Accept      -> doc_multicasebreak d $
                            (doc_verbose v $ PP.text "printf (\"SUCCESS\\n\");")
                            $$ PP.text "return true;"
                        Error       -> PP.empty
            act2ts = M.foldlWithKey' (\ m t act -> M.insertWith S.union act (S.singleton t) m) M.empty t2act
            d1 = M.foldlWithKey' (\ doc act ts -> doc $$ f ts act) PP.empty act2ts
            d2 = doc_default $
                (doc_verbose v $ PP.text "printf (\"FAIL\\n\");")
                $$ PP.text "return false;"
        in  doc_switch d0 (d1 $$ d2)
    )


doc_rule :: Verbosity -> RID -> NonTerminal -> [Symbol] -> Doc
doc_rule v rid n ss =
    PP.text "reduce_" <> PP.int rid <> PP.colon
    $$ PP.nest 4
        ( PP.text "stack -= " <> PP.int (length ss) <> PP.semi
        $$ doc_goto (PP.text ("nonterminal_" ++ show n))
        )


doc_nonterminal :: Verbosity -> LALRTable -> NonTerminal -> Doc
doc_nonterminal v tbl n =
    PP.text ("nonterminal_" ++ show n) <> PP.colon
    $$ PP.nest 4 (
        let d0 = PP.text "*(stack - 1)"
            f1 m sid (_, _, gotos) =
                let sid' = M.lookupDefault (error "can't find nonterminal") n gotos
                in  M.insertWith S.union sid' (S.singleton sid) m
            sid2sids = M.foldlWithKey' f1 M.empty tbl
            f2 0   _    = PP.empty
            f2 sid sids =
                let d = (map (PP.text . show) . S.toList) sids
                in  doc_multicasebreak d $ doc_goto (PP.text "state_" <> PP.int sid)
            d1 = M.foldlWithKey' (\ doc sid sids -> doc $$ f2 sid sids) PP.empty sid2sids
            d2 = doc_default $ doc_goto (PP.text "state_0")
        in  doc_switch d0 (d1 $$ d2)
    )


rule2rid :: M.HashMap (NonTerminal, [Symbol]) RID
rule2rid =
    let f (r2id, id) n = foldl' (\ (m, i) r -> (M.insert (n, r) i m, i + 1)) (r2id, id) (rules n)
    in  fst $ S.foldl' f (M.empty, 0) nonterminals


get_rid :: (NonTerminal, [Symbol]) -> RID
get_rid x = M.lookupDefault (error "missing RID for rule") x rule2rid


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


doc_multicasebreak :: [Doc] -> Doc -> Doc
doc_multicasebreak ds d =
    (PP.vcat $ map (\ d -> PP.text "case " <> d <> PP.colon) ds)
    $$ PP.nest 4 (d $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


