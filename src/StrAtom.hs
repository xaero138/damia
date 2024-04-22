module StrAtom
    ( saAdd
    , saSub
    , saMul
    , saDiv
    , saDeg
    , saSqr
    , saCnt
    , saEnd
    , saMsg
    , saMsgLn
    , saEqu
    , saBrackets
    , saBrIfNeg
    )
where

-- | atoms of binary operations
bin str a b = a ++ str ++ b

saEqu = bin " = "
saAdd = bin " + "
saSub = bin " - "
saMul = bin " * "
saDiv = bin " / "
saDeg = bin "^"
saSqr = ("√"++)
saCnt = bin ""

saMsg = bin ": "
saEnd = (++"\n")
saMsgLn = bin ":\n"

-- | atoms of brackets
saBrackets v = "(" ++ v ++ ")"

saBrIfNeg :: (Num v, Ord v, Show v) => v -> String
saBrIfNeg v = if v < 0 then saBrackets $ show v else show v

