{
module LEXER where   
}

%wrapper "posn"
:- 
    "--".*              ;
    $white+             ;
    "+"                 {\p s -> PLUS s}
    "-"                 {\p s -> MINUS s}
    "*"                 {\p s -> TIMES s}
    "~"                 {\p s -> NEGATE s}
    "=="                {\p s -> EQUALS s}
    ">"                 {\p s -> GREATERTHAN s}
    "<"                 {\p s -> LESSTHAN s}
    "NOT"               {\p s -> NOT s}
    "AND"               {\p s -> AND s}
    "OR"                {\p s -> OR s}
    "XOR"               {\p s -> XOR s}
    "IMPLIES"           {\p s -> IMPLIES s}
    "TRUE"              {\p s -> CONST s}
    "FALSE"             {\p s -> CONST s}
    [0-9]+              {\p s -> CONST s}
    "="                 {\p s -> ASSIGNMENT s}
    "("                 {\p s -> LPAREN s}
    ")"                 {\p s -> RPAREN s}
    "if"                {\p s -> IF s}
    "then"              {\p s -> THEN s}
    "elseif"            {\p s -> ELSEIF s}
    "else"              {\p s -> ELSE s}
    "fi"                {\p s -> FI s}
    "let"               {\p s -> LET s}
    "in"                {\p s -> IN s}
    "end"               {\p s -> END s}
    [a-z A-Z]+          {\p s -> ID s}
    ";"                 {\p s -> EOS s}
{
data Token =PLUS String | MINUS String | TIMES String | NEGATE String |
            EQUALS String | LESSTHAN String | GREATERTHAN String | ELSEIF String |
            NOT String | AND String | OR String | XOR String | EOS String |
            IMPLIES String | CONST String | EOF String | ASSIGNMENT String |
            LPAREN String | RPAREN String | ID String | LET String | IN String |
            IF String | END String | THEN String | ELSE String | FI String
                deriving (Eq, Show)

scan s =    let 
                fun i@(p,_,_,s) = case alexScan i 0 of 
                                            AlexEOF -> [(EOF "EOF",p)]
                                            AlexError ((AlexPn _ l c),_,_,_)-> error $ "\nUnknown Token:"++(show l)++":"++(show c)++("  ")++(takeWhile (/=' ') s)
                                            AlexSkip ip len -> fun ip
                                            AlexToken ip len act -> (act p (take len s),p): fun ip
            in  fun (alexStartPos,'\n',[],s)
giveToken [] = []         
giveToken a@(x:y) = (fst x) : giveToken y
}
