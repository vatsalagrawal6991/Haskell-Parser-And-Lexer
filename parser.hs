{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.Environment (getArgs) 
import LEXER
import Control.Exception
import System.Posix.ByteString (getArgs)

data Tree s = Terminal Token | If (Tree s)  | NULL | Exp Token (Tree s)  | Add (Tree s) | Sub (Tree s) | Mul (Tree s) |
              Equal (Tree s) |  Lst (Tree s) | Grt (Tree s) | Not (Tree s) | And (Tree s) |
              Xor (Tree s) | Or (Tree s) | Implies (Tree s) | 
              Then (Tree s) | Else (Tree s) |  Let (Tree s)  | Assign Token (Tree s) |
              In (Tree s) | Start (Tree s) | Eof | Eoe (Tree s) | Fi (Tree s) | End (Tree s) |
              Lparen (Tree s) | Rparen (Tree s) | Negate (Tree s)
                deriving(Eq,Show)

data AST s =    Starta (AST s) (AST s) | Impliesa (AST s) (AST s) | Equalsa (AST s) (AST s) | Eofa |
                Xora (AST s) (AST s) | Ora (AST s) (AST s) | Anda (AST s) (AST s) | Gta (AST s) (AST s) |
                Lta (AST s) (AST s) | Adda (AST s) (AST s) | Suba (AST s) (AST s) | Mula (AST s) (AST s) | Enda | Statement (AST s) (AST s) |
                Nega (AST s) | Nota (AST s) | Id String | Num String | Assigna (AST s) (AST s) (AST s) | Ina (AST s) Token |
                Condition Token (AST s) Token (AST s) Token (AST s) Token | Leta (AST s) Token (AST s) Token | NULLa | Eosa
                deriving (Eq,Show)


main = do
    args <- System.Environment.getArgs ::IO [String]
  --s <- getContents
    s <- readFile (head args)
    let a = scan s
    putStrLn ""
    let b = giveToken a
    if (null (preot b)==False) then print b else do 
        putStrLn "List Of Tokens"
        print b
        let e = start a []
        putStrLn ""
        if ((preoo e)/=["EOF"]) then print e else do
        --putStrLn "A Parse Tree for correct syntax :-"
        --print e
            let c = prefi (reverse a) [] [] 
            let d = giveToken c 
            let f= aststart d
            putStrLn ""
            putStrLn "Abstract Tree :-"
            print f
            putStrLn ""
            putStrLn "Preorder Of AST :-"
            print (preorder f)


start a b=  if (fst(head a))== EOF "EOF"
            then Start (Eof)
            else Start (start' a b ['$']) 


start' a b c= let x:y=a
              in  if (fst x)==LET "let"
                  then Let (leta y b ('i':c))
                  else expr a b c 
               
leta ((a@(ID m,n)):(c@(ASSIGNMENT o,p)):q) b d = Assign (fst a) (expr q (m:b) d) 
leta ((a@(ID m,n)):(o,p):q) b d = error $ "\nSyntax Error:"++(show (line p))++":"++(show (column p))++"Assign ::- ID Assign' Exp EOS "
leta ((a@(IN m,n)):q) b d =   if (head d == 'i')
                              then In(expr q b ('n':tail d))
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
leta ((m,n):q) b d= error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
leta [] b d= error $ "Start' ::- let Statment in Exp end"

line p@(AlexPn _ l _)= l
column p@(AlexPn _ _ c)= c
                    

expr ((a@(IF m,n)):q) b d =   if (head d /= 't' && head d /='e')
                              then If (expr q b ('t':d))
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Condition ::- if Exp else Exp then Exp fi"

expr ((a@(LPAREN m,n)):q) b d = Lparen (expr q b (')':d))

expr ((a@(ID m,n)):p) b d = if (elem m b)
                            then Exp (ID m) (exprs (p) b d)
                            else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Terminal ::- ID"
expr ((a@(CONST m,n)):p) b d = Exp (CONST m) (exprs (p) b d)

expr ((a@(NEGATE m,n)):(p@(ID o, k)):q) b d = Negate (expr (p:q) b d)
expr ((a@(NEGATE m,n)):(p@(CONST o, k)):q) b d =  if (o/="TRUE"&& o/="FALSE")
                                                  then Negate (expr (p:q) b d)
                                                  else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"NN ::- NEGATE Terminal"
expr ((a@(NEGATE m,n)):(p@(LPAREN o, k)):q) b d = Negate (expr (p:q) b d)
expr ((a@(NEGATE m,n)):q) b d = error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"NN ::- NEGATE Terminal "
expr ((a@(NOT m,n)):(p@(ID o, k)):q) b d = Not (expr (p:q) b d)
expr ((a@(NOT m,n)):(p@(CONST o, k)):q) b d =  if (o=="TRUE"&& o=="FALSE")
                                                  then Not (expr (p:q) b d)
                                                  else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"NN ::- NOT Terminal"
expr ((a@(NOT m,n)):(p@(LPAREN o, k)):q) b d = Not (expr (p:q) b d)
expr ((a@(NOT m,n)):q) b d = error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"NN ::- NEGATE Terminal"
expr (a@(x,n):q) b c =  error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Exp ::- Implies"


exprs ((PLUS "+",n):r) b c =  Add (expr r b c)
exprs ((MINUS "-",n):r) b c =  Sub (expr r b c)
exprs ((TIMES "*",n):r) b c =  Mul (expr r b c)
exprs ((EQUALS "==",n):r) b c =  Equal (expr r b c)
exprs ((LESSTHAN "<",n):r) b c =  Lst (expr r b c)
exprs ((GREATERTHAN ">",n):r) b c =  Grt (expr r b c)
exprs ((AND "AND",n):r) b c =  And (expr r b c)    
exprs ((OR "OR",n):r) b c =  Or (expr r b c)
exprs ((XOR "XOR",n):r) b c =  Xor (expr r b c)    
exprs ((IMPLIES "IMPLIES",n):r) b c =  Implies (expr r b c)                            
exprs ((a@(RPAREN m,n)):q) b d =if (head d == ')')
                                then Rparen (exprs (q) b (tail d))
                                else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Terminal ::- (Exp)"    
exprs ((a@(EOF m,n)):q) b d =  exita (a:q) b d
exprs ((a@(END m,n)):q) b d = if (head d == 'n')
                              then End (exita q b (tail d)) 
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
exprs ((a@(EOS m,n)):q) b d = if (head d == 'i')
                              then leta q b d
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Assign ::- ID Assign' Exp EOS "
exprs ((a@(THEN m,n)):q) b d =if (head d == 't')
                              then Then(expr q b ('e':tail d) )
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
exprs ((a@(ELSE m,n)):q) b d =if (head d == 'e')
                              then Else(expr q b ('f':tail d))
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
exprs ((a@(FI m,n)):q) b d =  if (head d == 'f')
                              then Fi (removefi q b d) 
                              else error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start' ::- let Statment in Exp end"
exprs (a@(x,n):q) b c =  error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Exp ::- Implies"

removefi q b ('f':d)= removefi q b d
removefi q b a = exprs q b a

exita ((a@(m,n)):q) b ['$'] =  case m of 
                                      EOF "EOF" -> Eof
                                      otherwise -> error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start ::- Start' EOF"

exita (a@(m,n):q) b c =  error $ "\nSyntax Error:"++(show (line n))++":"++(show (column n))++"Start ::- Start' EOF"

prefi (b@(EOF m, n):p) c d=  prefi p (b:c) d
prefi (b@(END m, n):p) c d=  prefi p (b:c) d
prefi (b@(ID m, n):p) c d=  prefi p (b:c) d
prefi (b@(CONST m, n):p) c d=  prefi p (b:c) d
prefi (b@(RPAREN m, n):p) c d=  prefi p c (b:d)
prefi (b@(LPAREN m, n):p) c d=  adjust p c d
prefi (b@(IN m, n):p) c d=  transferA (b:p) c d
prefi (b@(EOS m, n):p) c d=  prefi p (b:c) d
prefi (b@(ASSIGNMENT m, n):p) c d=  transferA (b:p) c d
prefi (b@(LET m, n):p) c d=  prefi p (b:c) d
prefi (b@(IF m, n):(e@(ELSE o, q)):p) c d=  adjustD (b:e:p) c d
prefi (b@(IF m, n):p) c d=  adjustB (b:p) c d
prefi (b@(THEN m, n):p) c d=  adjustC (b:p) c d
prefi (b@(ELSE m, n):p) c d=  adjustA (b:p) c d
prefi (b@(FI m, n):p) c d=  prefi p (b:c) (b:d)
prefi (b@(NEGATE m, n):p) c d=  prefi p (b:c) d
prefi (b@(NOT m, n):p) c d=  prefi p (b:c) d
prefi (b@(TIMES m, n):p) c d=  prefi p c (b:d)
prefi (b@(PLUS m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(PLUS m, n):p) c d= prefi p c (b:d)
prefi (b@(MINUS m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(MINUS m, n):p) c d= prefi p c (b:d)
prefi (b@(GREATERTHAN m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(GREATERTHAN m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(GREATERTHAN m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(GREATERTHAN m, n):p) c d= prefi p c (b:d)
prefi (b@(LESSTHAN m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(LESSTHAN m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(LESSTHAN m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(LESSTHAN m, n):p) c d= prefi p c (b:d)
prefi (b@(EQUALS m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(EQUALS m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(EQUALS m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(EQUALS m, n):p) c e@(d@(GREATERTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(EQUALS m, n):p) c e@(d@(LESSTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(EQUALS m, n):p) c d= prefi p c (b:d)
prefi (b@(AND m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(AND m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(AND m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(AND m, n):p) c e@(d@(GREATERTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(AND m, n):p) c e@(d@(LESSTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(AND m, n):p) c d= prefi p c (b:d)
prefi (b@(OR m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(OR m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(OR m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(OR m, n):p) c e@(d@(GREATERTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(OR m, n):p) c e@(d@(LESSTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(OR m, n):p) c d= prefi p c (b:d)
prefi (b@(XOR m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(XOR m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(XOR m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(XOR m, n):p) c e@(d@(GREATERTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(XOR m, n):p) c e@(d@(LESSTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(XOR m, n):p) c d= prefi p c (b:d)
prefi (b@(IMPLIES m, n):p) c e@(d@(TIMES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(PLUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(MINUS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(GREATERTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(LESSTHAN o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(XOR o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(OR o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(AND o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(EQUALS o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c e@(d@(IMPLIES o,j):q)= prefi (b:p) (d:c) q
prefi (b@(IMPLIES m, n):p) c d= prefi p c (b:d)
prefi [] c d = transfer c d

transferA a@(b:p) c []=prefi p (b:c) []
transferA a c e@(d@(m,n):q) = transferA a (d:c) q

transfer c []=c
transfer c e@(d@(m,n):q) = transfer (d:c) q

adjustA a@(b:p) c ((FI "fi",n):q) = prefi p (b:c) (b:q)
adjustA a c (d@(m,n):q)  = prefi a (d:c) q 


adjustC a@(b:p) c e@(d@(m,n):q)  =      if m==(ELSE "else")
                                        then prefi p (b:c) (b:q)
                                        else prefi a (d:c) q

adjustB a@(b:p) c e@(d@(m,n):q)  =      if m==THEN "then"
                                        then prefi p (b:c) q
                                        else prefi a (d:c) q


adjustD a@(b:x:p) c e@(d@(m,n):q)  =    if m==THEN "then"
                                        then prefi p (x:b:c) (x:q)
                                        else prefi a (d:c) q

adjust p c (d@(m,n):q) =    if m==RPAREN ")" 
                            then prefi p c q
                            else adjust p (d:c) q


aststart a@(x:y) =      let (tree1)= (NULLa)
                            (tree2)= (Eofa)
                            (tree3,d@(m:q))= starta' a
                            (tree4,e)=(Eofa,[])            
                        in  if x==EOF "EOF"
                            then (Starta tree1 tree2)
                            else (Starta tree3 tree4)
starta' a@((x):y)     =       let (tree1 , b)= starta' y
                                  (tree2 , c)= starta' b 
                              in  case x of 
                                    EQUALS "==" -> (Equalsa tree1 tree2,c) 
                                    OR "OR" -> (Ora tree1 tree2,c)
                                    XOR "XOR" -> (Xora tree1 tree2,c) 
                                    AND "AND" -> (Anda tree1 tree2,c) 
                                    IMPLIES "IMPLIES"-> (Impliesa tree1 tree2,c)
                                    GREATERTHAN ">" -> (Gta tree1 tree2,c) 
                                    LESSTHAN "<" -> (Lta tree1 tree2,c)
                                    PLUS "+" -> (Adda tree1 tree2,c) 
                                    MINUS "-" -> (Suba tree1 tree2,c)
                                    TIMES "*" -> (Mula tree1 tree2,c)
                                    EOS ";"-> (Eosa,y)
                                    otherwise -> startb a
startb a@((ID x):y)= (Id x,y)
startb a@((CONST x):y)= (Num x,y)
startb a@((IF x):y) =               let (tree1 , ba@((xa):gh))= starta' y 
                                        (tree2 , bb@((xb):gi))= starta' gh 
                                        (tree3 , bc@((xc):gk))= starta' gi 
                                    in  (Condition (IF x) tree1 xa tree2 xb tree3 xc,gk)
startb a@((LET x):y) =              let (tree1 , ba@((xa):gh))= startc y 
                                        (tree2 , bb@((xb):gi))= starta' gh                                    
                                    in  (Leta tree1 xa tree2 xb ,gi)
startc a@((ID x):y) =               let (tree1 , ba@((xa):gh))= startd a 
                                        (tree2 , bb@((xb):gi))= startc ba                                  
                                    in  (Statement tree1 tree2 ,bb)
startc a@((IN x):y) =               (NULLa,a)

startd a@((x):y) =                  let (tree1 , ba@((xa):gh))= starta' a  
                                        (tree2 , bb@((xb):gi))= starta' gh 
                                        (tree3 , bc@((xc):gk))= starta' bb                                   
                                    in  (Assigna tree1 tree2 tree3 ,bc)

preorder (Starta a b)  =  ["Start-> AST EOF"]++preorder a ++ preorder b
preorder (Impliesa a b)  =  ["Implies-> AST AST"]++preorder a ++ preorder b
preorder (Equalsa a b)  =  ["Equals-> AST AST"]++preorder a ++ preorder b
preorder (Eofa)  =  ["EOF" ]
preorder (Xora a b)  =  ["Xor-> AST AST" ]++preorder a ++ preorder b
preorder (Ora a b)  =  ["Or-> AST AST" ]++preorder a ++ preorder b
preorder (Anda a b)  =  ["And-> AST AST" ]++preorder a ++ preorder b
preorder (Gta a b)  =  ["GreaterThan-> AST AST" ]++preorder a ++ preorder b
preorder (Lta a b)  =  ["LessThan-> AST AST" ]++preorder a ++ preorder b
preorder (Adda a b)  =  ["Add-> AST AST" ]++preorder a ++ preorder b
preorder (Suba a b)  =  ["Sub-> AST AST" ]++preorder a ++ preorder b
preorder (Mula a b)  =  ["Mul-> AST AST" ]++preorder a ++ preorder b
preorder (Enda)  =  ["END end"]
preorder (Statement a b)  =  ["Statment-> Assign Statment | NULL" ]++preorder a ++ preorder b
preorder (Nega a )  =  ["Negative-> Ast" ]++preorder a
preorder (Nota a )  =  ["Not-> Ast" ]++preorder a
preorder (Id b)  =  ["ID "++b ]
preorder (Num b)  =  ["Num/Const "++b ]
preorder (Assigna a b c)  =  ["Assign-> ID AST EOS" ]++preorder a ++ preorder b++preorder c
preorder (Ina a b)  =  ["In-> AST END"]++preorder a++ ["END end"]
preorder (Condition a b c d e f g)  =  ["Condition-> IF AST ELSE AST THEN AST FI"]++["IF if"]++preorder b ++["THEN then"]++ preorder d++["ELSE else"]++preorder f++["FI fi"]
preorder (Leta a b c d)   =  ["Let-> Statment In" ]++preorder a ++["IN in"]++ preorder c++["END end"]
preorder (NULLa)   =  ["NULL" ]
preorder (Eosa)   =  ["EOS ;" ]



preoo (Terminal a)  =  []
preoo (If a)  =  []++preoo a
preoo (NULL)  =  []
preoo (Exp a b)  =  [ ]++preoo b
preoo (Add b)  =  []++ preoo b
preoo (Sub b)  =  []++ preoo b
preoo (Mul b)  =  []++ preoo b
preoo (Equal b)  =  []++ preoo b
preoo (Lst b)  =  []++ preoo b
preoo (Grt b)  =  []++ preoo b
preoo (Not b)  =  []++ preoo b
preoo (And b)  =  []++ preoo b
preoo (Or b)  =  []++ preoo b
preoo (Implies b)  =  []++ preoo b
preoo (Then b)  =  []++ preoo b
preoo (Else b)  =  []++ preoo b
preoo (Let b)  =  []++ preoo b
preoo (Assign a b)  =  []++ preoo b
preoo (In b)  =  []++ preoo b
preoo (Start b)  =  []++ preoo b 
preoo (Eof)  =  ["EOF"]
preoo (Eoe b)  =  []++ preoo b
preoo (Fi b)  =  []++ preoo b
preoo (End b)  =  []++ preoo b 
preoo (Lparen b)  =  []++ preoo b 
preoo (Negate b)  =  []++ preoo b 
preoo (Rparen b)  =  []++ preoo b 

{-data Tree s = Terminal Token | If (Tree s)  | NULL | Exp Token (Tree s)  | Add (Tree s) | Sub (Tree s) | Mul (Tree s) |
              Equal (Tree s) |  Lst (Tree s) | Grt (Tree s) | Not (Tree s) | And (Tree s) |
              Xor (Tree s) | Or (Tree s) | Implies (Tree s) | 
              Then (Tree s) | Else (Tree s) |  Let (Tree s)  | Assign Token (Tree s) |
              In (Tree s) | Start (Tree s) | Eof | Eoe (Tree s) | Fi (Tree s) | End (Tree s) |
              Lparen (Tree s) | Rparen (Tree s) | Negate (Tree s)
                deriving(Eq,Show)-}
preot [] =[]
preot (x:xs) = preot xs
