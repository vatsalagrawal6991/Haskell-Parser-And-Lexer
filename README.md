# Made By Vatsal Agrawal
# Haskell-Parser-And-Lexer

1) I HAVE USED 2 PASS PARSER
2) IN FIRST PASS I HAVE SEEN THAT SYNTAX IS VALID FOR ALL PARSER AND OUTPUT TREE IS SIMPLE TREE WITH NO PRECEDENCE AND ASSOCIATIVITY FIXED
3) IN FIRST PASS I HAVE USED LL1 PARSER
3) IN SECOND PASS I HAVE USED OPERATOR PRECEDENCE PARSER FOR EXPRESSION AND COMBINED FORM OF OPERATOR PRECEDENCE AND LL1 FOR GENERATING ABSTRACT TREE 
4) WE CAN ALSO GENERATE PARSE TREE FROM THIS SECOND PASS, BUT EXAMPLE GIVEN IS ABSTRACT TREE SO I HAVE GENERATED PARSE TREE DIRECTLY IN THIS PASS
5) NOTE WHILE GENERATING ABSTRACT TREE I HAVE REMOVED BRACKETS ( )
6) RECURSIVE LET IS NOT ALLOWED
7) RECURSIVE IF IS ALLOWED BUT IT SHOUD BE LIKE HASKELL FORM IF THEN ELSE IF THEN ELSE FI  
8) END IS FOLLOWED BY EOF IN OUR GRAMMAR
9) EVERY ASSIGNMENT STATMENT SHOULD END WITH ; EOS
10) REST EXPRESSION DO NOT END WITH ; EOS 
11) I HAVE ASSUMED TRUE AND FALSE AS CONSTANT AS GIVEN IN QUESTION
12) BOOLEAN AND NUM EVALUATION WILL BE DONE AT SEMANTICS STAGE


**************************HOW TO MAKE EXECUTABLE AND RUN PROGRAMME******************************
 
a)	RUN alex lexer.x -o LEXER.hs
b) 	RUN ghc --make parser.hs -o a2
c) 	RUN ./parser ./INPUT/input
		HERE ./PARSE IS OUTPUT FILE NAME AND ./INPUT/input IS FILE LOCATION AND FILE 		NAME PASSED AS ARGUMENTS
		
	OR
a)	RUN make
b)	RUN make run


Problem Statment

Integer arithmetic operators are: PLUS, MINUS, TIMES, NEGATE(unary operator), EQUALS,
LESSTHAN, GREATERTHAN. Boolean arithmetic operators are: NOT, AND, OR, XOR, IMPLIES,
EQUALS. The associativity of boolean operators is defined as: NOT, IMPLIES are right-to-left
associative and the rest are left-to-right associative. Assume the precedence to be the following:
NOT > AND = OR = XOR = EQUALS > IMPLIES

Constants TRUE, FALSE representing boolean 1 and 0 respectively. Use token type CONST for
constants.
• Use token types LPAREN and RPAREN for left and right parenthesis respectively, which help
resolve the order of evaluation over different operations.
• Each program should end with a token type EOF.
• Any other string containing only lower and upper case English alphabets is a variable. Use
token type ID for variable identifiers.
• The expression grammar also has:
– support for if exp then exp else exp fi.
– support for let var = exp in exp end to create temporary identifier bindings. Here exp
is either a valid formula or a valid integer arithmetic expression.


Implement a recursive descent parser from scratch for the sought language (do not use
Happy parser generator for Haskell). You will have to import the lexer module in your parser
implementation so that you can utilize the main lexing routines (look at the generated
lexer.hs for main lexing functions) while parsing.
compile your submission by running make command and the run the executable as
./a2 hfilenamei. The executable a2 should produce the output of the lexer followed by
a newline, then the parser’s output. Lexer output should be a comma-separated list (enclosed in square brackets) of tokens in order of their appearance in the input file. Each token
in output should be of the form <token type> space <actual token in the input file
enclosed in double quotes>. The output of the parser should be the preorder traversal of
the generated parse tree. The preorder traversal should be a comma-separated list of each
node’s representation. Use the production rules to represent a non-terminal node, and <token
type> space <actual token in the input file> to represent the terminal nodes in preorder traversal.
• Whenever an invalid token is encountered, lexer should generate Unknown Token:<line
no>:<column number>:<token> error. Here <line no> and <column number> start from 1,
and <token> is the invalid token. If the input is not syntatically correct according to the specifications, the parser should generate Syntax Error:<line no>:<column number>:<production
rule> error. Here <production rule> is the production rule where syntax did not match.
