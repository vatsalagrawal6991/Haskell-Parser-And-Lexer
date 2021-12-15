all:
	alex lexer.x -o LEXER.hs
	ghc --make parser.hs -o a2
run : all
	./a2 ./INPUT/input1
