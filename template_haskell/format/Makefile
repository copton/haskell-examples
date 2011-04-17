.PHONY: all clean

all:
	ghc -ddump-splices -XTemplateHaskell --make Main.hs -o main

clean:
	git clean -d -X -f
