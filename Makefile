BUILD_DIR=build
FILES=${BUILD_DIR}/{LexGrammar,AbsGrammar,ParGrammar,SkelGrammar,PrintGrammar,ErrM}.hs

all: interpreter

interpreter: grammar
	ghc --make Main.hs ${FILES} -o interpreter -outputdir ${BUILD_DIR}

grammar: grammar-compile
	cd build && make

grammar-compile:
	mkdir -p ${BUILD_DIR}
	cabal exec -- bnfc -m -haskell Grammar.cf -o ${BUILD_DIR}

.PHONY:
clean:
	rm -f ${BUILD_DIR}/*
