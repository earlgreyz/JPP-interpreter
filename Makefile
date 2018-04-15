BUILD_DIR=build
FILES=${BUILD_DIR}/{LexGrammar,AbsGrammar,ParGrammar,SkelGrammar,PrintGrammar,ErrM}.hs

all: interpreter

interpreter: grammar Main.hs MInterpreter.hs
	ghc --make Main.hs ${FILES} -o interpreter -outputdir ${BUILD_DIR}

grammar: Grammar.cf
	mkdir -p ${BUILD_DIR}
	cabal exec -- bnfc -m -haskell Grammar.cf -o ${BUILD_DIR}
	cd build && make

fast: Main.hs MInterpreter.hs
	ghc --make Main.hs ${FILES} -o interpreter -outputdir ${BUILD_DIR}

.PHONY:
clean:
	rm -f ${BUILD_DIR}/*
