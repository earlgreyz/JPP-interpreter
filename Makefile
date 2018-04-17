BUILD_DIR=build
GRAMMAR_FILES=${BUILD_DIR}/{LexGrammar,AbsGrammar,ParGrammar,SkelGrammar,PrintGrammar,ErrM}.hs
INTERPRETER_FILES={Main,MInterpreter,MUtil,MArray,MTuple,MString,MInt,MExec}.hs

all: interpreter

interpreter: grammar Main.hs MInterpreter.hs
	ghc --make ${INTERPRETER_FILES} ${GRAMMAR_FILES} -o interpreter -outputdir ${BUILD_DIR}

grammar: Grammar.cf
	mkdir -p ${BUILD_DIR}
	cabal exec -- bnfc -m -haskell Grammar.cf -o ${BUILD_DIR}
	cd build && make

fast: Main.hs MInterpreter.hs
	ghc --make ${INTERPRETER_FILES} ${GRAMMAR_FILES}  -o interpreter -outputdir ${BUILD_DIR}

.PHONY:
clean:
	rm -f ${BUILD_DIR}/*
