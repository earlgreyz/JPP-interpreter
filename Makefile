BUILD_DIR=build
G_FILES=${BUILD_DIR}/{LexGrammar,AbsGrammar,ParGrammar,SkelGrammar,PrintGrammar,ErrM}.hs
I_FILES={IInterpreter,IUtil,IExec}.hs
T_FILES={TCheck,TUtil,TExec}.hs
M_FILES={MArray,MTuple,MString,MInt}.hs
FILES=Main.hs ${I_FILES} ${T_FILES} ${M_FILES}

all: interpreter

interpreter: grammar *.hs
	ghc --make -XFlexibleContexts ${FILES} ${G_FILES} -o interpreter -outputdir ${BUILD_DIR}

grammar: Grammar.cf
	mkdir -p ${BUILD_DIR}
	cabal exec -- bnfc -m -haskell Grammar.cf -o ${BUILD_DIR}
	cd build && make

fast: *.hs
	ghc --make -XFlexibleContexts ${FILES} ${G_FILES} -o interpreter -outputdir ${BUILD_DIR}

.PHONY:
clean:
	rm -f ${BUILD_DIR}/* interpreter
