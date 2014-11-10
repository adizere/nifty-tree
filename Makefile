

all: main playground

main: src/Main.hs
	@echo "\t${COLOR}Compiling Main${NO_COLOR}"
	ghc -O2 --make -isrc/ src/Main.hs -threaded -rtsopts -o bin/Main

playground: playground/*.hs
	for file in playground/*.hs ; do \
		fname=$${file##*/} ; base=$${fname%%.*} ; \
		echo "\t${COLOR}Compiling playground $${base} file ${NO_COLOR}" ; \
		ghc -o bin/$${base} $$file ; \
	done

clean:
	rm -f bin/*
	rm -f playground/*.hi
	rm -f src/*.hi src/*.o

.PHONY: playground

NO_COLOR=\x1b[0m
COLOR=\x1b[0;36m