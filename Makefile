all: repl run

repl:
	cd src && ghc --make Repl.hs -outputdir ../bin -o ../bin/repl

run: 
	rm bin/Main.*
	cd src && ghc --make Run.hs -outputdir ../bin -o ../bin/run
	
.PHONY: all repl run
