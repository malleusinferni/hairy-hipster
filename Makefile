# TODO Switch to cabal
test: run clean

build: *.hs
	ghc -W Main.hs -o dungeon

run: build
	./dungeon

clean:
	find . -type f \( -name '*.hi' -or -name '*.o' \) -delete
	rm -f dungeon
