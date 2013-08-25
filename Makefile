# TODO Switch to cabal
test: run clean

build: *.hs
	ghc Main.hs -o dungeon

run: build
	./dungeon

clean:
	rm *.o *.hi dungeon
