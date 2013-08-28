# TODO Switch to cabal
test: run clean

build: *.hs
	ghc -W Main.hs -o dungeon

run: build
	./dungeon

clean:
	rm *.o *.hi dungeon
