all:
	dune build ./src/main.bc

run:
	dune exec ./src/main.bc

test-stress:
	dune exec ./lib/tests/stress.exe

test-reranger:
	dune exec ./lib/tests/reranger.bc

test-misc:
	dune exec ./lib/tests/misc.bc

clean:
	dune clean
