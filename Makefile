all:
	dune build ./src/main.bc

run:
	dune build ./src/main.bc
	dune exec ./src/main.bc 2> error.log
	cat error.log

test-stress:
	dune exec ./lib/tests/stress.exe

test-reranger:
	dune exec ./lib/tests/reranger.bc

test-misc:
	dune exec ./lib/tests/misc.bc

clean:
	dune clean
