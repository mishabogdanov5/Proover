build: 
	dune build
tests: test

test: 
	dune runtest

watch: 
	dune runtest -w

clean:
	@dune clean