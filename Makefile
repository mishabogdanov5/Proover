build: 
	dune build

test: 
	dune runtest

watch: 
	dune runtest -w

clean:
	@dune clean