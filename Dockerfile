FROM ocaml/opam:ubuntu
WORKDIR /app
COPY . .
RUN opam update
RUN opam upgrade
RUN opam install ppx_expect
RUN opam install angstrom
RUN opam install . --deps-only
RUN eval $(opam env)
CMD ["dune", "build"]