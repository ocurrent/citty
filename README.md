# A WIP terminal UI for OCaml-ci

### 1. Clone the project 

```bash
git clone --recursive https://github.com/ocaml-ci/citty.git
```

Note: if you missed the `--recursive` flag, run `git submodule init`

### 2. Install dependencies

```bash
opam pin add --yes ocaml-ci/ocurrent/
opam install --yes --deps-only ./citty.opam
```

### 2. Install a capability file

For now, Citty assumes that the capability is stored in `~/.ocaml-ci.cap`.

### 3. Run!

```bash
make run
# dune exec ./src/main.bc
```
### 4. Quit it

`(alt,meta,...)-q`, or just kill the process :-).
TODO: add a clean way to exit.
