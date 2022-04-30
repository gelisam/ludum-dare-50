# instructions: run 'make build' inside the asterius container

all: instructions
.PHONY: all serve build clean clobber

instructions:
	@echo "run the following command to open an Asterius shell:"
	@echo "  podman run -it --rm -v \$$(pwd):/workspace --privileged -w /workspace terrorjack/asterius:latest"
	@echo ""
	@echo "then in that shell, run the following command to compile the Haskell code to html+js+wasm:"
	@echo "  make build"
	@echo ""
	@echo "then in a regular, non-Asterius shell, run the following command to serve those generated files:"
	@echo "  make serve"
	@echo ""
	@echo "then in a web browser, open the following URL:"
	@echo "  http://0.0.0.0:8000/tordle-exe.html"

serve:
	python3 -m http.server

HS_FILES = $(shell find src -type f -name '*.hs')

~/.ahc-cabal:
	ahc-cabal update

build: ~/.ahc-cabal $(HS_FILES) app/Main.hs tordle.cabal
	ahc-cabal new-install --overwrite-policy=always --installdir . tordle-exe
	ahc-dist --input-exe tordle-exe --browser --bundle

clean:
	rm -rf dist-newstyle
	rm -rf tordle-exe
	rm -rf *.mjs

clobber: clean
	rm -rf tordle-exe.*
