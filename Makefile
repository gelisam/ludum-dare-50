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
	cd dist-js && python3 -m http.server

HS_FILES = $(shell find src -type f -name '*.hs')

~/.ahc-cabal:
	ahc-cabal update

dist-js/tordle-exe: ~/.ahc-cabal $(HS_FILES) app/Main.hs tordle.cabal
	ahc-cabal new-install --flags=asterius --overwrite-policy=always --installdir dist-js tordle-exe

dist-js/tordle-exe.html: dist-js/tordle-exe
	ahc-dist --input-exe dist-js/tordle-exe --browser --bundle

build: dist-js/tordle-exe.html

clean:
	rm -rf dist-newstyle
	rm -rf dist-js/tordle-exe
	rm -rf dist-js/*.mjs

clobber: clean
	rm -rf dist-js
