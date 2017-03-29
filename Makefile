all: static/pixiv.js

docs: documentation.json

cloc:
	cloc . --exclude-dir=elm-stuff,static,var --force-lang=Ruby,ru --include-lang=Ruby,make,Elm

static/pixiv.js:
	elm make Main.elm --output static/pixiv.js

documentation.json:
	elm make Main.elm --output /dev/null --docs=documentation.json

.PHONY: static/pixiv.js documentation.json
