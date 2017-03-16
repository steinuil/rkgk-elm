all:
	elm make Main.elm --output static/pixiv.js

docs:
	elm make Main.elm --output static/pixiv.js --docs=documentation.json

warn:
	elm make Main.elm --output static/pixiv.js --warn
