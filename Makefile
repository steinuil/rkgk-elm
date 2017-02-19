all:
	elm make Main.elm --output pixiv.js

docs:
	elm make Main.elm --output pixiv.js --docs=documentation.json

warn:
	elm make Main.elm --output pixiv.js --warn
