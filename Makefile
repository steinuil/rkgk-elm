all:
	elm make Main.elm --output pixiv.js

warn:
	elm make Main.elm --output pixiv.js --warn
