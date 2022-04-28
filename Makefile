build:
	npx spago bundle-app
	rm -f dist/*.js dist/*.map
	exec npx --node-arg=--max-old-space-size=4096 parcel build index.html

run:
	npx spago bundle-app
	python3 -m http.server

.PHONY: build