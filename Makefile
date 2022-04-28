build:
	npx spago bundle-app
	mkdir -p dist
	cp {index.html,index.js}

run:
	npx spago bundle-app
	python3 -m http.server

.PHONY: build