all: clean build

.PHONY: build
build:
	npm run build --silent

.PHONY: clean
clean:
	rm -rf dist/

.PHONY: serve
serve:
	npm run dev --silent
