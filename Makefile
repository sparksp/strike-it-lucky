all: clean build

.PHONY: build
build:
	yarn run build --silent

.PHONY: clean
clean:
	rm -rf dist/

.PHONY: serve
serve:
	yarn run dev --silent
