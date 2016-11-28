.PHONY: build assets deploy

all: build assets deploy

assets:
	cp static/*.wav dist/
build:
	npm run build:dist
deploy:
	scp -r dist/* tippenein@deltadrome.us:/www/data/pantheon/tea/
