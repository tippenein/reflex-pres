.PHONY: build assets deploy

assets:
	npm run scss
	cp static/*.wav dist/
build:
	stack build && npm run copy:stackbuild
deploy:
	scp -r dist/* tippenein@deltadrome.us:/www/data/pantheon/tea/
