all:
	build dist

build:
	stack build && npm run copy:stackbuild
dist:
	scp -r dist/* tippenein@deltadrome.us:/www/data/pantheon/tea/
