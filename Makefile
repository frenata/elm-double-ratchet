SRC = src
BUILD = build
MAIN = Ratchet
ASSETS = assets
CSS = $(SRC)/css

all: build 

build: build-directory js html asset

build-directory:
	mkdir -p $(BUILD)

js:
	elm-make src/$(MAIN).elm --output $(BUILD)/app.js
	uglifyjs $(BUILD)/app.js --compress --mangle \
		--output $(BUILD)/app.min.js 2> /dev/null
	mv $(BUILD)/app.min.js $(BUILD)/app.js

html:
	cp $(SRC)/index.html $(BUILD)/index.html

asset: 
	cp $(ASSETS)/* $(BUILD)

css:
	cat $(CSS)/*.css > $(BUILD)/style.css

push:
	git push

deploy:
	cp CNAME $(BUILD)/CNAME
	surge $(BUILD)

clean:
	rm -rf $(BUILD)
