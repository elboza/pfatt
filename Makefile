SRCDIR:=src
TARGET=pfatt
TMP=tmp
F=

.PHONY: all help build clean sm sl

all: help

show:
	@echo "http://localhost:3000/pfatt.php?file=fatt_fsecure_201910.json"
	php -S localhost:3000 -r .

build:
	$(MAKE) -C $(SRCDIR) $@
	cp $(SRCDIR)/$(TARGET) .

clean:
	$(MAKE) -C $(SRCDIR) $@
	rm *.html *.json
	echo "clean"

html:
	./pfatt -w $(F) -t html > $(TMP).html

json:
	./pfatt -w $(F) | jq . > $(TMP).json

sm: html
	open -a Google\ Chrome $(TMP).html

sl: html
	chromium $(TMP).html

sw: html
	explorer.exe $(TMP).html

help:
	@echo "show: start server"
	@echo "sm: show on osx"
	@echo "sl: show on linux"
	@echo "help: this help."
	@echo "clean: clean files"
	@echo ""
	@echo "parameters:"
	@echo "F=filename"
	@echo "example: make F=abc.json sm"

