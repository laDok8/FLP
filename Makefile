CC=ghc
TARGET=flp22-fun
FLAG=-fwarn-incomplete-patterns -Wall
PACK=flp-fun-xdokou14.zip

.PHONY: build run zip clean
build: $(TARGET)

$(TARGET): src/*.hs
	ghc --make -o $@ $(FLAG) $^

run: $(TARGET)
	./$<

zip: Makefile src/* doc/* test/*
	zip -r $(PACK) $^

clean:
	rm -f $(PACK).zip $(TARGET) src/*.hi src/*.o
