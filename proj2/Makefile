#FLP Project 2: Turing machine
#Author: Ladislav Dokoupil
#Login: xdokou14
#Year: 2023

PROLOG = swipl
TARGET = flp22-log
FLAGS = -q -g main
SRCS = $(wildcard src/*.pl)

.PHONY: all clean zip

all: $(TARGET)

$(TARGET): $(SRCS)
	$(PROLOG) $(FLAGS) -o $@ -c $^

clean:
	rm -f $(TARGET) $(TARGET).zip

zip: Makefile $(SRCS) test/* doc/*
	zip $(TARGET).zip $^
