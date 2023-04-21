#FLP Project 2: Turing machine
#Author: Ladislav Dokoupil
#Login: xdokou14
#Year: 2023

PROLOG = swipl
TARGET = flp22-log
FLAGS = -q -g main
SRCS = $(wildcard src/*.pl)
PACK=flp-log-xdokou14.zip

.PHONY: all clean zip run

all: $(TARGET)

$(TARGET): $(SRCS)
	$(PROLOG) $(FLAGS) -o $@ -c $^

#run on all inps 
run:
	@for i in test/*.in; do \
		./$(TARGET) < $$i > $${i%.in}.out || true  ; \
	done

clean:
	rm -f $(TARGET) $(PACK) test/*.out

zip: Makefile $(SRCS) test/*.in doc/*
	zip $(PACK) $^
