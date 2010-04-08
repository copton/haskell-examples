.PHONY: all clean
TARGETS=$(patsubst %.hs,%,$(wildcard *.hs))

all: $(TARGETS)

clean:
	rm -fr $(TARGETS) $(foreach ext, o hi, $(foreach file, $(TARGETS), $(file).$(ext)))

%: %.hs
	ghc -o $@ --make $<

