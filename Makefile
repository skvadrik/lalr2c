O            ?= obj
HI           ?= hi
PROGRAM       = lalr2c
GHCFLAGS     += -O2 -hidir $(HI) -odir $(O) -i$(O):$(HI) $(PROF) -rtsopts
GHCFLAGS     += -Wall -W -fno-warn-name-shadowing

G2HS_SOURCES       = Grammar.g
G2HS_AUTOGENERATED = $(G2HS_SOURCES:%.g=%.hs)
G2HS_OBJECTS       = $(G2HS_SOURCES:%.g=$(O)/%.o)
G2HS_INTERFACES    = $(G2HS_SOURCES:%.g=$(HI)/%.hi)

SOURCES             = Types.hs LALR.hs Codegen.hs lalr2c.hs $(G2HS_AUTOGENERATED)
OBJECTS             = $(SOURCES:%.hs=$(O)/%.o) Main.o
INTERFACES          = $(SOURCES:%.hs=$(HI)/%.hi) Main.hi




all: $(PROGRAM)

$(PROGRAM): $(SOURCES)
	ghc --make $(GHCFLAGS) lalr2c.hs

%.hs: %.g
	./g2hs.hs $< $@ || rm $@




.PHONY: clean clean_g2hs clean_all

clean:
	-rm -fv $(PROGRAM)

clean_g2hs:
	-rm -fv $(G2HS_AUTOGENERATED) $(G2HS_OBJECTS) $(G2HS_INTERFACES) $(PROGRAM)

clean_all:
	-rm -fv $(G2HS_AUTOGENERATED) $(G2HS_OBJECTS) $(G2HS_INTERFACES) $(PROGRAM)

.SUFFIXES:

.PRECIOUS: $(G2HS_AUTOGENERATED)
