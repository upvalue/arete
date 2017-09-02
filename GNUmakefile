-include site.mk

# Variables
CXX := clang++
CFLAGS := -g3 -O3
CXXFLAGS := -std=c++11 -fno-exceptions -fno-rtti $(CFLAGS)
LDFLAGS := -g3  -O3

# Fancy color compilation
define colorecho
      @tput setaf 3
      @echo -n $1
      @tput sgr0
endef

# Compile .cpp files
%.o: %.cpp
	$(call colorecho, "CC $< ")
	$(CXX) $(CXXFLAGS) -c -o $@ $<

%.o: %.c
	$(call colorecho, "CC $< ")
	$(CC) $(CFLAGS) -c -o $@ $<

all: arete test

cli.o: cli.cpp arete.hpp
test.o: test.cpp arete.hpp

# Link 
arete: cli.o linenoise.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

test: test.o linenoise.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

.PHONY: count clean

count:
	cloc arete.hpp
