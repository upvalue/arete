# Variables
CXX := clang++
CPPFLAGS := -Wall -I. -Ivendor
CFLAGS := $(CFLAGS) -g3 -O3
CXXFLAGS := $(CPPFLAGS) -std=c++11 -fno-exceptions -fno-rtti $(CFLAGS)
LDFLAGS := $(LDFLAGS) -g3  -O3

-include site.mk

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

all: arete

cli.o: cli.cpp arete.cpp arete.hpp
test.o: test.cpp arete.cpp arete.hpp
tests/test-semispace.o: tests/test-runtime.cpp arete.cpp arete.hpp
tests/test-incremental.o: tests/test-runtime.cpp arete.cpp arete.hpp

# Link 
arete: cli.o vendor/linenoise.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

test: test.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

tests/test-semispace: tests/test-semispace.o 
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

tests/test-incremental: tests/test-incremental.o 
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

test-all: tests/test-incremental tests/test-semispace
	tests/test-incremental
	tests/test-semispace
	python utils/run-tests.py

.PHONY: count clean

count:
	cloc arete.hpp arete.cpp boot.scm

clean:
	rm -f arete test *.o
