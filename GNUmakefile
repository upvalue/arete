-include site.mk

# Variables
CXX := clang++
CXXFLAGS := -std=c++11 -Wall -g3 -Os -fno-exceptions -fno-rtti
LDFLAGS := -g3 

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


all: test

test.o: test.cpp arete.hpp

# Link 
test: test.o 
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $<

.PHONY: count clean

count:
	cloc arete.hpp
