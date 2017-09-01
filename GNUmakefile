-include site.mk

# Variables
CXX := clang++
CXXFLAGS := -Wall -Werror -g3 -Os -fno-exceptions
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


all: arete

cli.o: cli.cpp arete.hpp

# Link 
arete: cli.o 
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $<

.PHONY: count clean

count:
	cloc arete.hpp
