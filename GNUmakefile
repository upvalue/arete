# Variables
CXX := g++
CPPFLAGS := $(CPPFLAGS) -Wall -Wextra -Wno-unused-parameter -I. -Ivendor -Ivendor/linenoise 
CFLAGS := $(CFLAGS) -g3 -O3
CXXFLAGS := $(CPPFLAGS) -std=c++14 -fno-rtti -fno-exceptions $(CFLAGS) $(shell pkg-config --cflags sdl2) 
LDFLAGS := $(shell pkg-config --libs sdl2) 

ECXX := em++
ECPPFLAGS := $(CPPFLAGS) -DAR_LINENOISE=0
ECXXFLAGS := $(ECPPFLAGS)

CXXOBJS := $(filter-out src/main.o,$(patsubst %.cpp,%.o,$(wildcard src/*.cpp )))
ECXXOBJS := $(patsubst %.o,%.em.o,$(CXXOBJS))
CXXOBJS := $(CXXOBJS) $(patsubst %.cpp,%.o,$(wildcard vendor/linenoise/*.cpp))
DEPS := $(CXXOBJS:.o=.d) src/main.d

# site.mk allows the user to override settings
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
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $< 

%.em.o: %.cpp
	$(call colorecho, "CC $< ")
	$(ECXX) $(ECXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $<

%.o: %.c
	$(call colorecho, "CC $< ")
	$(CC) $(CFLAGS) -c -o $@ $<

#all: arete.html
all: arete

-include $(DEPS)

# Link 
arete: $(CXXOBJS) src/main.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^ 

arete.html: $(ECXXOBJS) src/main.em.o
	$(call colorecho, "LD $@ ")
	$(ECXX) $(LDFLAGS) -o $@ $^

arete-distilled.cpp: $(wildcard src/*.cpp)
	$(call colorecho "arete.cpp")
	echo "// Automatically generated combination of Arete source files; do not edit" > $@
	cat vendor/linenoise/*.h src/*.cpp vendor/linenoise/*.cpp >> $@
	sed -e "s/#include \"ConvertUTF.h\"//g" -i $@
	sed -e "s/#include \"linenoise.h\"//g" -i $@

tests/test-semispace: $(CXXOBJS) tests/test-semispace.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

test-all: tests/test-semispace
	#tests/test-incremental
	tests/test-semispace
	python utils/run-tests.py

.PHONY: count clean cleaner

count:
	cloc arete.hpp $(wildcard src/*.cpp) $(wildcard *.scm)

clean:
	rm -f arete test $(wildcard src/*.o tests/*.o)

cleaner: clean
	rm -f $(wildcard vendor/linenoise/*.d vendor/linenoise/*.o)
