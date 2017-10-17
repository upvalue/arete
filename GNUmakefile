# Variables
CXX := clang++
CPPFLAGS := -Wall -I. -Ivendor -Ivendor/linenoise -DARETE_DEV
CFLAGS := $(CFLAGS) -g3 -O3
CXXFLAGS := $(CPPFLAGS) -std=c++14 -fno-rtti $(CFLAGS)
LDFLAGS := $(LDFLAGS) 

CXXOBJS := $(filter-out src/main.o,$(patsubst %.cpp,%.o,$(wildcard src/*.cpp vendor/linenoise/*.cpp)))
DEPS := $(CXXOBJS:.o=.d)

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

%.o: %.c
	$(call colorecho, "CC $< ")
	$(CC) $(CFLAGS) -c -o $@ $<

all: arete 

-include $(DEPS)

# Link 
arete: $(CXXOBJS) src/main.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

tests/test-semispace: $(CXXOBJS) tests/test-semispace.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

#arete: $(CXXOBJS) 
#	$(call colorecho, "LD $@ ")
#	$(CXX) $(LDFLAGS) -o $@ $^
#test: test.o vendor/linenoise.o
#	$(call colorecho, "LD $@ ")
#	$(CXX) $(LDFLAGS) -o $@ $^

#tests/test-semispace: tests/test-semispace.o 
#	$(call colorecho, "LD $@ ")
#	$(CXX) $(LDFLAGS) -o $@ $^

#tests/test-incremental: tests/test-incremental.o 
#	$(call colorecho, "LD $@ ")
#	$(CXX) $(LDFLAGS) -o $@ $^

#test-all: tests/test-incremental tests/test-semispace
#	tests/test-incremental
#	tests/test-semispace
#	python utils/run-tests.py

.PHONY: count clean

count:
	cloc arete.hpp $(wildcard src/*.cpp) boot.scm

clean:
	rm -f arete test $(patsubst *.o,*.d,$(CXXOBJS)) $(CXXOBJS) src/main.o tests/test-semispace tests/test-incremental
