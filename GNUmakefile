# Variables
CXX := g++
CPPFLAGS := $(CPPFLAGS) -Wall -I. -Ivendor -Ivendor/linenoise -DARETE_DEV
CFLAGS := $(CFLAGS) -g3 -O3
CXXFLAGS := $(CPPFLAGS) -std=c++14 -fno-rtti $(CFLAGS) 
LDFLAGS := 

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
	$(CXX)  -o $@ $^ $(LDFLAGS)

arete.html: $(ECXXOBJS) src/main.em.o
	$(call colorecho, "LD $@ ")
	$(ECXX) $(LDFLAGS) -o $@ $^

tests/test-semispace: $(CXXOBJS) tests/test-semispace.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^

#test-all: tests/test-incremental tests/test-semispace
#	tests/test-incremental
#	tests/test-semispace
#	python utils/run-tests.py

.PHONY: count clean

count:
	cloc arete.hpp $(wildcard src/*.cpp) $(wildcard *.scm)

clean:
	rm -f arete test $(patsubst %.o,%.d,$(CXXOBJS)) $(wildcard $(CXXOBJS)) $(wildcard $(ECXXOBJS)) $(wildcard $(CXXOBJS)) $(wildcard src/main.o tests/test-semispace tests/test-incremental)

cleaner:
	rm -f $(wildcard vendor/linenoise/*.d vendor/linenoise/*.o)
