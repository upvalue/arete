# Variables
CXX := clang++
CPPFLAGS := $(CPPFLAGS) -Wall -Wextra -Wno-unused-parameter -I. -Ivendor -Ivendor/linenoise 
CFLAGS := $(CFLAGS) -g3 -O3
CXXFLAGS := $(CPPFLAGS) -std=c++14 -fno-rtti -fno-exceptions $(CFLAGS) 
LDFLAGS := -lm

ECXX := em++
ECPPFLAGS := $(CPPFLAGS) -DAR_LINENOISE=0
ECXXFLAGS := $(ECPPFLAGS) -std=c++14 -fno-rtti -fno-exceptions 
ELDFLAGS :=

CXXOBJS := $(filter-out src/main.o,$(patsubst %.cpp,%.o,$(wildcard src/*.cpp )))
ECXXOBJS := $(patsubst %.o,%.em.o,$(CXXOBJS))
CXXOBJS := $(CXXOBJS) $(patsubst %.cpp,%.o,$(wildcard vendor/linenoise/*.cpp))
DEPS := $(CXXOBJS:.o=.d) src/main.d

ARETE_LIBS := sdl test

# site.mk allows the user to override settings
-include site.mk

ifeq ($(findstring sdl,$(ARETE_LIBS)),sdl)
	CXXFLAGS := $(CXXFLAGS) $(shell pkg-config --cflags sdl2 SDL2_ttf) -DAR_LIB_SDL=1
	ECXXFLAGS := $(CXXFLAGS) $(shell pkg-config --cflags sdl2 SDL2_ttf) -DAR_LIB_SDL=1 -s USE_SDL=2 -s USE_SDL_TTF=2
	LDFLAGS := $(LDFLAGS) $(shell pkg-config --libs sdl2 SDL2_ttf)
	ELDFLAGS := -s USE_SDL=2 -s USE_SDL_TTF=2
else
	CXXFLAGS := $(CXXFLAGS) -DAR_LIB_SDL=0
endif

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

%.em.o: CPPFLAGS := $(CPPFLAGS) -DAR_LINENOISE=0
%.em.o: %.cpp 
	$(call colorecho, "CC $< ")
	$(ECXX) $(ECXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $<

%.o: %.c
	$(call colorecho, "CC $< ")
	$(CC) $(CFLAGS) -c -o $@ $<

all: arete

-include $(DEPS)

# Link 
arete: $(CXXOBJS) src/main.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^ 

arete.html: $(ECXXOBJS) src/main.em.o
	$(call colorecho, "LD $@ ")
	$(ECXX) -O3 $(ELDFLAGS) -o $@ $^ $(addprefix --preload-file ,$(wildcard bootstrap.scm scheme/*.scm examples/*.scm))

arete-distilled.cpp: $(wildcard src/*.cpp)
	$(call colorecho "arete.cpp")
	echo "// Automatically generated combination of Arete source files; do not edit" > $@
	echo "// See https://github.com/upvalue/arete for details" >> $@
	echo "// Generated on $(shell date)" >> $@
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
	cloc arete.hpp $(wildcard src/*.cpp) $(wildcard scheme/*.scm)

clean:
	rm -f arete test $(wildcard src/*.o tests/*.o)

cleaner: clean
	rm -f $(wildcard vendor/linenoise/*.d vendor/linenoise/*.o)
