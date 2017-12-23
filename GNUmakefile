# Variables

# Normal compilation flags
CXX := clang++
CC := clang
CPPFLAGS := $(CPPFLAGS) -Wall -Wextra -Wno-unused-parameter -Wno-implicit-fallthrough -I. -Ivendor -Ivendor/linenoise -Ivendor/dynasm
CFLAGS := $(CFLAGS) -g3 -O3 
CXXFLAGS := $(CPPFLAGS) -std=c++14 -fno-rtti -fno-exceptions $(CFLAGS) -fpermissive
LDFLAGS := -fno-rtti -fno-exceptions

# Emscripten
ECXX := em++
ECPPFLAGS := $(CPPFLAGS) 
ECXXFLAGS := -s ASSERTIONS=1 -s EMULATE_FUNCTION_POINTER_CASTS=1 -Os $(ECPPFLAGS) -std=c++14 -fno-rtti -fno-exceptions -DAR_LINENOISE=0 -DARETE_LOG_TAGS="(ARETE_LOG_TAG_IMAGE|ARETE_LOG_TAG_DEFUN)"
ELDFLAGS := -Os -s ASSERTIONS=1 -s EMULATE_FUNCTION_POINTER_CASTS=1

# Retrieve compilation targets
CXXOBJS := $(filter-out src/compile-x64.o,$(filter-out src/main.o,$(patsubst %.cpp,%.o,$(wildcard src/*.cpp )))) src/compile-x64.o
ECXXOBJS := $(patsubst %.o,%.em.o,$(CXXOBJS))
CXXOBJS := $(CXXOBJS) $(patsubst %.cpp,%.o,$(wildcard vendor/linenoise/*.cpp))
CXXOBJS32 := $(patsubst %.o,%.32.o,$(CXXOBJS))
DEPS := $(CXXOBJS:.o=.d) $(CXXOBJS32:.o=.d) $(ECXXOBJS:.o=.d) src/main.d 

# Files to include with Emscripten builds
EFILES := $(addprefix --preload-file ,$(wildcard heap32.boot *.scm scheme/*.scm examples/*.scm examples/*.ttf))

# 
PREFIX = /usr/local/stow/arete

# Libraries to build

ifeq ($(OS),Windows_NT)
	ARETE_LIBS := 
	DASMFLAGS := -D WINDOWS
	MATH :=	
else
	ARETE_LIBS := sdl
	DASMFLAGS :=
	MATH := -lm
endif

# site.mk allows the user to override settings
-include site.mk

LDFLAGS := $(MATH) $(LDFLAGS)

ifeq ($(findstring sdl,$(ARETE_LIBS)),sdl)
	CXXFLAGS := $(CXXFLAGS) $(shell pkg-config --cflags sdl2 SDL2_ttf) -DAR_LIB_SDL=1
	ECXXFLAGS := $(CXXFLAGS) $(shell pkg-config --cflags sdl2 SDL2_ttf) -DAR_LIB_SDL=1 -s USE_SDL=2 -s USE_SDL_TTF=2
	LDFLAGS := $(LDFLAGS) $(shell pkg-config --libs sdl2 SDL2_ttf)
	ELDFLAGS := $(ELDFLAGS) -s USE_SDL=2 -s USE_SDL_TTF=2 -s NO_EXIT_RUNTIME=1
else
	CXXFLAGS := $(CXXFLAGS) -DAR_LIB_SDL=0
endif

# Fancy color compilation
define colorecho
      @echo -n $1
endef

# Compile .cpp files
%.o: %.cpp
	$(call colorecho, "CC $< ")
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $< 

%.em.o: CPPFLAGS := $(CPPFLAGS) -DAR_LINENOISE=0
%.em.o: %.cpp 
	$(call colorecho, "EMCC $< ")
	$(ECXX) $(ECXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $<

%.32.o: %.cpp
	$(call colorecho "CC32 $< ")
	$(CXX) $(CXXFLAGS) -m32 -MMD -MF $(patsubst %.o,%.d,$@) -c -o $@ $<

%.o: %.c
	$(call colorecho, "CC $< ")
	$(CC) $(CFLAGS) -c -o $@ $<

all: bin/arete

-include $(DEPS)

# Link 
vendor/dynasm/minilua: vendor/dynasm/minilua.c
	$(CC) -O2 -o $@ $< $(MATH)

src/compile-x64.cpp: src/compile-x64.cpp.dasc vendor/dynasm/minilua 
	vendor/dynasm/minilua vendor/dynasm/dynasm.lua $(DASMFLAGS) -D X64 -o $@ $< 

# Compile this to assembly for examination
src/compile-x64.S: src/compile-x64.cpp
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -S -o $@ $< 

bin/arete: $(CXXOBJS) src/main.o
	$(call colorecho, "LD $@ ")
	$(CXX) $(LDFLAGS) -o $@ $^ 

bin/arete32: $(CXXOBJS32) src/main.32.o
	$(call colorecho, "LD32 $@ ")
	$(CXX) $(LDFLAGS) -m32 -o $@ $^

heap.boot: bin/arete $(CXXOBJS) $(wildcard scheme/*.scm)
	$(call colorecho, "IMG $@ ")
	bin/arete scheme/expand.scm scheme/syntax.scm scheme/rules.scm scheme/compiler.scm --eval "(pull-up-bootstraps)" --save-image $@

heap32.boot: bin/arete32 $(wildcard scheme/*.scm)
	$(call colorecho, "IMG $@ ")
	bin/arete32 scheme/expand.scm scheme/syntax.scm scheme/compiler.scm --eval "(pull-up-bootstraps)" --save-image $@

web/arete.js: $(ECXXOBJS) src/main.em.o heap32.boot
	$(call colorecho, "LD $@ ")
	$(ECXX) $(ELDFLAGS) -o $@ $(ECXXOBJS) src/main.em.o $(EFILES)


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

.PHONY: count clean cleaner install

count:
	cloc arete.hpp $(wildcard src/*.cpp) $(wildcard scheme/*.scm)

clean:
	rm -f arete test $(wildcard src/*.o tests/*.o)

cleaner: clean
	rm -f $(wildcard vendor/linenoise/*.d vendor/linenoise/*.o)

install: arete heap.boot
	install -d $(PREFIX)/bin
	install -m 0755 arete $(PREFIX)/bin
	install -m 0644 heap.boot $(PREFIX)/share/arete
