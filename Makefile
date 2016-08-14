

INCLUDES :=
INCLUDES := ../stb

CCFLAGS := $(addprefix -I,$(INCLUDES)) -std=c++11

COMPILED_DIR := "$(CURDIR)/bin/zos/@(version)"

##RACKETDIR := /Applications/Racket\ v6.0.1/
##RACKETBINDIR := $(RACKETDIR)/bin
RACKET := racket -R $(COMPILED_DIR)  ##$(RACKETBINDIR)/racket
RACO := $(RACKET) -l- raco

CPP_SRCS := $(wildcard rt/*.cpp)
CPP_OBJS := $(subst rt/,bin/,$(CPP_SRCS:.cpp=.o))

FRACAS_SRCS := $(wildcard src/*.frc)
FRACAS_TARGETS := $(subst src/,bin/,$(FRACAS_SRCS:.frc=.bin))


bin/%.o : rt/%.cpp
	@echo $@
	@mkdir -p $(dir $@)
	@clang++ ${CCFLAGS} -c $< -o $@

bin/%.bin : src/%.frc
	@echo $@
	@mkdir -p $(dir $@)
	@$(RACO) make --vv lib/fracas/make-bin.rkt
##	@$(RACKET) -l fracas/make-bin -- $^

all: bin/main $(FRACAS_TARGETS)

test: all
	${RACO} test *.rkt
	bin/main test

clean:
	rm -f $(CPP_OBJS) bin/main
	rm -rf $(COMPILED_DIR)

bin/main: $(CPP_OBJS)
	@echo $@
	@clang++ -o $@ $^


test.bin: stream.rkt integer.rkt crc32.rkt
	${RACKET} stream.rkt
