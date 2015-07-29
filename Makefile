

INCLUDES :=
INCLUDES := ../stb

CCFLAGS := $(addprefix -I,$(INCLUDES))

RACKETDIR := /Applications/Racket\ v6.0.1/
RACKETBINDIR := $(RACKETDIR)/bin
RACKETBIN := $(RACKETBINDIR)/racket

OBJS :=
OBJS += vm.o

%.o : %.cpp
	@echo $@
	clang++ ${CCFLAGS} -c $<

all: main test.bin

test: all
	${RACKETBINDIR}/raco test *.rkt
	./main test

clean:
	rm -f ${OBJS} main

main: ${OBJS}
	@echo $@
	clang++ -o $@ ${OBJS}


test.bin: stream.rkt integer.rkt crc32.rkt
	${RACKETBIN} stream.rkt
