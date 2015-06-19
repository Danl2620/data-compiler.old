

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
	clang ${CCFLAGS} -c $<

all: main test.bin

test: all
	${RACKETBINDIR}/raco test stream.rkt
	./main test.bin

clean:
	rm -f ${OBJS} main

main: ${OBJS}
	@echo $@
	clang -o $@ ${OBJS}


test.bin: stream.rkt
	${RACKETBIN} stream.rkt
