

INCLUDES :=
INCLUDES := ../stb

CCFLAGS := $(addprefix -I,$(INCLUDES))

RACKETDIR := /Applications/Racket\ v6.0.1/
RACKETBIN := $(RACKETDIR)/bin/racket

OBJS :=
OBJS += vm.o

%.o : %.cpp
	@echo $@
	clang ${CCFLAGS} -c $<

all: main

clean:
	rm -f ${OBJS} main

main: ${OBJS} test.bin
	@echo $@
	clang -o $@ ${OBJS}


test.bin: stream.rkt
	${RACKETBIN} stream.rkt
