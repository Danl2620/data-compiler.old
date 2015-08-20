

INCLUDES :=
INCLUDES := ../stb

CCFLAGS := $(addprefix -I,$(INCLUDES)) -std=c++11

##RACKETDIR := /Applications/Racket\ v6.0.1/
##RACKETBINDIR := $(RACKETDIR)/bin
RACKET := `which racket`  ##$(RACKETBINDIR)/racket
RACO := `which raco`

OBJS :=
OBJS += vm.o
OBJS += string-utils.o

%.o : %.cpp
	@echo $@
	clang++ ${CCFLAGS} -c $<

all: main test.bin

test: all
	${RACO} test *.rkt
	./main test

clean:
	rm -f ${OBJS} main

main: ${OBJS}
	@echo $@
	clang++ -o $@ ${OBJS}


test.bin: stream.rkt integer.rkt crc32.rkt
	${RACKET} stream.rkt
