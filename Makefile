CPPSRC=$(wildcard src/*.cpp)
CSRC=$(wildcard src/*.c)
OBJS=$(CPPSRC:.cpp=.o) $(CSRC:.c=.o)
CPPFLAGS=-O2 -std=c++11 -pthread -fstack-protector -fpermissive -I./include -I./gojira/include -I.
CFLAGS=-O2 -fstack-protector -I./include -I./gojira/include -I. -Wall

zebibot: linenoise/linenoise.o gojira/out/libgojira.o $(OBJS)
	g++ $(CPPFLAGS) -ljansson -lcurl -o $@ linenoise/linenoise.o gojira/out/libgojira.o $(OBJS)

gojira/out/libgojira.o:
	cd gojira; make out/libgojira.o

linenoise/linenoise.o:
	gcc $(CFLAGS) -c -o $@ linenoise/linenoise.c

clean:
	rm -f $(OBJS) zebibot
	cd gojira; make clean
	rm linenoise/linenoise.o

.PHONY: zebibot
