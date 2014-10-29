SRC=$(wildcard src/*.cpp)
OBJS=$(SRC:.cpp=.o)
CPPFLAGS=-Wall -O2 -I./include

zebibot: $(OBJS)
	g++ $(CPPFLAGS) -o $@ $(OBJS)

clean:
	rm -f $(OBJS) zebibot

.PHONY: zebibot

