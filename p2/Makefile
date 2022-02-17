
CC=gcc
CFLAGS=-Wall -pthread -g
LIBS=
OBJS= producer.o options.o list.o

PROGS= producer

all: $(PROGS)

%.o : %.c
	$(CC) $(CFLAGS) -c $<

producer: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROGS) *.o *~

