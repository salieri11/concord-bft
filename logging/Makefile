CC=g++
CFLAGS=--std=c++11
EXEC=log_example
OBJ=athenalogger.o
LIBS=-lpthread -llog4cplus

example: athena_logger
	$(CC) -o log_example $(CFLAGS) athenalogger.o log_example.cc $(LIBS)

athena_logger: athenalogger.h
	$(CC) -c athenalogger.cc $(CFLAGS) -o athenalogger.o

clean:
	rm -f $(EXEC) $(OBJ)
