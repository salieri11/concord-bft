CC=g++
CFLAGS=--std=c++11
EXEC=log_example
LIBS=-lpthread -llog4cplus

example: log_example.cc
	$(CC) -o log_example $(CFLAGS) log_example.cc $(LIBS)

clean:
	rm -f $(EXEC)
