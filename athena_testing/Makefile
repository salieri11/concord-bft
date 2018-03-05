CC=g++
LIBS=-lpthread -llog4cplus -lboost_program_options -ljsoncpp
build:
	$(CC) -o CoreVMTests --std=c++11 -I../.. -I.. src/*.cpp ../../hermes/src/lib/*.cpp $(LIBS)

run_core_vm_tests:
	./CoreVMTests

clean:
	rm -rf CoreVMTests
