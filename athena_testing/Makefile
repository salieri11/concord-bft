CC=g++
LIBS=-lpthread -llog4cplus -lboost_program_options
build:
	$(CC) -o CoreVMTests --std=c++11 -I../.. -I.. src/*.cpp $(LIBS) ../../hermes/src/lib/*.cpp

run_core_vm_tests:
	./CoreVMTests

clean:
	rm -rf CoreVMTests
