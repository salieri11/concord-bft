CC=g++

build:
	$(CC) -o CoreVMTests --std=c++11 -I../.. -I.. src/*.cpp ../../hermes/src/lib/*.cpp

run_core_vm_tests:
	./CoreVMTests

clean:
	rm -f CoreVMTests
