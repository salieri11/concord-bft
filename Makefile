CC=g++

build:
	$(CC) -o RunTests --std=c++11 -I.. src/*.cpp

run_core_vm_tests:
	./RunTests -t CoreVMTests

run_all_tests: run_core_vm_tests

clean:
	rm -f RunTests
