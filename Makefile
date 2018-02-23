CC=g++

build:
	$(CC) -o RunTests *.cpp --std=c++11

runCoreVMTests:
	./RunTests -t CoreVMTests

runAllTests: runCoreVMTests

clean:
	rm -f RunTests
