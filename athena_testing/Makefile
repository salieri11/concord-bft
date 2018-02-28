CC=g++
LIBS=-lpthread -llog4cplus -lboost_program_options
build:
	$(CC) -o CoreVMTests --std=c++11 -I../.. src/*.cpp $(LIBS) ../../hermes/src/lib/*.cpp

run_core_vm_tests:
	cp -r ../resources .
	./CoreVMTests ${ARGS}

clean:
	rm -rf CoreVMTests ./resources
