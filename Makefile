SRCDIR := src/
INCLUDEDIR := include/

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -std=c++11 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -std=c++11 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -std=c++11 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(INCLUDEDIR)
CXXFLAGS += -fPIC -I $(INCLUDEDIR)

LDLIBS += -L $(SRCDIR) -lstdc++ -lboost_system -lboost_program_options -lprotobuf -llog4cplus
LDFLAGS +=

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

PROTOS := $(shell find $(SRCDIR) -type f \( -name "*.proto" \))
PROTOSRCS := $(addsuffix .pb.cc, $(basename $(PROTOS)))
PROTOOBJS := $(addsuffix .o, $(basename $(PROTOSRCS)))

SOURCES := $(shell find $(SRCDIR) -type f \( -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -g

athena: $(PROTOOBJS) $(OBJECTS)
	$(link_verbose) $(CC) $^ $(LDFLAGS) $(LDLIBS) -o $@

%.pb.cc: %.proto
	protoc --proto_path=$(SRCDIR) --cpp_out=$(SRCDIR) $<

%.pb.o: %.pb.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f athena $(OBJECTS) $(PROTOOBJS) $(PROTOSRCS) $(SRCDIR)/*.pb.h
