PREREQS_IMAGE:= ${shell grep -o "athena-docker-local\.artifactory\.eng\.vmware\.com\/concord-core:.*" ./Dockerfile}
CONCORD_IMAGE_NAME:=concord-core
CONCORD_IMAGE_VERSION:=latest
CONCORD_BUILD_DIR:=build
CONCORD_CONTAINER_SHELL:=/bin/bash
CONCORD_WORK_DIR:=/concord
CONCORD_BUILD_CONTAINER:=concord-build
CONCORD_CMAKE_FLAGS:= \
			  -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
			  -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7

CONCORD_CTEST_TIMEOUT:=3000
CONCORD_USER_GROUP:=--user `id -u`:`id -g`

CONCORD_ADDITIONAL_RUN_PARAMS:=

BASIC_RUN_PARAMS:=-it --init --rm \
				  --name=${CONCORD_BUILD_CONTAINER} \
				  --workdir=${CONCORD_WORK_DIR} \
				  --mount type=bind,source=${CURDIR},target=${CONCORD_WORK_DIR} \
				  --mount type=bind,source=${CURDIR}/../communication,target=/communication \
				  ${CONCORD_ADDITIONAL_RUN_PARAMS} \
				  ${PREREQS_IMAGE} \

.DEFAULT_GOAL:=build

# MakefileCustom may be useful for overriding the default variables
# or adding custom targets. The include directive is ignored if MakefileCustom file does not exist.
-include MakefileCustom

IF_CONTAINER_RUNS=$(shell docker container inspect -f '{{.State.Running}}' ${CONCORD_BUILD_CONTAINER} 2>/dev/null)

.PHONY: help
help: ## The Makefile helps to build Concord in a docker container
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: login
login: ## Login to the container
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		docker run ${BASIC_RUN_PARAMS} \
			${CONCORD_CONTAINER_SHELL};exit 0; \
	else \
		docker exec -it ${CONCORD_BUILD_CONTAINER} \
			${CONCORD_CONTAINER_SHELL};exit 0; \
	fi

.PHONY: build
build: ## Build Concord-BFT source. Note: this is the default target
	docker run ${CONCORD_USER_GROUP} ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make -j $$(nproc) all"
	@echo
	@echo "Build finished. The binaries are in ${CURDIR}/${CONCORD_BUILD_DIR}"

.PHONY: operator
operator:
	docker run ${CONCORD_USER_GROUP} ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make -j $$(nproc) operator_server"
	@echo
	@echo "Build finished. The binaries are in ${CURDIR}/${CONCORD_BUILD_DIR}"

.PHONY: format
format: ## Format Concord source
	docker run ${CONCORD_USER_GROUP} ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make format-concord"
	@echo
	@echo "Format finished."

.PHONY: test
test: ## Run all tests
	docker run ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: single-test
single-test: ## Run a single test `make single-test TEST_NAME=<test name>`
	docker run ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest -R ${TEST_NAME} --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: clean
clean: ## Clean Concord build directory
	docker run ${BASIC_RUN_PARAMS} \
		${CONCORD_CONTAINER_SHELL} -c \
		"rm -rf ${CONCORD_BUILD_DIR}"

.PHONY: build-docker-image
build-docker-image: ## Build Concord image used by Hermes tests
	docker build -f Dockerfile .. -t ${CONCORD_IMAGE_NAME}:${CONCORD_IMAGE_VERSION}
