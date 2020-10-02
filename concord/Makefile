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
CONCORD_ADDITIONAL_RUN_COMMANDS:=

.DEFAULT_GOAL:=build

# MakefileCustom may be useful for overriding the default variables
# or adding custom targets. The include directive is ignored if MakefileCustom file does not exist.
-include MakefileCustom

IF_CONTAINER_RUNS=$(shell docker container inspect -f '{{.State.Running}}' ${CONCORD_BUILD_CONTAINER})

.PHONY: help
help: ## The Makefile helps to build Concord in a docker container
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

	# Basic HOW-TO:
	# make test               # Run tests
	# make remove-c           # Remove existing container
	# make build-docker-image # Build Concord image

.PHONY: run-c
run-c: ## Run container in background
	docker run -d --rm \
			  --name=${CONCORD_BUILD_CONTAINER} \
			  --workdir=${CONCORD_WORK_DIR} \
			  --mount type=bind,source=${CURDIR},target=${CONCORD_WORK_DIR} \
			  --mount type=bind,source=${CURDIR}/../communication,target=/communication \
			  ${CONCORD_ADDITIONAL_RUN_PARAMS} \
			  ${PREREQS_IMAGE} \
			  ${CONCORD_CONTAINER_SHELL} -c \
			  "${CONCORD_ADDITIONAL_RUN_COMMANDS} \
			  /usr/bin/tail -f /dev/null"
	@echo
	@echo "The container \"${CONCORD_BUILD_CONTAINER}\" with the build environment is started as daemon."
	@echo "Run \"make stop-c\" to stop or \"make remove-c\" to delete."

login: ## Login to the container
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL};exit 0

.PHONY: stop-c
stop-c: ## Stop the container
	docker container stop ${CONCORD_BUILD_CONTAINER}
	@echo
	@echo "The container \"${CONCORD_BUILD_CONTAINER}\" is successfully stopped."

.PHONY: remove-c
remove-c: ## Remove the container
	docker container rm -f ${CONCORD_BUILD_CONTAINER}
	@echo
	@echo "The container \"${CONCORD_BUILD_CONTAINER}\" is successfully removed."

.PHONY: build
build: ## Build Concord source. Note: this command is mostly for developers
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it ${CONCORD_USER_GROUP} --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make -j $$(nproc) all"
	@echo
	@echo "Build finished. The binaries are in ${CURDIR}/${CONCORD_BUILD_DIR}"

.PHONY: operator
operator:
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it ${CONCORD_USER_GROUP} --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make -j $$(nproc) operator_server"
	@echo
	@echo "Build finished. The binaries are in ${CURDIR}/${CONCORD_BUILD_DIR}"

.PHONY: format
format: ## Format Concord source
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it ${CONCORD_USER_GROUP} --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make format-concord"
	@echo
	@echo "Format finished."

.PHONY: test
test: ## Run all tests
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: single-test
single-test: ## Run single test `make single-test TEST_NAME=<test name>`
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest -R ${TEST_NAME} --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: clean
clean: ## Clean Concord build directory
	@if [ "${IF_CONTAINER_RUNS}" != "true" ]; then \
		make run-c; \
	fi
	docker exec -it --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"rm -rf ${CONCORD_BUILD_DIR}"

.PHONY: build-docker-image
build-docker-image: ## Build Concord image used by Hermes tests
	docker build -f Dockerfile .. -t ${CONCORD_IMAGE_NAME}:${CONCORD_IMAGE_VERSION}
