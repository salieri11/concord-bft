PREREQS_IMAGE:= ${shell grep -o "athena-docker-local\.artifactory\.eng\.vmware\.com\/concord-core:.*" ./Dockerfile}
CONCORD_IMAGE_NAME:=concord-core
CONCORD_IMAGE_VERSION:=latest
CONCORD_BUILD_DIR:=build
CONCORD_CONTAINER_SHELL:=/bin/bash
CONCORD_WORK_DIR:=/concord
CONCORD_BUILD_CONTAINER:=concord-build
CONCORD_CMAKE_FLAGS:= \
			  -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
			  -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7 \
			  -DCMAKE_CXX_FLAGS=\"-march=x86-64 -mtune=generic\" \
			  -DUSE_CONAN=OFF
CONCORD_CTEST_TIMEOUT:=3000

.PHONY: help
help: ## The Makefile helps to build Concord in a docker container
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

	# Basic HOW-TO:
	# make run-c              # Run container in background
	# make build-s            # Build Concord sources
	# make test               # Run tests
	# make remove-c           # Remove existing container

.PHONY: run-c
run-c: ## Run container in background
	docker run -d --rm \
			  --name=${CONCORD_BUILD_CONTAINER} \
			  --mount type=bind,source=${CURDIR},target=${CONCORD_WORK_DIR} \
			  --mount type=bind,source=${CURDIR}/../communication,target=/communication \
			  ${PREREQS_IMAGE} \
			  ${CONCORD_CONTAINER_SHELL} -c \
			  "make -C /googletest/_build install; /usr/bin/tail -f /dev/null"
	@echo
	@echo "The container \"${CONCORD_BUILD_CONTAINER}\" with the build environment is started as daemon."
	@echo "Run \"make stop-c\" to stop or \"make remove-c\" to delete."

login: ## Login to the container
	docker exec -it --workdir=${CONCORD_WORK_DIR} \
		${CONCORD_BUILD_CONTAINER} ${CONCORD_CONTAINER_SHELL};exit 0

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
	docker exec -t --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"mkdir -p ${CONCORD_BUILD_DIR} && \
		cd ${CONCORD_BUILD_DIR} && \
		cmake ${CONCORD_CMAKE_FLAGS} .. && \
		make -j $$(nproc) all"
	@echo
	@echo "Build finished. The binaries are in ${CURDIR}/${CONCORD_BUILD_DIR}"

.PHONY: test
test: ## Run all tests
	docker exec -t --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: single-test
single-test: ## Run single test `make single-test TEST_NAME=<test name>`
	docker exec -t --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"cd ${CONCORD_BUILD_DIR} && \
		ctest -R ${TEST_NAME} --timeout ${CONCORD_CTEST_TIMEOUT} --output-on-failure"

.PHONY: clean
clean: ## Clean Concord build directory
	docker exec -t --workdir=${CONCORD_WORK_DIR} ${CONCORD_BUILD_CONTAINER} \
		${CONCORD_CONTAINER_SHELL} -c \
		"rm -rf ${CONCORD_BUILD_DIR}"

.PHONY: build-docker-image
build-docker-image: ## Build Concord image used by Hermes tests
	docker build -f Dockerfile .. -t ${CONCORD_IMAGE_NAME}:${CONCORD_IMAGE_VERSION}
