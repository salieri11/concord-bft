# Build time image.
FROM python:3.7 as builder

RUN pip install grpcio-tools

## Copy build-dependent resource files.
COPY ./api/setup.py ./persephone/api/setup.py
COPY ./api/src ./persephone/api/src

WORKDIR /persephone/api
RUN python setup.py grpc egg_info --egg-base=target sdist --dist-dir=target/dist bdist_wheel --dist-dir=target/dist --universal clean --all

FROM python:3.7

COPY --from=builder ./persephone/api/target/dist/persephone_api-*-py2.py3-none-any.whl ./persephone/persephone_api-0.0.1-py2.py3-none-any.whl
COPY ./resources/scripts ./persephone/scripts

RUN pip install grpcio /persephone/persephone_api-0.0.1-py2.py3-none-any.whl \
  && apt-get update && apt-get -y install vim

VOLUME ["/config"]

WORKDIR /persephone/scripts
