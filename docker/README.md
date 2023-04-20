# OCaml-CI Dockerfiles

They must be built using the root of the project as context: `docker build -f docker/gitlab/Dockerfile .`.
See [docker-compose.yml](../docker-compose.yml) for 

## GitLab Pipeline

The OCurrent pipeline for building projects hosted on public GitLab.

## GitHub Pipeline

The OCurrent pipeline for building projects hosted on public GitHub.

## WWW

Public website for ocaml-ci using Dream and CapnProto to connect to the pipelines.

## worker

ocluster worker to run in a Linux x86_64 pool to test local builds.
The worker uses Docker in Docker to run builds as the production cluster would on Linux.
