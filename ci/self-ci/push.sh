#!/bin/bash
set -eux
make docker DOCKER_HOST=unix:///var/run/docker.sock
docker -H unix:///var/run/docker.sock push editions/datakit-self-ci
IMAGE=$(docker -H unix:///var/run/docker.sock image inspect editions/datakit-self-ci -f '{{index .RepoDigests 0}}')
docker pull $IMAGE
docker service update self-ci_ci --image $IMAGE
