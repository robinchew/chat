#!/usr/bin/bash -x
# https://stackoverflow.com/questions/16365130/what-is-the-difference-between-usr-bin-env-bash-and-usr-bin-bash

set -e

ctr=$(buildah from ubuntu:24.04);
buildah run "$ctr" -- bash -c "echo 'Australia/Perth' > /etc/timezone"
buildah run "$ctr" -- apt update

# Dev essentials
buildah run "$ctr" -- apt install --assume-yes iproute2 make git build-essential curl erlang-dev vim

buildah run "$ctr" -- apt install --assume-yes erlang-nox
buildah commit --rm "$ctr" "erlang-image"

ctr2=$(buildah from erlang-image);
buildah run -v $(readlink -f ./):/var/www/erlchat --workingdir /var/www/erlchat --env SERVER_PORT=8080 $ctr2 -- make run
