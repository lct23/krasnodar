#!/bin/bash

for service in server; do
    docker build --target "${service}" -t "lct-krasnodar-${service}" .
done
