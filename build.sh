#!/bin/bash

for service in passport events rating image-store admin; do
    docker build --target "${service}" -t "lct2023-${service}" .
done
