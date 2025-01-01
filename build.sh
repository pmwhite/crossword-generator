#!/bin/env sh

set -eux

warnings="-w A-40-42-70-30-44 -error-style short"
ocamlopt $warnings main.ml -o main.exe
