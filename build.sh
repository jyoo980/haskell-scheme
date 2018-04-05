#!/bin/bash

# Output an executible eval
ghc -o eval --make main.hs
rm -rf *.o
rm -rf *.hi
