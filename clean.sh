#!/bin/bash

# Remove everything but .hs source files
rm -rf *.hi
rm -rf *.o
find . -type f -not -iname "*.*" -delete