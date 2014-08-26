#!/bin/bash

memcached &

cabal configure --enable-tests
cabal build
cabal test
