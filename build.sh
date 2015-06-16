#!/usr/bin/bash

cd `dirname $0`

git pull
cabal install
