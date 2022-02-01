#!/bin/sh
make prod
git checkout gh-pages
cp -r dist/* .
git clean -f -x -d .
git commit -am"deploy to gh-pages"
