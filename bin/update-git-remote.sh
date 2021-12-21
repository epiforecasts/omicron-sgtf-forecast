#!/bin/bash

git add data writeup scripts
date=$(date +'%Y-%m-%d')
git commit -m "$date - update estimates"
git pull -Xours
git push