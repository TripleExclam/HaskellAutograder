#!/usr/bin/env bash

apt-get install -y python3 python3-pip python3-dev

pip3 install -r /autograder/source/requirements.txt

curl -sSL https://get.haskellstack.org/ | sh

add-apt-repository -y "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc) main universe"
apt-get install -y libncurses5-dev

cd /autograder/source/

echo $PWD

stack setup
stack build
stack test

echo "===>>All set up"