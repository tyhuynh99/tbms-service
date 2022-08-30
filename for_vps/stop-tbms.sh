#!/bin/bash
echo "Begin stopping TBMS Service"
sudo pkill -f ./source/github/workspace/target/tbms-build.jar
sudo rm -r source
echo "End stopping TBMS Service"