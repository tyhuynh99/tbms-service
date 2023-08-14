#!/bin/bash
echo "Begin stopping TBMS Service"
sudo pkill -f ./source/github/workspace/target/tbms-build.jar
sudo pkill -f ~/service/tbms-build.jar
sudo rm ./source/github/workspace/target/tbms-build.jar
echo "End stopping TBMS Service"