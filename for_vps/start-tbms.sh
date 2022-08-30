#!/bin/bash
echo "Begin starting TBMS Service"
nohup java -jar ./source/github/workspace/target/tbms-build.jar > /dev/null 2>&1&
echo "End starting TBMS Service"