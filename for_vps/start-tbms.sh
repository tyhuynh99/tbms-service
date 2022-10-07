#!/bin/bash
echo "Begin starting TBMS Service"

DB_SERVER="127.0.0.1"
DB_PORT="5432"
DB_USERNAME="admin"
DB_PASSWORD="123"
ADMIN_KEY_FILE_PATH="/home/admin0099/tbms-a082d-firebase-adminsdk-43tdy-6fc21f54ab.json"
BUCKET_NAME="tbms-a082d.appspot.com"
PREVIEW_URL="https://firebasestorage.googleapis.com/v0/b/tbms-a082d.appspot.com/o/%s?alt=media"

nohup java -jar ./source/github/workspace/target/tbms-build.jar -Dspring.profiles.active=dev -DDB_SERVER=$DB_SERVER -DDB_PORT=$DB_PORT -DDB_USERNAME=$DB_USERNAME -DDB_PASSWORD=$DB_PASSWORD -DADMIN_KEY_FILE_PATH=$ADMIN_KEY_FILE_PATH -DBUCKET_NCKET_NAME=$BUCKET_NAME -DPREVIEW_URL=$PREVIEW_URL > /dev/null 2>&1&
echo "End starting TBMS Service"