#!/bin/bash

target_dir=$1

find $target_dir -mindepth 1 -mmin +720 -exec rm -rf {} \;


echo "==============";
echo $target_dir;
echo "======END=====";

TARGET_DIR="/tmp"
LOG_FILE="/home/glassfish/cleanup_tmp.log"

# Find and delete up to 5000 files older than 2 days that literally end with "tmp"
DELETED_FILES=$(find "$TARGET_DIR" -maxdepth 1 -type f -mtime +2 -name "*tmp" 2>/dev/null | head -n 5000)

NUM_DELETED=$(echo "$DELETED_FILES" | grep -c '^')

if [ "$NUM_DELETED" -gt 0 ]; then
  echo "$(date): Deleted $NUM_DELETED *tmp file(s)." >> "$LOG_FILE"
  echo "$DELETED_FILES" | xargs rm -f
else
  echo "$(date): No *tmp files to delete." >> "$LOG_FILE"
fi