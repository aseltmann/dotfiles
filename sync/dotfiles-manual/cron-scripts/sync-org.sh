#!/bin/bash
# author: A Seltmann

#####################
##  Configuration  ##
#####################

# Name of your remote storage as defined in Rclone
DRIVE_NAME="dropbox"

# Compose full path to local and remote database files
LOCAL_PATH="$HOME/org"
REMOTE_PATH="rclone/org"
FILTERS_FILE="$HOME/sync/rclone/bisync-filters.txt"
LOG_FILE="$HOME/sync/rclone/bisync.log"
#####################

sync_org ()
{
    printf "%s - Starting Org sync...\n" "$(date -u)" \
        >> "$LOG_FILE"
    rclone bisync "$LOCAL_PATH" "$DRIVE_NAME":"$REMOTE_PATH" \
        --check-access \
        --filters-file "$FILTERS_FILE" \
        --verbose \
        >> "$LOG_FILE" 2>&1
}

sync_org
