#!/bin/bash
# this script was inspired by Zvi Baratz Blog post here:
# https://medium.com/@z.baratz/setting-up-keepassxc-on-linux-with-cloud-synchronization-85ccce837365
# and contains the check if a local database exists from thuvh:
# https://gist.github.com/ZviBaratz/a4a51544c3d876543d37abfd0c6ee2a3?permalink_comment_id=3724038#gistcomment-3724038
# as well as modifications and shellcheck refactoring by A Seltmann

#####################
##  Configuration  ##
#####################

# Name of your remote storage as defined in Rclone
DRIVE_NAME="mega"

# Name and locations of the folders for syncing
LOCAL_PATH="$HOME/Programs/zotero"
REMOTE_PATH="rclone/zotero"

#####################

# import and export commands
db_export ()
{
    rclone copy "$LOCAL_PATH" "$DRIVE_NAME:$REMOTE_PATH"
    rclone copyto \
        --ignore-times \
        "$LOCAL_PATH/RCLONE_TIMESTAMP" \
        "$DRIVE_NAME:$REMOTE_PATH/RCLONE_TIMESTAMP"
    rclone touch "$LOCAL_PATH/RCLONE_TIMESTAMP"
}

db_import ()
{
    rclone copy "$DRIVE_NAME:$REMOTE_PATH" "$LOCAL_PATH"
    rclone touch "$LOCAL_PATH/RCLONE_TIMESTAMP"
}

format_datetime_from_string ()
{
    date -d "$1" +"%F %T.%3N"
}

# Parse local passwords file modification time using the stat command
get_local_db_mtime ()
{
    if ! stat_of_db=$(stat -c %y "$LOCAL_PATH/zotero.sqlite"); then
       echo ""
       unset stat_of_db
    else
       _get_local_db_mtime_str="$(echo "$stat_of_db" | cut -d ' ' -f 1,2;)"
       format_datetime_from_string "$_get_local_db_mtime_str"
       unset stat_of_db
    fi
}

# Parse remote folder db modification time using Rclone's lsl command
# See: https://rclone.org/commands/rclone_lsl/
# we use the dummy file `RCLONE_TIMESTAMP` to get the information when the folder
# was last updated
get_remote_db_mtime ()
{
    output=$(rclone lsl "$DRIVE_NAME:$REMOTE_PATH/RCLONE_TIMESTAMP" 2>/dev/null)
    if [ $? -eq 3 ]; then
        unset output
        return 1
    else
        _get_remote_db_mtime_str="$(echo "$output" | tr -s ' ' | cut -d ' ' -f 3,4;)"
        format_datetime_from_string "$_get_remote_db_mtime_str"
        unset output
        return 0
    fi

}

sync_zotero ()
{
    # if there is no local yet
    human_readable_local_mtime="$(get_local_db_mtime)"
    if [ -z "$human_readable_local_mtime" ]; then
        printf "No local zotero database found!\n"
        printf "Importing...\t"
        db_import
        printf "Done!\n"
        return 0
    fi


    # In case there is no remote yet
    if ! human_readable_remote_mtime=$(get_remote_db_mtime 2>/dev/null); then
        printf "No remote zotero database found!\n"
        printf "Exporting...\t"
        db_export
        printf "Done!\n"
        return 0
    fi

    # Printing modification times to the user
    printf "Local zotero folder modification time:\t%s\n" "$human_readable_local_mtime"
    printf "Remote zotero folder modification time:\t%s\n" "$human_readable_remote_mtime"

    # The conversion is required for the comparison in the following if statement
    local_mtime_in_seconds_since_epoch=$(date -d "$human_readable_local_mtime" +%s)
    remote_mtime_in_seconds_since_epoch=$(date -d "$human_readable_remote_mtime" +%s)
    unset human_readable_local_mtime
    unset human_readable_remote_mtime

    # Handle local being newer than remote
    if [ "$local_mtime_in_seconds_since_epoch" -gt "$remote_mtime_in_seconds_since_epoch" ]; then
        printf "Local zotero database found to be newer than remote!\n"
        printf "Exporting...\t"
        db_export
        printf "Done!\n"
        return 0

    # Handle remote being newer than local
    elif [ "$local_mtime_in_seconds_since_epoch" -lt "$remote_mtime_in_seconds_since_epoch" ]; then
        printf "Local zotero database found to be older than remote!\n"
        printf "Importing...\t"
        db_import
        printf "Done!\n"
        return 0

    else
        printf "Zotero folders are synchronized.\n"
        return 0
    fi
}

sync_zotero
