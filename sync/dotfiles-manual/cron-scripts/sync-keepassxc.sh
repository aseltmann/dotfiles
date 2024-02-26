#!/bin/bash
# this script was taken from Zvi Baratz Blog post here:
# https://medium.com/@z.baratz/setting-up-keepassxc-on-linux-with-cloud-synchronization-85ccce837365
# and contains the check if a local database exists from thuvh:
# https://gist.github.com/ZviBaratz/a4a51544c3d876543d37abfd0c6ee2a3?permalink_comment_id=3724038#gistcomment-3724038
# as well as shellcheck refactoring by A Seltmann

# Important note:
# If you don't synchronize but then edit the other file,
# the newer modification time on the second file edited
# will cause data to be overridden.
# The solution would be to merge manually
# (Database --> Merge from database).


#####################
##  Configuration  ##
#####################

# Name of your remote storage as defined in Rclone
DRIVE_NAME="dropbox"

# Name and locations of the passwords file
DB_FILE_NAME="Passwords.kdbx"
LOCAL_LOCATION="$HOME/sync/rclone"
REMOTE_LOCATION="rclone"

#####################


# Compose full path to local and remote database files
LOCAL_PATH="$LOCAL_LOCATION/$DB_FILE_NAME"
REMOTE_PATH="$REMOTE_LOCATION/$DB_FILE_NAME"

# Alias import and export commands and make them available within the functions
alias passwords_export="rclone copy $LOCAL_PATH $DRIVE_NAME:$REMOTE_LOCATION"
alias passwords_import="rclone copy $DRIVE_NAME:$REMOTE_PATH $LOCAL_LOCATION"
shopt -s expand_aliases

format_datetime_from_string ()
{
	date -d "$1" +"%F %T.%3N"
}

# Parse local passwords file modification time using the stat command
get_local_passwords_mtime ()
{
    if ! stat_of_db=$(stat -c %y "$LOCAL_PATH"); then
       echo ""
       unset stat_of_db
    else
       _get_local_passwords_mtime_str="$(echo "$stat_of_db" | cut -d ' ' -f 1,2;)"
	   format_datetime_from_string "$_get_local_passwords_mtime_str"
       unset stat_of_db
    fi
}

# Parse remote passwords file modification time using Rclone's lsl command
# See: https://rclone.org/commands/rclone_lsl/
get_remote_passwords_mtime ()
{
    output=$(rclone lsl $DRIVE_NAME:$REMOTE_PATH 2>/dev/null)
    if [ $? -eq 3 ]; then
        unset output
        return 1
    else
        _get_remote_passwords_mtime_str="$(echo "$output" | tr -s ' ' | cut -d ' ' -f 3,4;)"
        format_datetime_from_string "$_get_remote_passwords_mtime_str"
        unset output
        return 0
    fi

}

sync_passwords ()
{
    # first, display the time
    printf "%s - Starting KeepassXC sync...\n" "$(date -u)"
    # if there is no local yet
    human_readable_local_mtime="$(get_local_passwords_mtime)"
    if [ -z "$human_readable_local_mtime" ]; then
        printf "No local passwords database found!\n"
        printf "Importing...\t"
        passwords_import
        printf "Done!\n"
        return 0
    fi


    # In case there is no remote yet
    if ! human_readable_remote_mtime=$(get_remote_passwords_mtime 2>/dev/null); then
        printf "No remote passwords database found!\n"
        printf "Exporting...\t"
        passwords_export
        printf "Done!\n"
        return 0
    fi

    # Printing modification times to the user
    printf "Local passwords file modification time:\t%s\n" "$human_readable_local_mtime"
    printf "Remote passwords file modification time:\t%s\n" "$human_readable_remote_mtime"

    # The conversion is required for the comparison in the following if statement
    local_mtime_in_seconds_since_epoch=$(date -d "$human_readable_local_mtime" +%s)
    remote_mtime_in_seconds_since_epoch=$(date -d "$human_readable_remote_mtime" +%s)
    unset human_readable_local_mtime
    unset human_readable_remote_mtime

    # Handle local being newer than remote
    if [ "$local_mtime_in_seconds_since_epoch" -gt "$remote_mtime_in_seconds_since_epoch" ]; then
        printf "Local passwords file found to be newer than remote!\n"
        printf "Exporting...\t"
        passwords_export
        printf "Done!\n"
        return 0

    # Handle remote being newer than local
    elif [ "$local_mtime_in_seconds_since_epoch" -lt "$remote_mtime_in_seconds_since_epoch" ]; then
        printf "Local passwords file found to be older than remote!\n"
        printf "Importing...\t"
        passwords_import
        printf "Done!\n"
        return 0

    else
        printf "Password files are synchronized.\n"
        return 0
    fi
}

sync_passwords
