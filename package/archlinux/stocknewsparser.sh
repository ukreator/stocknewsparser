#!/bin/sh
if [ "$(id -u)" != "0" ]; then
    echo "This script must be run as root" 1>&2
    exit 1
fi
su stocknewsparser -c "/opt/stocknewsparser/bin/stocknewsparser $@"
