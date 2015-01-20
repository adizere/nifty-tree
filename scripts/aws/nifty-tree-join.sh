#!/bin/sh
#
# nifty-tree     Initialization procedure
#
# description: nifty-tree is the companion of robostreamer, it handles data
# streaming, while robostreamer handles exclusively metadata streaming
#
# This script doesn't have to be installed with chkconfig.
# Robostreamer (/etc/init.d/robostreamer) automatically calls this script with
# one of the following arguments: 'start', 'stop', or 'reload'.
#


function wait4_session_files()
{
    while [[ ! -a "/opt/streamer/session.json" ]]; do
        echo `date` "waiting for session file" >> "${output}"
        sleep 1
    done

    while [[ ! -a "/opt/streamer/digests.list" ]]; do
        echo `date` "waiting for digests file" >> "${output}"
        sleep 1
    done
}


function stop_service()
{
    echo `date` "Trying to stop the nifty-tree service.." >> "${output}"
    pkill -9 -f "${exec}" || echo `date` \
        "nifty-tree wasn't running" >> "${output}"
}


function reload_service_code()
{
    echo `date` "Reloading nifty-tree directly from git.." >> "${output}"
    set -e

    cd /home/ec2-user/git/nifty-tree

    git pull
    cabal install --only-dependencies
    cabal build

    echo `date` "Finished reloading" >> "${output}"
}


function start_service()
{
    echo `date` "Starting up.." >> "${output}"
    eval "${exec}" &
}


#
# Execution starts here
#

local_address=`GET http://169.254.169.254/latest/meta-data/local-ipv4`
log_file=`printf "/logs/node.%s.log.nifty" $local_address`


dir=$TP
exec="${dir}/dist/build/nifty-tree/nifty-tree -a >${log_file} 2>&1"
output="/tmp/nifty-tree-status"

echo `date` "nifty-tree init script: $1" >> "${output}"

touch "${log_file}"

cd $dir


if [ "$1" = "start" ]; then

    stop_service;
    echo `date` "About to start.." >> "${output}"
    wait4_session_files;
    reload_service_code;
    start_service;

elif [ "$1" = "stop" ]; then
    stop_service;

elif [ "$1" = "reload" ]; then
    reload_service_code;
fi
