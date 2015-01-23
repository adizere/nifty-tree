#!/bin/sh
#
# nifty-tree     Cleanup procedure
#
# description: Cleans an EC2 instance. Needed prior to creating an AMI.


rm -f /logs/*
rm -f /opt/streamer/*
truncate /tmp/nifty-tree-status --size 0
truncate /tmp/robostreamer-status --size 0