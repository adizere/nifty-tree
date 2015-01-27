

# extract all the archives found in the current dir:
find . -type f | grep -e "node.*.gz" | xargs -I {} tar -zxvf {}


# extract the delivery time for the last frame from all ./*nifty files:
for f in ./*.nifty ; do egrep -e ' d \d+' $f | tail -n 1 ; done

# extract the delivery time for frame 1 in the last streaming session
# from all ./*nifty files:
for f in ./*.nifty ; do egrep -e ' d 1$' $f | tail -n 1 ; done