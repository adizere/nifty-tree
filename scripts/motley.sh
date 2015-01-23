

# extract all the archives found in the current dir:
find . -type f | grep -e "node.*.gz" | xargs -I {} tar -zxvf {}


# extract the delivery time for the last frame from all ./*nifty files:
for f in ./*.nifty ; do egrep -e ' d \d+' $f | tail -n 1 ; done