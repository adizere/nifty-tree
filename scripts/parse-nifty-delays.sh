# This scripts parses all *nifty log files and extracts, for each delivered
# frame, the difference between the time when that frame was first received by
# some node and the time when that frame was last received by some node.


# Maximum frame number across all the logs
n=120


> "delay.results"
> "delay.interim"
> "delay.average"

# for each frame number..
for (( fn = 1; fn <= $n; fn++ )); do
    # extract the delivery time for ALL frames having seq nr $fn
    # across ALL ./*nifty files
    > "__tmp.${fn}"
    for fl in ./*.nifty.trimmed ; do
        c=`egrep -e " d ${fn}$" ${fl} | egrep -o -e '\d+\.\d+' | tr '\n' ' '`;
        echo -e "$c" >> "__tmp.${fn}"
    done
    # now in file __tmp.${fn} we have X rows, each row having Z columns, and
    # each cell representing delivery time for frame nr Y, s.t.:
    # X = total number of nodes,
    # Z = total number of streaming session that contained frame number ${fn}
    # Y = ${fn}

    # obtain Z
    a=""
    z=`head -n1 "__tmp.${fn}" | egrep -oe '\d+.\d+' | wc -l`
    for (( i = 1; i <= $z; i++ )); do
        # isolate each column and take the smallest and the greatest values,
        # then subtract them
        v=`cat "__tmp.${fn}" | cut -d' ' -f "${i}" | sort | sed \
             -n '1p;$p' | paste -s -d '-' - | bc -l`
        # now do the subtraction
        echo $v >> "delay.interim"
        a="$a $v"
    done

    # obtain the average
    a=`echo "$a" | tr -d "-" | awk '{s+=$1}END{print s/3}'  RS=" "`
    echo $a >> "delay.average"

    rm -f "__tmp.${fn}"
    tr -d "-" < "delay.interim" > "delay.results"

    echo "Extracted delivery delays for $fn.."
done