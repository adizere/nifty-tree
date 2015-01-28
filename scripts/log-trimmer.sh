
for fl in ./*.nifty ; do
    egrep -e " d " ${fl} > "${fl}.trimmed"
done

rm -f ./*.nifty