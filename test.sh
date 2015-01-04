for f in lattests/good/*.lat;
do
    ./latc $f
    lli $f.bc > $f.out
    diff -s $.out $f.output
done;
    
