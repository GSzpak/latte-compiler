for f in lattests/good/*.lat;
do  
    name="${f%%.*}"
    ./latc $f
    lli $name.bc > $name.out
    diff -s $name.out $name.output
done;
rm lattests/good/*.ll
rm lattests/good/*.out
rm lattests/good/*.bc    
for f in lattests/bad/*.lat;
do  
    name="${f%%.*}"
    ./latc $f
done;
