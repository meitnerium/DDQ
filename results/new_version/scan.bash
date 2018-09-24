

pourcw=80
dpw=1
w="0.004210207312979400"
cas=0
echo "test"
echo "$pourcw<=120" | bc -l
while [ $(echo "$pourcw<=120" | bc -l) == 1 ] ; do
  if [ $cas -lt 10 ] ; then
    mkdir "case_0000$cas" 
    input="case_0000$cas/input" 
  elif [ $cas -lt 100 ] ; then
    mkdir "case_000$cas" 
    input="case_000$cas/input" 
  elif [ $cas -lt 1000 ] ; then
    mkdir "case_00$cas" 
    input="case_00$cas/input" 
  else
    mkdir "case_0$cas" 
    input="case_0$cas/input" 
  fi
  echo " &iofile
 t0=-11458.0d0,
 pulsetype=2,
 E0=7.d14,
 phase=0.d0,
 dt=1.d0,
 nc=3
 w=$(echo "0.004210207312979400*$pourcw/100" | bc -l)d0
 &end" >> $input
  let cas=$cas+1
  if [ $cas -lt 10 ] ; then
    mkdir "case_0000$cas" 
    input="case_0000$cas/input"
  elif [ $cas -lt 100 ] ; then
    mkdir "case_000$cas" 
    input="case_000$cas/input"
  elif [ $cas -lt 1000 ] ; then
    mkdir "case_00$cas" 
    input="case_00$cas/input"
  else
    mkdir "case_0$cas" 
    input="case_0$cas/input"
  fi
  echo " &iofile
 t0=-11458.0d0,
 pulsetype=2,
 E0=7.d14,
 phase=0.5d0,
 dt=1.d0,
 nc=3
 w=$(echo "0.004210207312979400*$pourcw/100" | bc -l)d0
 &end" >> $input
 let pourcw=$pourcw+$dpw
 let cas=$cas+1
done


