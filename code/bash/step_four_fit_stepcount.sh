conda activate stepcount

for i in data/stepcount/clemson/*.csv.gz;
do
  echo "${i}";
  stepcount ${i} -o results/stepcount/clemson
done

for i in data/stepcount/marea/*.csv.gz;
do
  echo "${i}";
  stepcount ${i} -o results/stepcount/marea
done

for i in data/stepcount/oxwalk/*.csv.gz;
do
  echo "${i}";
  stepcount ${i} -o results/stepcount/oxwalk
done
