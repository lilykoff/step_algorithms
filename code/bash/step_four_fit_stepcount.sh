conda activate stepcount

for i in data/stepcount/clemson/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 -o results/stepcount/clemson
done

for i in data/stepcount/clemson/*15Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=15 -o results/stepcount/clemson
done

for i in data/stepcount/marea/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 -o results/stepcount/marea
done

for i in data/stepcount/marea/*128Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=128 -o results/stepcount/marea
done

for i in data/stepcount/oxwalk/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 -o results/stepcount/oxwalk
done

for i in data/stepcount/oxwalk/*100Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=100 -o results/stepcount/oxwalk
done

for i in data/stepcount/oxwalk/*25Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=25 -o results/stepcount/oxwalk
done
