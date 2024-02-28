# code to run stepcount and save to results/stepcount folder
# both ssl and random forest version

conda activate stepcount

for i in data/stepcount/clemson/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 --model-type='ssl' -o results/stepcount_ssl/clemson
  stepcount ${i} --sample-rate=30 --model-type='rf' -o results/stepcount_rf/clemson

done

for i in data/stepcount/clemson/*15Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=15 --model-type='ssl' -o results/stepcount_ssl/clemson
  stepcount ${i} --sample-rate=15 --model-type='rf' -o results/stepcount_rf/clemson

done

for i in data/stepcount/marea/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 --model-type='ssl' -o results/stepcount_ssl/marea
  stepcount ${i} --sample-rate=30 --model-type='rf' -o results/stepcount_rf/marea


done

for i in data/stepcount/marea/*128Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=128 --model-type='ssl' -o results/stepcount_ssl/marea
  stepcount ${i} --sample-rate=128 --model-type='rf' -o results/stepcount_rf/marea

done

for i in data/stepcount/oxwalk/*30Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=30 --model-type='ssl' -o results/stepcount_ssl/oxwalk
  stepcount ${i} --sample-rate=30 --model-type='rf' -o results/stepcount_rf/oxwalk
done

for i in data/stepcount/oxwalk/*100Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=100 --model-type='ssl' -o results/stepcount_ssl/oxwalk
  stepcount ${i} --sample-rate=100 --model-type='rf' -o results/stepcount_rf/oxwalk
done

for i in data/stepcount/oxwalk/*25Hz.csv.gz;
do
  echo "${i}";
  stepcount ${i} --sample-rate=25 --model-type='ssl' -o results/stepcount_ssl/oxwalk
  stepcount ${i} --sample-rate=25 --model-type='rf' -o results/stepcount_rf/oxwalk
done


