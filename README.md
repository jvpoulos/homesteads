# homesteads

This repository provides data and code for reproducing ["State-Building through Public Land Disposal? An Application of Matrix Completion for Counterfactual Prediction"](https://arxiv.org/abs/1901.02991).

Please cite the paper if you use this code for academic research:

```
@ARTICLE{2019arXiv190308028P,
       author = {{Poulos}, Jason},
        title = "{State-Building through Public Land Disposal? An Application of Matrix Completion for Counterfactual Prediction}",
      journal = {arXiv e-prints},
     keywords = {Economics - General Economics, Economics - Econometrics, Statistics - Applications},
         year = "2019",
        month = "Mar",
          eid = {arXiv:1903.08028},
        pages = {arXiv:1903.08028},
archivePrefix = {arXiv},
       eprint = {1903.08028},
 primaryClass = {econ.GN},
       adsurl = {https://ui.adsabs.harvard.edu/abs/2019arXiv190308028P},
      adsnote = {Provided by the SAO/NASA Astrophysics Data System}
}
```

Instructions
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/homesteads
```
* The code uses **R** version 3.5.2 (2018-12-20). To install this **R** version on Ubuntu, use the command 
```
$ sudo apt-get install r-base-core=3.5.2-1xenial
```
* Download and extract patents data (2.8 GB uncompressed) to `data/` directory:
  * [patents.tar.xz](https://www.dropbox.com/s/3g5jlqpp6kvreur/patents.tar.xz?dl=1)
* Make a directory to store plots: 
```
$ mkdir results/plots
```
* Open `package-list.R` in a script editor
  * Verify that all required packages in `package-list.R`are installed in your **R** library
* In **R**, install MCPanel from my forked repo:
```
install.packages("devtools")
library(devtools) 
install_github("jvpoulos/MCPanel")
```
* Make shell file `main.sh` executable from the Linux/Unix command line:
```
$ chmod +x main.sh
```
* Execute the file:
```
$ ./main.sh > main.txt
```
* Repeat the last two steps for `mc-synth.sh` and `mc-capacity.sh` to run experiments
* For MC placebo estimates: `mc-capacity-placebo.sh` and `mc-capacity-placebo-covars.sh`
* For MC causal estimates: `mc-capacity-estimates.sh` and `mc-capacity-estimates-covars.sh`
* For sensitivity based on imputation method: `mc-capacity-estimates-covars-linear.sh`, `mc-capacity-estimates-covars-random.sh`, and
`mc-capacity-estimates-covars-median.sh`