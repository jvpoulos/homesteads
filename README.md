# homesteads

This repository provides data and code for reproducing ["State-Building through Public Land Disposal? An Application of Matrix Completion for Counterfactual Prediction"](https://arxiv.org/abs/1903.08028).

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

Prerequsites
------

* **R** (tested on 4.0.1)
* Required packages located in *package-list.R*
  * The packages libgdal-dev and libproj-dev are required prior to installing the rgdal **R** package:
```
$ sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev
```
  * The package mdbtools is required prior for the mdb.get function of the HMisc **R** package:
```
$ sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev
```

Install the forked MCPanel repo:
```R
install.packages("devtools")
library(devtools) 
install_github("jvpoulos/MCPanel")
```
**Note:** fitting the matrix completion model with covariates (*mcnnm_wc_cv*) is computationally expensive and will likely make a laptop crash. The code below is run on a single node with 30G RAM and 6 CPU-cores on a high-performance compute cluster.  

Instructions
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/homesteads
```

* Download and extract patents data (2.8 GB uncompressed) to `data/` directory:
  * [patents.tar.xz](https://www.dropbox.com/s/3g5jlqpp6kvreur/patents.tar.xz?dl=1)

* Make shell file `main.sh` executable from the Linux/Unix command line:
```
$ chmod +x main.sh
```
* Execute the file:
```
$ ./main.sh > main.txt
```
* Repeat the last two steps for `mc-synth.sh` and `mc-capacity.sh` to run experiments
* For MC placebo estimates: `mc-capacity-placebo.sh`
* For MC causal estimates: `mc-capacity-estimates.sh`