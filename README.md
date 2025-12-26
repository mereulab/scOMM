![alt text](https://github.com/mereulab/scOMM/blob/master/scOMM_logo.png?raw=true)

# scOMM
### A Multi-Language Multi-Omics Deep Neural Network for automatic single-cell Label Transfer and approach Benchmarking

- **/tutorials** directory includes jupyer notebook showing how to prepare and run scOMM in both R and Python.
- **/utils** directory includes convenient extra functions that facilitates working with different fortmats of single-cell data.
- **/R** directory includes the source code to run scOMM in R.
- **/python** directory includes the source code to run scOMM in Python.

Even though the package is fully functional, it is still in development and it has not been packaged and released in a public repository. 
To implement scOMM in your analyses, run:

## R

```R
remotes::install_github("mereulab/scOMM")
```

In R, the following dependencies are needed:
- seurat / signac
- reshape2
- dplyr
- ggplot2
- tensorflow & keras

## Python

```Python
import sys
sys.path.append("<dirpath>/deepscore/python")
from deepscore import DeepScore
from marker_analysis import *
```

In Python, the following dependencies are needed:
- scanpy / episcanpy
- anndata
- seaborn
- matplotlib
- pandas
- numpy
- tensorflow / keras
