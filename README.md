ibelief
=======


The R package **ibelief** aims to provide some basic functions to implement belief function theory.

## Installation

You can install the stable version on
[CRAN](http://cran.rstudio.com/package=ibelief):

```r
install.packages("ibelief", dependencies = TRUE)
```

Also you can download the newest source folder of the package from [GitHub](https://github.com/kuangzhou/ibelief), and build the package yourselves:

```bash
git clone https://github.com/kuangzhou/ibelief.git
R CMD build ibelief
```

Then you can install the package using R CMD INSTALL command

```bash
R CMD INSTALL ibelief_*.tar.gz
```

or install the package from source in R:

```r
install.packages("~/ibelief_*.tar.gz", repos = NULL, type = "source")
```

## Change information 

### For the stable version on CRAN

see the [NEWS-CRAN](http://cran.r-project.org/web/packages/ibelief/NEWS) file

### For the latest beta version on GitHub 
see the [NEWS-BETA](https://raw.githubusercontent.com/kuangzhou/ibelief/master/NEWS) file
