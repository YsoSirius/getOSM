# getOSM

[![](https://www.r-pkg.org/badges/version/getOSM)](https://www.r-pkg.org/pkg/getOSM)
[![Build Status](https://travis-ci.org/YsoSirius/getOSM.svg?branch=master)](https://travis-ci.org/YsoSirius/getOSM)
[![codecov](https://codecov.io/gh/YsoSirius/getOSM/branch/master/graph/badge.svg)](https://codecov.io/gh/YsoSirius/getOSM)


# Installation
 ```sh
# install.packages("devtools")
devtools::install_github("YsoSirius/getOSM")
```


# Description
The package offers an interactive interface to 
the download service of Geofabrik GmbH. If osmconvert is 
set as system variable, the function convertOSM can also be
used.


    
## Example Calls
 ```sh
dest <- getOSM()
dest <- getOSM(exclude = "md5", r1 = 2, r2=13)
dest <- getOSM(filterby="shp", r1 = 2, r2=13)
dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
```
