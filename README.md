# getOSM

[![](https://www.r-pkg.org/badges/version/getOSM)](https://www.r-pkg.org/pkg/getOSM)
[![Build Status](https://travis-ci.org/YsoSirius/getOSM.svg?branch=master)](https://travis-ci.org/YsoSirius/getOSM)
[![codecov](https://codecov.io/gh/YsoSirius/getOSM/branch/master/graph/badge.svg)](https://codecov.io/gh/YsoSirius/getOSM)


## Installation
 ```sh
# install.packages("devtools")
devtools::install_github("YsoSirius/getOSM")
```


## Description
The package offers an interactive interface to 
the download service of Geofabrik GmbH. If osmconvert is 
set as system variable, the function ```convertOSM``` can also be
used. Some basic information about the data size per country
and continent can be plotted as treemap with ```treemapOSM``` or
as mapview plot with ```mapviewOSM``` 


## Usage 
### Download OSM Data
 ```sh
dest <- getOSM()
dest <- getOSM(exclude = "md5", r1 = 2, r2=13)
dest <- getOSM(filterby="shp", r1 = 2, r2=13)
dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
```

### Convert OSM File
 ```sh
dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
convertOSM(dest, cm=T, cb = F, cw = F, fname = "capverde4", ext = "pbf")
```

### Get a summary of the OSM - Data size
 ```sh
sumry <- summaryOSM()
```

### Plot the summary with treemapOSM
 ```sh
treemapOSM(sumry)
```

### Plot the summary with mapviewOSM
 ```sh
mapviewOSM(con_df, mergeby = "country", unit = "gb")
```

