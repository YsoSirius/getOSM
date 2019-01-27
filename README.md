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
used. The same goes for osmosis, which enables the function 
```osmosisR```.

Some basic information about the data size per country
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
mapviewOSM(sumry, mergeby = "country", unit = "gb")
```

### Create an OSM-file filtered for pedestrian routing
The functions ```graphcycleOSM``` and ```graphcarOSM``` are used for bicycle and car routing.
 ```sh
## Download Comores Data
dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13)
## Filters only highway elements for pedestrian routing and converts to osm 
graphpedesOSM(dest)
```

### Import to DB with osmosis
 ```sh
 ## The Database must already exist beforahand. 
dblist <- list(dbname="test", dbuser="postgres", dbhost="localhost",
               dbport="5432", dbpwd="postgres")
cmd <- postgresOSM(dest, dblist)
```

### Inspect Database 
 ```sh
ways <- sfOSM(dblist)
r <- filterSF(ways, tf=c("footway", "path"), plot=T);
 ```
