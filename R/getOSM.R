#' @title getOSM
#' @name getOSM
#' @description Get OSM-Data from
#' \href{https://www.geofabrik.de/}{Geofabrik GmbH}
#'
#' @export
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom utils download.file
#'
#' @param filterby Filter by extensions
#' @param exclude Exclude by extensions
#' @param r1 First link (Continent)
#' @param r2 Second link (Countries)
#' @param r3 If (\code{trimForce}==FALSE) the
#' @param dest A destination directory
#'
#' @return The download destination.
#'
#' @examples \donttest{
#' dest <- getOSM()
#' dest <- getOSM(exclude = "md5", r1 = 2, r2=13)
#' dest <- getOSM(filterby="shp", r1 = 2, r2=13)
#' dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
#'}
#' @author Sebastian Gatscha
getOSM <- function(filterby=NULL, exclude=NULL, r1=NULL,
                   r2=NULL, r3=NULL, dest=NULL) {
  # filterby="pbf"; exclude = "md5";r1=r2=r3=dest=NULL

  ## Base URL #######
  url <- 'https://download.geofabrik.de/'
  args <- c("osm", "pbf", "shp", "poly", "kml", "md5")

  ## Step 1 ##########
  geofabrik <- read_html(url)
  subregions <- html_nodes(geofabrik, '#subregions')
  levl1 = html_table(subregions, fill=TRUE)[[1]]
  if (is.null(r1)) {
    print(levl1)
    row_select = readline(prompt="Select the row of the Region you want to download.")
    print(paste("You selected", levl1[row_select,1]))
  } else {
    row_select = r1
  }

  row_select <- as.integer(row_select)
  link1 = tolower(levl1[row_select,1])
  if (length(grep(x = link1, pattern = " and "))!=0) {
    link1 = gsub(x = link1, pattern = " and ", replacement = "-")
  } else if (length(grep(x = link1, pattern = " "))!=0) {
    link1 = gsub(x = link1, pattern = " ", replacement = "-")
  }
  print(paste("step1:", link1))


  ## Step 2 ##########
  levl1html <- read_html(paste0(url, link1, ".html"))
  subregions1 <- html_nodes(levl1html, '#subregions')
  levl2 = html_table(subregions1, fill=TRUE)[[2]]
  if (is.null(r2)) {
    print(levl2)
    row_select1 = readline(prompt="Select the row of the Region you want to download.")
    print(paste("You selected", levl2[row_select1,1]))
  } else {
    row_select1 = r2
  }
  row_select1=as.integer(row_select1)
  link2 = tolower(levl2[row_select1,1])
  if (length(grep(x = link2, pattern = " and "))!=0) {
    link2 = gsub(x = link2, pattern = " and ", replacement = "-")
  } else if (length(grep(x = link2, pattern = " "))!=0) {
    link2 = gsub(x = link2, pattern = " ", replacement = "-")
  }
  print(paste("step2:", link2))


  ## Step 3 ##########
  levl2html <- read_html(paste0(url, link1,"/",
                                link2, ".html"))
  subregions2 <- html_nodes(levl2html, '#subregions')
  levl3 = html_table(subregions2, fill=TRUE)[[1]]

  ## Filter and Exclude ##################
  if (!is.null(filterby)) {
    if (!filterby %in% args) {
      # print(paste("filterby has to be one of:", paste(args, collapse = ", ")))
      stop(paste("filterby has to be one of:", paste(args, collapse = ", ")))
    }
    if (!is.null(exclude)){
      if (!exclude %in% args) {
        # print(paste("exclude has to be one of:", paste(args, collapse = ", ")))
        stop(paste("exclude has to be one of:", paste(args, collapse = ", ")))
      }
      print(paste("step2: Filter and exclude by", filterby, exclude))
      levl3 <- levl3[setdiff(grep(pattern = filterby, x = levl3$file),
                             grep(pattern = exclude, x = levl3$file)),]
    } else {
      print(paste("step2: Filter by", filterby))
      levl3 <- levl3[grep(pattern = filterby, x = levl3$file),]
    }
  } else {
    if (!is.null(exclude)){
      if (!exclude %in% args) {
        print(paste("exclude has to be one of:", paste(args, collapse = ", ")))
        exclude=NULL
      }
      print(paste("step2: Exclude by", exclude))
      levl3 <- levl3[-grep(pattern = exclude, x = levl3$file),]
    }
  }

  if (is.null(r3)) {
    print(levl3)
    row_select2 = readline(prompt="Select the File you want to download.")
    print(paste("You selected", levl3[which(row_select2 == rownames(levl3)),1]))
  } else {
    row_select2 = r3
  }


  ## Subregions for Germany, France, Italy, GB, Netherlands,
  ## Poland, Russian Federation (russia), Japan, zB ##################
  ## Step 4 - Download ##############
  row_select2=as.integer(row_select2)
  downurl <- paste0(url, link1,"/",
                    tolower(levl3[which(row_select2 == rownames(levl3)),1]))

  # dest=NULL
  if (is.null(dest)) {
    dest = readline(prompt="Enter the download destination.")
    if(dest=="") {
      dest = getwd()
    }
    dest = gsub("\\\\", "/", dest)
    print(paste("You selected", dest))
  } else if(dest=="") {
    dest = getwd()
  }
  if (!dir.exists(dest)) {stop("This is not a directory.")}

  if (substr(dest,(nchar(dest)+1)-1,nchar(dest)) == "/") {
    dest <- substr(dest, 1, nchar(dest)-1)
  }

  destname = tolower(levl3[which(row_select2 == rownames(levl3)),1])
  destfin = file.path(dest, destname)
  # browser()

  print(paste("Configs: r1 =",row_select, ",r2 =",row_select1, ",r3 =",row_select2))
  print(paste("Download Path: ", destfin))
  download.file(url = downurl, destfile = destfin, method="libcurl", mode="wb")
  return(destfin)
}
# dest <- getOSM()
# dest <- getOSM(exclude = "md5", r1 = 2, r2=13)
# dest <- getOSM(filterby="shp", r1 = 2, r2=13)
# dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")


#' @title convertOSM
#' @name convertOSM
#' @description Interface to osmconvert. (It must be set as system variable)
#'
#' @export
#'
#' @param destfin The input file path.
#' @param ext Possible extensions.
#' @param cm Include --complete-multipolygons
#' @param cb Include --complete-boundaries
#' @param cw Include --complete-ways
#' @param bbox Cut out the result via bbox.
#' @param poly Cut out the result via a .poly-file
#' @param fname Output file name
#'
#' @return The osmconvert call.
#'
#' @examples \donttest{
#' dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
#' convertOSM(dest, cm=T, cb = F, cw = F, fname = "capverde4", ext = "pbf")
#'}
#' @author Sebastian Gatscha
convertOSM <- function(destfin, ext="osm", cm=FALSE, cb=FALSE, cw=FALSE,
                       bbox=NULL, fname=NULL, poly=NULL) {
  # destfin="C:/Users/Bobo/Documents/TraffiCon/getOSM/getOSM/comores-latest.osm.bz2"
  # destfin=dest
  # ext="bz2"

  extarg = c("osm", "o5m", "pbf", "bz2", "osc", "osh", "o5c", "osc.gz")
  if (!ext %in% extarg) {
    stop(paste("ext must be one of", paste(extarg, collapse = ", ")))
  }

  # fname=NULL
  # fname="osmoutput"
  # ext="osc"
  # cm=cb=cw=F
  # bbox=T
  # poly=T
  # destfin="C:/Users/Bobo/Documents/cape-verde-latest.osm.pbf"


  if (is.null(fname)) {
    fname = basename(destfin)
  }
  if (ext=="osm"){
    destform <-  sub(pattern = "(.*)\\..*$", replacement = "\\1",
                     fname)
    if (!is.na(strsplit(destform, ".", fixed=T)[[1]][2]) &&
        strsplit(destform, ".", fixed=T)[[1]][2] == "osm") {
      destform <- sub(pattern = "(.*)\\..*$", replacement = "\\1",
          destform)
    }
    destform <- paste0(destform, ".osm")
  } else {
    destform <- sub(pattern = "(.*)\\..*$", replacement = "\\1",
                    fname)
    destform <- paste0(destform, ".", ext)
  }

  destform <- file.path(dirname(destfin), destform)

  cm <- ifelse(cm==T, " --complete-multipolygons ", "")
  cb <- ifelse(cb==T, " --complete-boundaries ", "")
  cw <- ifelse(cw==T, " --complete-ways ", "")

  if (!is.null(bbox)){
    # bbox="10.5,49,11.5,50"
    if(length(strsplit(bbox, ",")[[1]])!=4){
      stop(paste("bbox must consist of 4 comma separated numeric values, like: 10.5,49,11.5,50"))
    }
    # as.numeric(strsplit(bbox, ",")[[1]])
    bbox = paste0(" -b=", bbox )
  }
  if(!is.null(poly)){
    poly = paste0(" -B=", poly )
  }
  call <- paste0("osmconvert ",destfin, " ", cm,cb,cw,bbox,poly, " -o=", destform);
  print(call)
  system(call)

  invisible(call)
}

# dest<-getOSM(filterby="pbf", exclude = "md5")
# dest <- getOSM(filterby="pbf", exclude = "md5", r1 = 2, r2 =13, r3 = 45, dest="")
# convertOSM(destfin, cm=T, cb = F, cw = F, fname = "seychelles", ext = "osm")

# convertOSM(dest, cm=T, cb = F, cw = F, fname = "capverde4", ext = "pbf")




#' @title summaryOSM
#' @name summaryOSM
#' @description Get a summary dataset of OSM
#'
#' @export
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom utils head
#'
#' @return A summary dataset, which can be used for treemapOSM or mapviewOSM.
#'
#' @examples \donttest{
#' con_df <- summaryOSM()
#'}
#' @author Sebastian Gatscha
summaryOSM <- function() {
  url <- 'https://download.geofabrik.de/'
  ## Step 1 ##########
  geofabrik <- read_html(url)
  subregions <- html_nodes(geofabrik, '#subregions')
  levl1 = html_table(subregions, fill=TRUE)[[1]]
  row_select = 2:9

  row_select <- as.integer(row_select)
  link1 = tolower(levl1[row_select,1])
  if (length(grep(x = link1, pattern = " and "))!=0) {
    link1 = gsub(x = link1, pattern = " and ", replacement = "-")
  } else if (length(grep(x = link1, pattern = " "))!=0) {
    link1 = gsub(x = link1, pattern = " ", replacement = "-")
  }
  print(paste("step1:", link1))


  ## Step 2 ##########
  continents <- lapply(paste0(url, link1, ".html"), function(i) {
    if (length(grep(x = i, pattern = " and "))!=0) {
      i = gsub(x = i, pattern = " and ", replacement = "-")
    } else if (length(grep(x = i, pattern = " "))!=0) {
      i = gsub(x = i, pattern = " ", replacement = "-")
    }
    tmp <- read_html(i)
    subregions1 <- html_nodes(tmp, '#subregions')
    levl2 = tryCatch(html_table(subregions1, fill=TRUE)[[2]],
                     error= function(e) print(paste0("Error opening ",i,". Skip it.")))
    print(levl2)
    levl2
  })

  names(continents) <- link1
  cont_df <- do.call(rbind, continents)
  cont_df$contin <- rownames(cont_df)
  cont_df$contin <- gsub(pattern = "[\\.][0-9]*", replacement = "", cont_df$contin)
  rownames(cont_df) = NULL

  cont_df <- cont_df[!cont_df[,1] == "",]


  remo <- grep(pattern = "Error", x = cont_df[,1], fixed = F)
  if(length(remo)!=0){cont_df <- cont_df[-remo,]}

  colnames(cont_df) <- c("subregions", "osm_format", "size", "shp_format", "bz2_format", "continent")
  cont_df$size <- gsub(pattern = "[()]", replacement = "", cont_df$size)

  cont_df$kilobyte <- cont_df$size
  cont_df$unit <- "KB"
  cont_df[grep(pattern = "GB", fixed = T, cont_df$kilobyte),"unit"] = "GB"
  cont_df[grep(pattern = "MB", fixed = T, cont_df$kilobyte),"unit"] = "MB"
  cont_df[grep(pattern = "KB", fixed = T, cont_df$kilobyte),"unit"] = "KB"

  cont_df$kilobyte <- gsub(pattern = "(MB)|(GB)|(KB)|([[:space:]])", replacement = "", x = cont_df$kilobyte)
  cont_df$kilobyte <- as.numeric(cont_df$kilobyte)

  cont_df$kilobyte[cont_df$unit == "MB"] = cont_df$kilobyte[cont_df$unit == "MB"]*1000L
  cont_df$kilobyte[cont_df$unit == "GB"] = cont_df$kilobyte[cont_df$unit == "GB"]*1000L*1000L

  cont_df$continent <- gsub("(^|[[:space:]])([[:alpha:]])",
                            "\\1\\U\\2",
                            cont_df$continent, perl=TRUE)

  print(head(cont_df))
  cat("\n")
  print(summary(cont_df))

  return(cont_df)
}
# con_df <- summaryOSM()


#' @title treemapOSM
#' @name treemapOSM
#' @description Plot a treemap of the OSM summary dataset
#'
#' @export
#'
#' @importFrom sunburstR sunburst
#'
#' @param sumry The output of summaryOSM
#'
#' @examples \donttest{
#' con_df <- summaryOSM()
#' treemapOSM(con_df)
#'}
#' @author Sebastian Gatscha
treemapOSM <- function(sumry) {
  # sumry=cont_df
  sumry$seq=NULL
  sumry$seq <- paste(sumry$continent, sumry$subregions, sep = "-")
  cont_plot <- sumry[,c("seq", "kilobyte")]
  sunburstR::sunburst(cont_plot, count=T)
}


#' @title mapviewOSM
#' @name mapviewOSM
#' @description Plot the OSM summary dataset with mapview.
#'
#' @export
#'
#' @import rworldmap
#' @importFrom sf st_as_sf st_cast
#' @importFrom mapview mapview
#' @importFrom dplyr group_by summarise %>%
#' @importFrom utils globalVariables
#'
#' @param sumry The output of summaryOSM
#' @param mergeby Should the values be assigned per continent
#' or per country. Default is 'country'
#' @param unit Which unit should be taken. Default is 'mb'
#' @param ... Arguments passed on to mapview
#'
#'
#' @examples \donttest{
#' con_df <- summaryOSM()
#' mapviewOSM(con_df, mergeby = "subregions", unit = "gb")
#'}
#' @author Sebastian Gatscha
utils::globalVariables(c("REGION","kilobyte"));
mapviewOSM <- function(sumry, mergeby="country", unit="mb",...) {
  # sumry <- con_df;mergeby="subregions"; unit="mb"
  mgb = c("country", "continent")
  if (!mergeby %in% mgb){stop(paste0("mergeby must be one of ", paste0(mgb,collapse = ", "),"."))}

  unit = tolower(unit)
  units <- c("b","kb","mb","gb","tb")
  if (!unit %in% units){stop(paste0("unit must be one of ", paste0(units,collapse = ", "),"."))}

  world <- st_as_sf(rworldmap::countriesLow)

  sumry <- sumry[,-c(2,4,5,8)]
  if(mergeby=="country") {
    sumry[sumry$subregions == "Russian Federation", "subregions"] = "Russia"
    sumry[sumry$subregions == "Cyprus","subregions"] = "Northern Cyprus"
    sumry[sumry$subregions %in% c("Nevada", "Kansas", "Idaho","Indiana","Georgia (US State)",
                            "Louisiana","Maryland","Michigan","Mississippi","Montana","Oregon",
                            "Nevada","New Jersey", "New York", "North Dakota", "Oklahoma",
                            "Pennsylvania", "Rhode Island","South Dakota","Texas","Vermont",
                            "Washington","Wisconsin","West Virginia","Virginia", "Wyoming",
                            "District of Columbia","Alabama","Arizona","Greenland","Alaska",
                            "Utah", "Tennessee", "South Carolina", "North Carolina","Ohio",
                            "New Mexico","New Hampshire", "Nebraska", "Missouri","Minnesota",
                            "Massachusetts", "Maine", "Kentucky","Iowa","Illinois","Hawaii",
                            "Florida", "Delaware","Colorado","Arkansas","Connecticut",
                            "California", "Puerto Rico"),"subregions"] = "United States of America"
    sumry[sumry$subregions == "Congo (Democratic Republic/Kinshasa)",
          "subregions"] = "Democratic Republic of the Congo"
    sumry[sumry$subregions == "Tanzania", "subregions"] = "United Republic of Tanzania"
    sumry[sumry$subregions == "Congo (Republic/Brazzaville)",
          "subregions"] = "Republic of the Congo"
    sumry[sumry$subregions == "Ukraine (with Crimea)", "subregions"] = "Ukraine"
    sumry[sumry$subregions == "Serbia", "subregions"] = "Republic of Serbia"
    sumry[sumry$subregions == "Great Britain", "subregions"] = "United Kingdom"
    sumry[sumry$subregions == "Isle of Man", "subregions"] = "United Kingdom"
    sumry[sumry$subregions == "Saint Helena, Ascension, and Tristan da Cunha",
          "subregions"] = "United Kingdom"
    sumry[sumry$subregions == "Ireland and Northern Ireland", "subregions"] = "Ireland"
    sumry[sumry$subregions == "Georgia (Eastern Europe)", "subregions"] = "Georgia"
    sumry[sumry$subregions == "Myanmar (a.k.a. Burma)", "subregions"] = "Myanmar"
    sumry[sumry$subregions == "Senegal and Gambia", "subregions"] = "Senegal"
    sumry[sumry$subregions == "Malaysia, Singapore, and Brunei", "subregions"] = "Malaysia"
    sumry[sumry$subregions == "Israel and Palestine", "subregions"] = "Israel"
    sumry[sumry$subregions == "Haiti and Dominican Republic", "subregions"] = "Haiti"
    sumry[sumry$subregions == "Bosnia-Herzegovina", "subregions"] = "Bosnia and Herzegovina"
    sumry[sumry$subregions == "Faroe Islands", "subregions"] = "Denmark"
    sumry[sumry$subregions == "Canary Islands", "subregions"] = "Spain"
    sumry[sumry$subregions == "Guinea-Bissau", "subregions"] = "Guinea Bissau"
    sumry[sumry$subregions == "Azores", "subregions"] = "Portugal"
    sumry[sumry$subregions == "GCC States", "subregions"] = "Saudi Arabia"
    sumry[sumry$subregions == "New Caledonia", "subregions"] = "France"


    world1 <- merge(x = world, y = sumry, by.x="SOVEREIGNT", by.y="subregions")

    print("No matching found:")
    print(base::setdiff(unique(sumry$subregions),
                  as.character(unique(world1$SOVEREIGNT))))



    # mapview(world1)
    # # world2 <- world
    # world2$geometry=NULL
    #
    # sort(unique(sumry$subregions))
    # paste(sort(unique(sumry$subregions)),collapse = " - ")
    # a <- sapply(colnames(world2), function(i) {
    #   "Comores" %in% world2[[i]]
    # }); which(a)
    #
    # world2[world2[[names(which(a)[1])]] == "New Caledonia",]


    # colnames(world1)
    world1 <- world1[,-c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,
                         20,21,22,23,26,27,28,29,30,31,32,33,34,35,36,37,38,
                         39,40,41,42,43,44,45,46,47,48,49,50)]

    colnames(world1) <- c("Country", "Type", "Pop_est", "GDP_est", "Continent",
                          "Size", "geometry")

  } else {
    world1 <- base::merge(x = world, y = sumry, by.x="REGION", by.y="continent")
    world1 <- world1 %>%
      group_by(REGION) %>%
      summarise(m = sum(kilobyte)) %>%
      st_cast()
    colnames(world1) <- c("Continent", "Size", "geometry")
  }

  mult = switch(unit,
                "b" = c(0.001, "Bytes"),
                "kb" = c(1L, "kb"),
                # "mb" = c(1024L, "MB"),
                "mb" = c(1000L, "MB"),
                # "gb" = c(1048576L, "GB"),
                "gb" = c(1000000L, "GB"),
                # "tb" = c(1073741824L, "TB"))
                "tb" = c(1000000000L, "TB"))

  world1$Size <- round(world1$Size / as.numeric(mult[1]), 2)

  # mapview(world1, zcol="Size", layer.name = paste0('Size in ',mult[2]))
  mapview(world1, zcol="Size", layer.name = paste0('Size in ',mult[2]), ...)
}
# con_df <- summaryOSM()
# library(mapview)
mapviewOSM(con_df, mergeby = "country", unit = "gb")
# mapviewOSM(cont_df, mergeby = "continent", unit = "gb")


# library(rworldmap)
# library(sf)
# library(mapview)
# library(dplyr)
