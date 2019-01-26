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


#' @title convertOSM
#' @name convertOSM
#' @description Basic Interface to osmconvert.
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
#' @param osmconvert Name of the executable. Default is 'osmconvert'
#'
#' @return The osmconvert call.
#'
#' @examples \donttest{
#' dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
#' convertOSM(dest, cm=T, cb = F, cw = F, fname = "capverde4", ext = "pbf")
#'}
#' @author Sebastian Gatscha
convertOSM <- function(destfin, ext="osm", cm=FALSE, cb=FALSE, cw=FALSE,
                       bbox=NULL, fname=NULL, poly=NULL, osmconvert="osmconvert") {
  h <- system(osmconvert, ignore.stdout = F,ignore.stderr = F)
  if (h!=0) { stop("osmconvert is not installed or not set as system variable.")}

  extarg = c("osm", "o5m", "pbf", "bz2", "osc", "osh", "o5c", "osc.gz")
  if (!ext %in% extarg) {
    stop(paste("ext must be one of", paste(extarg, collapse = ", ")))
  }

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


#' @title osmosisR
#' @name osmosisR
#' @description Basic Interface to osmosis.
#'
#' @export
#'
#' @importFrom utils tail
#'
#' @param input The input file path
#' @param savename Output name
#' @param idtracker If TRUE, sets idTrackerType=BitSet
#' @param usednode If TRUE, sets --used-node
#' @param usedway If TRUE, sets --used-way
#' @param extract_nodes Must be given as key-value pair, like 'natural.tree'
#' @param extract_ways Must be given as key-value pair.
#' @param bbox Extract data with bounding box coordinates in lat/lon
#' @param poly Extract data with .POLY files
#' @param filter_nodes Use --tag-filter with nodes
#' @param filter_ways Use --tag-filter with ways
#' @param filter_relations Use --tag-filter with relations
#' @param nodes_accept logical, if nodes should be accepted or rejected.
#' Default is TRUE
#' @param ways_accept logical, if ways should be accepted or rejected.
#' Default is TRUE
#' @param relation_accept logical, if relations should be accepted or
#' rejected. Default is TRUE
#'
#' @return The osmosis call.
#'
#' @examples \donttest{
#' dest <- getOSM(filterby="osm", exclude = "md5", r1 = 2, r2 =13, dest="")
#' osmosisR(dest, savename = "highway_capeverde.osm",
#'          filter_ways = "highway=*")
#'
#' osmosisR(dest,
#'          savename = "motorways_dest.osm",
#'          usednode = T, usedway = T,
#'          ways_accept = F,filter_ways = "",
#'          filter_nodes = "natural=tree")
#'
#' ## Get only tags with 'building=yes' and reject ways and relations.
#' osmosisR(dest,
#'          savename = "buildings_dest.osm",
#'          filter_relations = T,filter_ways = "",
#'          filter_nodes = "building=yes",
#'          relation_accept = F, nodes_accept =  T,
#'          ways_accept = F)
#'
#' ## Filter for multiple tag values (beach, tree)
#' osmosisR(dest,
#'         savename = "buildings_dest.osm",
#'         filter_nodes = "natural=beach,tree")
#'
#' ## Extract data based on node key-values
#' osmosisR(dest,
#'          savename = "tree_beach_dest.osm",
#'          usednode=F, usedway = T,
#'          extract_nodes = "natural.tree,natural.beach")
#'
#'}
#' @author Sebastian Gatscha
osmosisR <- function(input, savename="output.osm",
                     idtracker=FALSE, usednode=FALSE, usedway=FALSE,
                     extract_nodes=NULL, extract_ways=NULL,
                     bbox=NULL, poly=NULL,
                     filter_nodes=NULL, nodes_accept=TRUE,
                     filter_ways=NULL, ways_accept=TRUE,
                     filter_relations=NULL, relation_accept=TRUE) {

  ## Check if osmosis is accessible and input exists ###################
  h <- system("osmosis", ignore.stdout = T,ignore.stderr = T)
  if (h!=0) {stop("osmosis is not installed or not set as system variable.")}

  if (is.logical(filter_relations) && isTRUE(filter_relations)) {filter_relations=""}
  if (is.logical(filter_ways) && isTRUE(filter_ways)) {filter_ways=""}
  if (is.logical(filter_nodes) && isTRUE(filter_nodes)) {filter_nodes=""}

  extarg = c("osm", "pbf")
  ext = tail(strsplit(basename(input),".", fixed = T)[[1]],1)
  if (!ext %in% extarg) {stop(paste("ext must be one of",
                                    paste(extarg, collapse = ", ")))}
  if(!file.exists(input)){stop(paste0("File does not exist: ", input))}

  readinp <- ifelse(ext=="pbf", " --read-pbf ", " --read-xml ")

  ## Used-Nodes / Used-Ways ######################
  unod <- ifelse(usednode, " --used-node ", "")
  uway <- ifelse(usedway, " --used-way ", "")

  ## Extract with keyValueList  ######################
  if(!is.null(extract_nodes)) {
    extr_nodes <- paste0(' --node-key-value keyValueList=',
                         paste0(extract_nodes, collapse = ','), ' ')
  } else {
    extr_nodes = NULL
  }
  if(!is.null(extract_ways)) {
    extr_ways <- paste0(' --way-key-value keyValueList=',
                        paste0(extract_ways, collapse = ','), ' ')
  } else {
    extr_ways = NULL
  }

  ## Filter with --tag-filter ######################
  if(!is.null(filter_ways)) {
    way_acc <- ifelse(ways_accept, " accept-ways ", " reject-ways ")
    filt_way <- paste0(" --tf ", way_acc,
                       paste0(filter_ways, collapse = ","), " ")
  } else {
    filt_way = NULL
  }
  if(!is.null(filter_relations)) {
    rela_accept <- ifelse(relation_accept, " accept-relations ", " reject-relations ")
    filt_rela <- paste0(" --tf ", rela_accept,
                        paste0(filter_relations, collapse = ","), " ")
  } else {
    filt_rela = NULL
  }
  if(!is.null(filter_nodes)) {
    node_accept <- ifelse(nodes_accept, " accept-nodes ", " reject-nodes ")
    filt_node <- paste0(" --tf ", node_accept,
                        paste0(filter_nodes, collapse = ","), " ")
  } else {
    filt_node = NULL
  }

  ## Area Filtering ######################
  bbox <- ifelse(!is.null(bbox), paste(" --bounding-box ", bbox, " "), "")
  poly <- ifelse(!is.null(poly), paste(" --bounding-polygon file='", poly, "'"), "")
  idtracker <- ifelse(idtracker, " idTrackerType=BitSet ", "")

  ## Output Handling #################
  save = " --write-xml "

  outpath = paste0(c(dirname(input), savename), collapse = "/")
  if (length(grep(pattern = ".osm|.pbf", x= outpath, fixed = F))==0) {
    outpath <- paste0(outpath, ".osm")
  }

  ## Osmosis Call #################
  call <- paste0("osmosis",readinp, input, extr_nodes, extr_ways,
                 filt_node, filt_way, filt_rela, bbox, poly,
                 unod, uway, idtracker, save, outpath);
  print("Osmosis Call:")
  print(call)
  cat("\n")
  system(call)

  invisible(call)
}


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
#' @param ... Arguments passed on to mapview.
#' See \code{\link[mapview]{mapview}} or
#' \href{https://r-spatial.github.io/mapview/}{Mapview}
#'
#' @examples \donttest{
#' ## Get a summary dataset and plot it
#' con_df <- summaryOSM()
#' mapviewOSM(con_df, mergeby = "country", unit = "gb")
#'
#' ## With Custom col.regions
#' YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
#' colfunc = colorRampPalette(YlOrBr, space = "Lab")
#' mapviewOSM(con_df, mergeby = "country", col.regions = colfunc)
#'
#' ## With custon col.regions and breaks
#' jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#'                        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#' mapviewOSM(con_df, mergeby = "country", unit = "gb", burst=F,
#'           col.regions = jet.colors, at = 0:3)
#'
#' ## With custom popups
#' mapviewOSM(con_df, mergeby = "country",
#'            popup = mapview::popupTable(con_df, zcol = c("subregions", "kilobyte"),
#'                                       feature.id = F))
#'}
#' @author Sebastian Gatscha
utils::globalVariables(c("REGION","kilobyte"));
mapviewOSM <- function(sumry, mergeby="country", unit="mb",...) {
  mgb = c("country", "continent")
  if (!mergeby %in% mgb){stop(paste0("mergeby must be one of ", paste0(mgb,collapse = ", "),"."))}

  unit = tolower(unit)
  units <- c("b","kb","mb","gb","tb")
  if (!unit %in% units){stop(paste0("unit must be one of ", paste0(units,collapse = ", "),"."))}

  world <- sf::st_as_sf(rworldmap::countriesLow)

  sumry <- sumry[,-c(2,4,5,8)]
  if(mergeby=="country") {
    print("Merge by Countries")

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
    print("Merge by Continents")

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

  # mapview::mapview(world1, zcol="Size", layer.name = paste0('Size in ',mult[2]))
  mapview::mapview(world1, zcol="Size", layer.name = paste0('Size in ',mult[2]), ...)
}


