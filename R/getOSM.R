#' @title getOSM
#' @name getOSM
#' @description Get OSM-Data from
#' \href{https://www.geofabrik.de/}{Geofabrik GmbH}
#'
#' @export
#'
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
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


  ## Subregions for Germany, France, zB ##################
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
