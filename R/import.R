#' Imports the envirocar data
#' 
#' @param serverUrl url to server
#' @param trackIDs ids of tracks that should be retrieved
#' @param bbox spatial bounding box
#' @param timeInterval interval
#' @return Tracks objects for the requested tracks
#' TODO: unclear how to encode temporalFilter!!
#' 
importEnviroCar = function(serverUrl, trackIDs, bbox, timeInterval) {
  require(rjson) # fromJSON
  require(maptools) # spCbind
  require(rgdal) #readOGR
  require(RCurl) #getURL
  require(stringr) #str_replace_all
  require(spacetime) #Tracks
  
  if (missing(trackIDs)){
    trackIDs = getTrackIDs()
  }
  
  # read data as spatial object:
  layer = readOGR(getURL(file,ssl.verifypeer = FALSE), layer = "OGRGeoJSON")
  
  # convert time from text to POSIXct:
  layer$time = as.POSIXct(layer$time, format="%Y-%m-%dT%H:%M:%SZ")
  # the third column is JSON, we want it in a table (data.frame) form:
  # 1. form a list of lists
  l1 = lapply(as.character(layer[[3]]), fromJSON)
  # 2. parse the $value elements in the sublist:
  l2 = lapply(l1,
              function(x) as.data.frame(lapply(x, function(X) X$value)))
  # create a matrix with all columns and then convert it to a data frame
  # thanks to Kristina Helle!
  # dynamic parsing of phenomenon names and units
  phenomenonsUrl = "https://www.envirocar.org/api/stable/phenomenons"
  phenomenons = fromJSON(getURL(phenomenonsUrl))
  colNames <- str_replace_all(sapply(phenomenons[[1]], "[[", "name"), pattern=" ", repl=".")
  
  resultMatrix = matrix(nrow=length(l2),ncol=length(colNames))
  dimnames(resultMatrix)[[2]]=colNames
  for (i in seq(along = l2))
    resultMatrix[i,names(l2[[i]])]=as.numeric(l2[[i]])
  result = as.data.frame(resultMatrix)
  
  # set the units:
  units <- sapply(phenomenons[[1]], "[[", "unit")
  names(units)=colNames
  
  # add a units attribute to layer
  layer[[3]] = NULL
  # add the table as attributes to the spatial object 
  if (length(layer) == nrow(result)) {
    layer = spCbind(layer, result)
    attr(layer, "units") = units
    layer
  } else
    NULL
}

#' Imports the envirocar data
#' 
#' @param bbox spatial bounding box
#' @param timeInterval interval
#' @return Tracks objects for the requested tracks
#' TODO: unclear how to encode temporalFilter!!
#' 
getTrackIDs <- function(serverUrl,bbox,timeInterval){
  library(RCurl)
  body = getURI( "https://envirocar.org/api/dev/tracks",ssl.verifypeer=FALSE,header=1)
  headerString = strsplit(body, split="[{]")[[1]][1]
  headerString
  header = parseHTTPHeader(headerString)
  header
  headerList=as.vector(header)
  headerList
  ?lapply
  lapply(headerList,parseLinkHeaderParam)
  headerParam = header["Link"]
  rel
  value(header)
  print(header)
  typeof(header)
}

#' Imports the envirocar data
#' 
#' @param serverUrl url to server
#' @param trackIDs ids of tracks that should be retrieved
#' @param bbox spatial bounding box
#' @param timeInterval interval
#' @return Tracks objects for the requested tracks
#' TODO: unclear how to encode temporalFilter!!
#'
parseLinkHeaderParam <- function(headerParam){
  if (grep("Link",headerParam)){
    lastpart=strsplit(headerParam,"&page=")[[1]][2]
    lastpartSplitted=strsplit(lastpart,"[>]")[[1]]
    pageNumberString = lastpartSplitted[1]
    rel = strsplit(strsplit(lastpartSplitted[2],"rel=")[[1]][2],";type")[[1]][1]
    c(pageNumberString,rel)
    }
  else NULL
}