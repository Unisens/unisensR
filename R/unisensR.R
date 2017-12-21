library(XML)

#' @import XML
#' @importFrom utils read.csv
#'
NULL

namespaces <- c(ns="http://www.unisens.org/unisens2.0")

#' Read Unisens Values Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the values entry.
#' @return DataFrame.
#' @examples
#' readUnisensValuesEntry('../UnisensR/inst/extdata/unisensExample', 'rr.csv')
readUnisensValuesEntry <- function(unisensFolder, id){
  if(unisensXMLExists(unisensFolder)){
    doc <- xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    xpath <- paste("//ns:valuesEntry[@id='", id, "']", sep = '')
    entries <- getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No ValuesEntry found with name', id))
    entry <- entries[[1]]
    sampleRate <- as.numeric(xmlGetAttr(entry, "sampleRate"))
    csvData <- read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = ",")
    csvData <- setTime(csvData, start, sampleRate)
    csvData <- setValuesEntryColumnNames(entry, csvData)
    free(doc)
    return(csvData)
  }
  else
    stop('Folder does not contain Unisens data!')
}

unisensXMLExists <- function(unisensFolder){
  unisensXML <- paste(unisensFolder, 'unisens.xml', sep = '/')
  file.exists(unisensXML)
}

#' Read Unisens Event Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the event entry.
#' @return DataFrame.
#' @examples
#' readUnisensEventEntry('../UnisensR/inst/extdata/unisensExample', 'qrs-trigger.csv')
readUnisensEventEntry <- function(unisensFolder, id){
  if(unisensXMLExists(unisensFolder)){
    doc <- xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    xpath <- paste("//ns:eventEntry[@id='", id, "']", sep = '')
    entries <- getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No EventEntry found with name', id))
    entry <- entries[[1]]
    sampleRate <- as.numeric(xmlGetAttr(entry, "sampleRate"))
    csvData <- read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = ",")
    csvData <- setTime(csvData, start, sampleRate)
    csvData <- setEventEntryColumnNames(entry, csvData)
    free(doc)
    return(csvData)
  }
  else
    stop('Folder does not contain Unisens data!')
}

setTime <- function(data, startTime, sampleRate) {
  fix <- function(x) startTime + x / sampleRate
  data[c("V1")] <- lapply(data[c("V1")], fix)
  return(data)
}

setValuesEntryColumnNames <- function(entry, data) {
  entryDoc <- xmlDoc(entry)
  channels <- getNodeSet(entryDoc, "//ns:channel", namespaces )
  free(entryDoc)
  channelNames <- sapply(channels, function(el) xmlGetAttr(el, "name"))
  colnames(data) <- c('Time', channelNames)
  return(data)
}

setEventEntryColumnNames <- function(entry, data) {
  channelNames <- c('Time', 'Marker', 'Comment')
  colnames(data) <- channelNames
  return(data)
}

readStartTime <- function(doc) {
  startTimeString <- xmlGetAttr(xmlRoot(doc), "timestampStart")
  start <- as.POSIXct(strptime(startTimeString, "%Y-%m-%dT%H:%M:%S"))
  return(start)
}

#' Read Unisens Start Time
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @return POSIXct unisens start time
#' @examples
#' readUnisensStartTime('../UnisensR/inst/extdata/unisensExample')
readUnisensStartTime <- function(unisensFolder) {
  if(unisensXMLExists(unisensFolder)){
    doc <- xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    free(doc)
    start
  }
  else
    stop('Folder does not contain Unisens data!')
}
