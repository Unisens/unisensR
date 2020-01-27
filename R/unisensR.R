namespaces <- c(ns="http://www.unisens.org/unisens2.0")

#' Read Unisens Signal Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the signal entry.
#' @return DataFrame.
#' @examples
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' readUnisensSignalEntry(unisensPath, 'ecg.bin')
readUnisensSignalEntry <- function(unisensFolder, id){
  if(unisensXMLExists(unisensFolder)){
    doc <- XML::xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    startTime <- readStartTime(doc)
    xpath <- paste("//ns:signalEntry[@id='", id, "']", sep = '')
    entries <- XML::getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No SignalEntry found with name', id))
    entry <- entries[[1]]

    sampleRate <- as.numeric(XML::xmlGetAttr(entry, "sampleRate"))

    lsbValue <- as.numeric(XML::xmlGetAttr(entry,"lsbValue"))
    if (length(lsbValue)==0)
    {
      lsbValue <- 1;
    }

    baseline <- as.numeric(XML::xmlGetAttr(entry,"baseline"))
    if (length(baseline) ==0)
    {
      baseline <- 0;
    }

    channelNames <- getEntryChannelNames(entry)
    nChannels <- length(channelNames)

    entryPath <- paste(unisensFolder, id, sep = .Platform$file.sep)

    if (length(XML::getNodeSet(entry, "ns:binFileFormat", namespaces )) == 1)
    {
      dataType <- XML::xmlGetAttr(entry, "dataType")
      if (dataType=="int16")
      {
        rbSize <- 2;
        rbType <- "integer"
        rbSigned <- 1
      }
      else if (dataType=="int32")
      {
        rbSize <- 4;
        rbType <- "integer"
        rbSigned <- 1
      }
      else
      {
        stop("Datatype not defined yet.")
      }

      rbN <- file.info(entryPath)$size / rbSize

      signalDataVec <- readBin(entryPath, rbType, n = rbN,  size = rbSize, signed = rbSigned, endian = "little")
      signalData <- matrix(signalDataVec, ncol = nChannels, byrow = TRUE)
      signalDataFrame <- as.data.frame(signalData)
    }
    else if (length(XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )) == 1)
    {
      csvFileFormatElement<-XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )[[1]]
      separator <- XML::xmlGetAttr(csvFileFormatElement, "separator")
      signalDataFrame <- utils::read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = separator)
    }
    else {
      stop('Unknown entry file format.');
    }
    if ((baseline!=0) | (lsbValue!=1))
    {
      signalDataFrame <- (signalDataFrame - baseline) * lsbValue
    }

    attr(signalDataFrame,"sampleRate") <- sampleRate
    attr(signalDataFrame, "startTime") <- startTime

    colnames(signalDataFrame) <- channelNames


    XML::free(doc)
    return(signalDataFrame)
  }
  else
    stop('Folder does not contain Unisens data!')
}

#' Read Unisens Values Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the values entry.
#' @return DataFrame.
#' @examples
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' readUnisensValuesEntry(unisensPath, 'rr.csv')
readUnisensValuesEntry <- function(unisensFolder, id){
  if(unisensXMLExists(unisensFolder)){
    doc <- XML::xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    xpath <- paste("//ns:valuesEntry[@id='", id, "']", sep = '')
    entries <- XML::getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No ValuesEntry found with name', id))
    entry <- entries[[1]]
    sampleRate <- as.numeric(XML::xmlGetAttr(entry, "sampleRate"))
    lsbValue <- as.numeric(XML::xmlGetAttr(entry,"lsbValue"))
    if (length(lsbValue)==0)
    {
      lsbValue <- 1;
    }

    baseline <- as.numeric(XML::xmlGetAttr(entry,"baseline"))
    if (length(baseline) ==0)
    {
      baseline <- 0;
    }
    if (length(XML::getNodeSet(entry, "ns:binFileFormat", namespaces )) == 1)
    {
      stop("Values in binFileFormat not implemented.")
    }
    else if (length(XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )) == 1)
    {
      csvFileFormatElement<-XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )[[1]]
      separator <- XML::xmlGetAttr(csvFileFormatElement, "separator")
      valuesDataFrame <- utils::read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = separator)
    }
    else {
      stop('Unknown entry file format.');
    }
    if ((baseline!=0) | (lsbValue!=1))
    {
      valuesDataFrame <- (valuesDataFrame - baseline) * lsbValue
    }
    valuesDataFrame <- setTime(valuesDataFrame, start, sampleRate)
    valuesDataFrame <- setValuesEntryColumnNames(entry, valuesDataFrame)

    XML::free(doc)
    return(valuesDataFrame)
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
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' readUnisensEventEntry(unisensPath, 'qrs-trigger.csv')
readUnisensEventEntry <- function(unisensFolder, id){
  if(unisensXMLExists(unisensFolder)){
    doc <- XML::xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    xpath <- paste("//ns:eventEntry[@id='", id, "']", sep = '')
    entries <- XML::getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No EventEntry found with name', id))
    entry <- entries[[1]]
    sampleRate <- as.numeric(XML::xmlGetAttr(entry, "sampleRate"))
    if (length(XML::getNodeSet(entry, "ns:binFileFormat", namespaces )) == 1)
    {
      stop("Event in binFileFormat not implemented.")
    }
    else if (length(XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )) == 1)
    {
      csvFileFormatElement<-XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )[[1]]
      separator <- XML::xmlGetAttr(csvFileFormatElement, "separator")
      eventDataFrame <- utils::read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = separator)
    }
    else {
      stop('Unknown entry file format.');
    }
    eventDataFrame <- setTime(eventDataFrame, start, sampleRate)
    eventDataFrame <- setEventEntryColumnNames(entry, eventDataFrame)
    XML::free(doc)
    return(eventDataFrame)
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
  channelNames <- getEntryChannelNames(entry)
  colnames(data) <- c('Time', channelNames)
  return(data)
}

getEntryChannelNames <- function(entry)
{
  channels <- XML::getNodeSet(entry, "ns:channel", namespaces )
  channelNames <- sapply(channels, function(el) XML::xmlGetAttr(el, "name"))
  return(channelNames)
}

setEventEntryColumnNames <- function(entry, data) {
  if (ncol(data) == 2)
    channelNames <- c('Time', 'Marker')
  else
    channelNames <- c('Time', 'Marker', 'Comment')
  colnames(data) <- channelNames
  return(data)
}

readStartTime <- function(doc) {
  startTimeString <- XML::xmlGetAttr(XML::xmlRoot(doc), "timestampStart")
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
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' readUnisensStartTime(unisensPath)
readUnisensStartTime <- function(unisensFolder) {
  if(unisensXMLExists(unisensFolder)){
    doc <- XML::xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    start <- readStartTime(doc)
    XML::free(doc)
    start
  }
  else
    stop('Folder does not contain Unisens data!')
}
