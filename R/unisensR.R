namespaces <- c(ns="http://www.unisens.org/unisens2.0")

#' Read Unisens Signal Entry
#'
#' @export
#'
#' @param unisensFolder String containing path to Unisens folder.
#' @param id String containing ID of the signal entry.
#' @param startIndex Integer of the value-index on which the read process starts, default: 1.
#' @param endIndex Integer of the value-index on which the read process ends, default: last Index of file.
#' @param readInChunks Boolean determines if the reading process is done in chunks.
#' This could be useful if you run into memory limits when reading big files. default: FALSE.
#' @param readChunkSize Integer defining the size of chunks if chunk reading is enabled, defined in samples, default: 2^16.
#' @return DataFrame.
#' @examples
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' readUnisensSignalEntry(unisensPath, 'ecg.bin')
readUnisensSignalEntry <- function( unisensFolder, id, startIndex = 1, endIndex = getUnisensSignalSampleCount(unisensFolder, id), readInChunks = FALSE, readChunkSize = 2^16 ){
  if(unisensXMLExists(unisensFolder)){

    signalSampleCount <- getUnisensSignalSampleCount(unisensFolder, id)
    if (startIndex < 1 || startIndex > signalSampleCount) {stop("startIndex out of bounds.")}
    if (endIndex > signalSampleCount) {stop("endIndex out of bounds.")}
    if (endIndex < startIndex) {stop("endIndex has to be greater or equal to startIndex.")}
    if (readChunkSize <= 0) {stop('readChunkSize has to be greater than zero!')}

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

      rbN <- (endIndex - startIndex + 1) * nChannels
      totalLinesToRead <- endIndex - startIndex + 1
      readVector <- getReadVector(
        readCount = rbN,
        readInChunks = readInChunks,
        readChunkSize = readChunkSize
      )

      signalDataVec <- vector()
      for (index in 1:length(readVector)) {
        signalDataVec <- c(signalDataVec, hexView::blockValue(hexView::readRaw(
          file = entryPath,
          offset = ( (startIndex - 1) * nChannels + ( (index - 1) * readChunkSize) ) * rbSize,
          nbytes = readVector[index] * rbSize,
          human = "int",
          size = rbSize,
          endian = "little",
          signed = rbSigned
        )))
      }

      signalData <- matrix(signalDataVec, ncol = nChannels, byrow = TRUE)
      signalDataFrame <- as.data.frame(signalData)
    }
    else if (length(XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )) == 1)
    {
      csvFileFormatElement<-XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )[[1]]
      separator <- XML::xmlGetAttr(csvFileFormatElement, "separator")

      totalLinesToRead <- endIndex - startIndex + 1
      readVector <- getReadVector(
        readCount = totalLinesToRead,
        readInChunks = readInChunks,
        readChunkSize = readChunkSize
      )

      signalDataFrame  <- data.frame()

      for (index in 1:length(readVector)) {
        signalDataFrame <- rbind(
          signalDataFrame,
          utils::read.csv(
            file = paste(unisensFolder, id, sep = .Platform$file.sep),
            header = FALSE,
            sep = separator,
            skip = startIndex - 1 + ( (index - 1) * readChunkSize ),
            nrows = readVector[index],
            flush = TRUE
          ))
      }
      # delete all columns that only contain 'NA'
      signalDataFrame <- signalDataFrame[colSums(!is.na(signalDataFrame)) > 0]
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

#' Get Read Vector for signal reading
#' Read Vector contains the number of samples that should be read in one reading process
#' @param readCount Integer ( >= 0 ) defining the number of data-entities (integers, lines) that should be read
#' @param readInChunks Boolean defines if chunk reading is enabled or not
#' @param readChunkSize Integer ( > 0 ) defining the size of reading chunks if chunk reading is enabled
#' @noRd
getReadVector <- function (readCount, readInChunks, readChunkSize) {
  if (readCount < 0) {stop('readCount can\'t be negative!')}
  if (readChunkSize <= 0) {stop('readChunkSize has to be greater than zero!')}

  readVector <- vector()
  if (readInChunks) {
    readCountQuotient <- readCount %/% readChunkSize
    readCountRemainder <- readCount %% readChunkSize
    readVector <- rep(readChunkSize, readCountQuotient)
    if (readCountRemainder > 0) {
      readVector <- c(readVector, readCountRemainder)
    }
  } else {
    readVector <- c(readVector, readCount)
  }
  return(readVector)
}

#' Get Unisens Signal Sample Count
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the signal entry
#' @return Long
#' @examples
#' unisensPath <- system.file('extdata/unisensExample', package = 'unisensR', mustWork = TRUE)
#' getUnisensSignalSampleCount(unisensPath, 'ecg.bin')
getUnisensSignalSampleCount <- function (unisensFolder, id) {
  if(unisensXMLExists(unisensFolder)){
    doc <- XML::xmlParse(paste(unisensFolder, 'unisens.xml', sep = '/'))
    xpath <- paste("//ns:signalEntry[@id='", id, "']", sep = '')
    entries <- XML::getNodeSet(doc, xpath, namespaces )
    if(length(entries) <= 0) stop(paste('No SignalEntry found with name', id))
    entry <- entries[[1]]
    entryPath <- paste(unisensFolder, id, sep = .Platform$file.sep)

    channelNames <- getEntryChannelNames(entry)
    nChannels <- length(channelNames)

    if (length(XML::getNodeSet(entry, "ns:binFileFormat", namespaces )) == 1)
    {
      dataType <- XML::xmlGetAttr(entry, "dataType")
      if (dataType=="int16")
      {
        rbSize <- 2;
      }
      else if (dataType=="int32")
      {
        rbSize <- 4;
      }
      else
      {
        stop("Datatype not defined yet.")
      }

      rbN <- file.info(entryPath)$size / rbSize
      maxIndex <- rbN %/% nChannels
    }
    else if (length(XML::getNodeSet(entry, "ns:csvFileFormat", namespaces )) == 1)
    {
      maxIndex <- length(vroom::vroom_lines(entryPath))
    }
    else {
      stop('Unknown entry file format.');
    }
    XML::free(doc)
    return(maxIndex)
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
