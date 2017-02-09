#' @import rJava
#'
NULL

#' Read Unisens Values Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the values entry.
#' @return DataFrame.
#' @examples
#' readUnisensValuesEntry('C:/path/to/unisens/folder', 'Location.csv')
readUnisensValuesEntry <- function(unisensFolder, id){
  unisensFactory <- rJava::J('org.unisens.UnisensFactoryBuilder', 'createFactory')
  unisens <- rJava::J(unisensFactory, 'createUnisens', unisensFolder)
  start <- readStartTime(unisens)
  entry <- rJava::J(unisens, 'getEntry', id)
  timedEntry <- rJava::.jcast(entry, new.class = "org.unisens.ValuesEntry", check = TRUE, convert.array = FALSE)
  sampleRate <- rJava::J(timedEntry, 'getSampleRate')
  csvData <- read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = ",")
  csvData <- setTime(csvData, start, sampleRate)
  csvData <- setValuesEntryColumnNames(timedEntry, csvData)
  return(csvData)
}

#' Read Unisens Event Entry
#'
#' @export
#'
#' @param unisensFolder Unisens Folder
#' @param id ID of the event entry.
#' @return DataFrame.
#' @examples
#' readUnisensEventEntry('C:/path/to/unisens/folder', 'SMS.csv')
readUnisensEventEntry <- function(unisensFolder, id){
  unisensFactory <- rJava::J('org.unisens.UnisensFactoryBuilder', 'createFactory')
  unisens <- rJava::J(unisensFactory, 'createUnisens', unisensFolder)
  start <- readStartTime(unisens)
  entry <- rJava::J(unisens, 'getEntry', id)
  timedEntry <- rJava::.jcast(entry, new.class = "org.unisens.EventEntry", check = TRUE, convert.array = FALSE)
  sampleRate <- rJava::J(timedEntry, 'getSampleRate')
  csvData <- read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = ",")
  csvData <- setTime(csvData, start, sampleRate)
  csvData <- setEventEntryColumnNames(timedEntry, csvData)
  return(csvData)
}

setTime <- function(data, startTime, sampleRate) {
  fix <- function(x) startTime + x / sampleRate
  data[c("V1")] <- lapply(data[c("V1")], fix)
  return(data)
}

setValuesEntryColumnNames <- function(entry, data) {
  channelNames <- c('Time',J(entry, 'getChannelNames'))
  colnames(data) <- channelNames
  return(data)
}

setEventEntryColumnNames <- function(entry, data) {
  channelNames <- c('Time', 'Marker', 'Comment')
  colnames(data) <- channelNames
  return(data)
}

readStartTime <- function(unisens) {
  timeStampStart <- J(unisens, 'getTimestampStart')
  timeStampStartUnix <- J(timeStampStart, 'getTime')
  start <- as.POSIXct(timeStampStartUnix/1000, origin="1970-01-01")
  return(start)
}
