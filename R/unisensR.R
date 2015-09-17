library(rJava)

unisensR <- function(){
}

readValuesEntryUnisens <- function(unisensFolder, id){
	unisensFactory <- J('org.unisens.UnisensFactoryBuilder', 'createFactory')
	unisens <- J(unisensFactory, 'createUnisens', unisensFolder)
  start <- readStartTime(unisens)
  entry <- J(unisens, 'getEntry', id)
	timedEntry <- .jcast(entry, new.class = "org.unisens.ValuesEntry", check = TRUE, convert.array = FALSE)
	sampleRate <- J(timedEntry, 'getSampleRate')
  csvData <- read.csv(paste(unisensFolder, id, sep = .Platform$file.sep), header = FALSE, sep = ",")
	csvData <- setTime(csvData, start, sampleRate)
	csvData <- setColumnNames(timedEntry, csvData)
  return(csvData)
}

setTime <- function(data, startTime, sampleRate) {
  fix <- function(x) startTime + x / sampleRate
  data[c("V1")] <- lapply(data[c("V1")], fix)
  return(data)
}

setColumnNames <- function(entry, data) {
  channelNames <- c('Time',J(entry, 'getChannelNames'))
  colnames(data) <- channelNames
  return(data)
}

readStartTime <- function(unisens) {
  timeStampStart <- J(unisens, 'getTimestampStart')
  timeStampStartUnix <- J(timeStampStart, 'getTime')
  start <- as.POSIXct(timeStampStartUnix/1000, origin="1970-01-01")
  return(start)
}
