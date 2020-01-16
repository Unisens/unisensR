# unisensR

This Library provides functions to import Unisens data into R. Unisens is a universal data format for multi sensor data.

Currently it supports to read CSV ValuesEntries and EventEntries with comma separated values and BIN SignalEntries in int16 and int32 data types.

## How to install unisensR

### From CRAN

```r
install.packages("unisensR")
```

### From GitHub

```r
install.packages("devtools")
library(devtools)
devtools::install_github('Unisens/unisensR')
```

## How to use unisensR
```r
library(unisensR)
location <- readUnisensValuesEntry('C:/path/to/unisens/folder', 'Location.csv')
sms <- readUnisensEventEntry('C:/path/to/unisens/folder', 'SMS.csv')
startTime <- readUnisensStartTime('C:/path/to/unisens/folder')
```
