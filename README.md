# unisensR

This library is a prototype to demonstrate how to use the unisens file format in R.

Currenlty it is only supported to read CSV ValuesEntries and EventEntries with comma separated values.

## How to install unisensR
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
