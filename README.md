# unisensR

This library is a prototype to demonstrate how to use the unisens file format in R.

Currenlty it is only supported to read ValuesEntrys

## How to install unisensR
```r
install.packages("devtools")
devtools::install_github('unisens/unisens4r')
```
## How to use unisensR
```r
    library(unisensR)
    location <- readValuesEntryUnisens('C:/path/to/unisens/folder', 'Location.csv')
```