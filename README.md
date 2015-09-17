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
location <- readUnisensEventEntry('C:/path/to/unisens/folder', 'SMS.csv')
```

## Troubleshooting
If you receive the following error during installation:
```r
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: inDL(x, as.logical(local), as.logical(now), ...)
  error: unable to load shared object 'C:/Users/me/Documents/R/win-library/2.13/rJava/libs/x64/rJava.dll':
  LoadLibrary failure:  %1 is not a valid Win32 application.
```
Try to execute this line before installing.
```r
if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="");
```