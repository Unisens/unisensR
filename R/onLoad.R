library(rJava)
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname) # jars parameter did not work
  jars = c('org.unisens.jar', 'org.unisens.ri.jar')
  classes <- system.file("java", package = pkgname, lib.loc = libname)
  if (nchar(classes)) {
    .jaddClassPath(classes)
    if (length(jars)) {
      .jaddClassPath(paste(classes, jars, sep = .Platform$file.sep))
    }
  }
}
