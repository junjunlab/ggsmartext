## S3 methods used by grid have to be registered explicitly because `grid`
## only knows about its own classes.
.onLoad <- function(libname, pkgname) {
  registerS3method(
    "makeContent", "ggse_smart_labels",
    makeContent.ggse_smart_labels,
    envir = asNamespace("grid")
  )
}

.onUnload <- function(libpath) {
  ## nothing to clean up currently
  invisible(NULL)
}
