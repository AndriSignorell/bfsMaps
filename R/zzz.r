
.onLoad <- function(libname, pkgname) {

  # does not work
  # .env <- new.env()
  # attach(.env)
  # assign(x = "fmt", value = fmt, envir = .env)


  # assign("tkart", new.env(), parent.env("bfsMaps"))

  invisible()
}

# prepare map cache environment
tkart <- new.env()

# store default values
.pardefault <- par(no.readonly=TRUE)


