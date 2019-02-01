# package initialization
.onLoad <- function(libname, pkgname){
  options(bigmemory.allow.dimnames=TRUE)
}

.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage('Initializing sensasim.')
  }
  senseasim$.init(T)
}
