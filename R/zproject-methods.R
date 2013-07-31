#' getVariant
#' Get a specified variant in a Zonation project
#'
#' @param x Zproject object
#' @param index int or string index defining the variant required
#'
#' @return Zvariant object
#' 
#' @docType methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("getVariant", c("Zproject", "ANY"), function(x, index) {
  return(x@variants[[index]])
}
)

#' nvariants
#' Get the number of variants included in a Zonation project
#'
#' @param x Zproject object
#'
#' @return int number of variants
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("nvariants", c("Zproject"), function(x) {
  return(length(x@variants))
}
)

setMethod("names", "Zproject", function(x) {
  return(names(x@variants))
}
)

#' open.dir
#' Open the directory of a Zproject using the system file browser.
#' 
#' Currently support Windows Explorer (Windows) amd Dolphin (Linux/KDE)
#'
#' @param x object
#'
#' @return invisible
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("opendir", c("Zproject"), function(object) {
  invisible(open.dir(object@root))
}
)