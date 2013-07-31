setMethod("initialize", "Zproject", function(.Object, root) {
  
  if (!file.exists(root)) {
    stop(paste0("Root folder ", root, " not found")) 
  } else {
    .Object@root <- root
  }
  
  variants <- list()
  
  # List all the bat-files
  bat.files <- list.files(root, ".bat$", full.names=TRUE)
  
  for (bat.file in bat.files) {
    
    variants[bat.file] <- new("Zvariant", bat.file=bat.file)
  }
  
  .Object@variants <- variants 
  
  .Object
})

setMethod("initialize", "Zvariant", function(.Object, name=NULL, bat.file) {
  
  if (!file.exists(bat.file)) {
    stop(paste0("Variant .bat-file does not exist: ", bat.file))
  }
  
  if (is.null(name)) {
    # If no name is provided, use the name of the bat-file (without the 
    # extension)
    .Object@name <- strsplit(".", basename(bat.file))[[1]]
  } else {
    .Object@name <- name
  }
  .Object@bat.file <- bat.file
  # Read the content of the bat file
  .Object@call.params <- read.bat(bat.file)
  
  results <- list()
  
  # bat-file's existence has already been verified, try to get the output. 
  # bat-file includes a template for an output file, but we're more  interested
  # in the directory where that file resides in.
  output.folder <- dirname(.Object@call.params[["output.file"]]) 
  
  if (!is.null(output.folder)) {
    
    .get.file <- function(output.folder, x) {
      
      target <- list.files(output.folder, pattern=x, full.names=TRUE)
      if (length(target) == 0) {
        return(NA)
      } else if (length(target) == 1) {
        return(target)
      } else {
        warning(paste("More matches than 1 found for", x, "using only the first"))
        return(target[1])
      }
    }
    # Curves file is named *.curves.txt
    curve.file <- .get.file(output.folder, "\\.curves\\.txt")
    if (is.na(curve.file)) {
      warning(paste("In bat-file", bat.file, 
                    "Could not find curves file for folder ", output.folder)) 
      results[["curves"]] <- NA
    } else {
      results[["curves"]] <- read.curves(curve.file)
    }
    
    # Group curves file is named *.grp_curves.txt
    results[["grp.curves"]] <- read.grp.curves(.get.file(output.folder, 
                                                         "\\.grp_curves\\.txt"))
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- .get.file(output.folder, "\\.rank\\.")
    
    .Object@results <- results
  }
  
  .Object
})
