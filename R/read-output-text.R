
read.curves <- function(infile) {
  
  # Read in the curves file skipping the header line, we'll construct this 
  # later on
  curves <- read.table(infile, as.is=TRUE, header=FALSE, skip=1)
  # Standard header entries
  
  # The header has a set of standard components + proportion for each species
  # remaining at level of removal (created dynamically)
  header <- c("Prop_landscape_lost",           # 1
              "cost_needed_for_top_fraction",  # 2
              "min_prop_rem",                  # 3
              "ave_prop_rem",                  # 4
              "W_prop_rem",                    # 5
              "ext-1",                         # 6
              "ext-2")                         # 7
  
  # Populate the rest of the header lines with sp headers and assign it
  header <- c(header, paste("F", 1:(ncol(curves) - length(header)), sep=""))
  colnames(curves) <- header
  return(curves)
}

read.admu.curves <- function(file) {
  
  # First row is (again) malformatted 
  
}

read.grp.curves <- function(file) {
  
  grp.curves <- read.table(file, header=TRUE)
  
  # standard part of the header
  header <- c("F.lost", "TF_cost")
  
  # Repeating parts of the group curves header
  rep.header <- c("min", "mean", "max", "w.mean", "ext2")
  times <- (ncol(grp.curves) - length(header)) / length(rep.header)
  rep.header <- paste(rep(rep.header, times), rep(1:times, each=length(rep.header)), sep="-")
  header <- c(header, rep.header)
  colnames(grp.curves) <- header
  
  return(grp.curves)
  
}

read.stats <- function(wildcard=".cmp$") {
  
  data <- list()
  
  # Get all the comparisons (.cmp) files
  # TODO: fix the wildcard so that it's strict about the extension
  files <- list.files(pattern=wildcard)
  
  # Loop over the comparison files
  
  for (i in 1:length(files)) {
    
    thresh <- read.table(files[i], nrows=10, as.is=TRUE, header=TRUE)
    lines <- readLines(files[i])
    tot <- grep("Total correlation", lines, value=TRUE)
    tot <- as.numeric(tail(strsplit(tot, ":")[[1]], 1))
    data[[files[i]]] <- list(thresh=thresh, total=tot)
  }
  class(data) <- "z.comp.plot"
  return(data)
  
}