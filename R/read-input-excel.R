# Function for reading in Zonation result rasters in various formats

readWorksheet.disjoint <- function(wb, sheet, regions, ...) {
  
  regions <- unlist(strsplit(regions, ";"))
  
  data.regions <- data.frame()
  
  for (i in 1:length(regions)) {
    if (i == 1) {
      data.regions <- readWorksheet(wb, sheet, region = regions[i], 
                                    header=TRUE)
    } else {
      temp <- readWorksheet(wb, sheet, region = regions[i], header=FALSE)
      # Use colnames fromt the first read
      colnames(temp) <- colnames(data.regions)
      data.regions <- rbind(data.regions, temp)
    }
  }
  
  return(data.regions)
}