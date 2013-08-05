# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Check if Zonation is installed.
#'
#' @param exe Chrarcter string for overriding the default Zonation executable
#'   (default: zig3).
#' 
#' @return A logical indicating whether requested Zonation executable is found.
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
#' @examples \dontrun{
#'   check_zonation("zig4")
#' }
#' 
check_zonation <- function(exe="zig3") {

  if (.Platform$OS.type == "unix") {
    z.exe <- exe
  } else  {
    z.exe <- paste0(exe, ".exe")
  }
  # FIXME: works only on Linux
  check <- system(paste("which", z.exe), intern=FALSE, ignore.stdout=TRUE,
                  ignore.stderr=TRUE)
  if (check == 0) {
    return(TRUE)
  } else {
    warning("Zonation executable ", z.exe, " not found in the system.")
    return(FALSE)
  }
}

# # Function for calculation the distribution smoothing related alpha value
# calculate.alpha <- function(cell.size, landscape.use, input.cell.size) {
#   return((2 * cell.size) / (landscape.use * input.cell.size))
# }
# 
# # Load the biodiversity features
# load.bdf <- function(data.folder, wildcard) {
#   bdf.files <- list.files(file.path(data.folder), wildcard)
# }
# 
# root.folder <- file.path(getwd(), "z")
# dat.file <- paste("\"", file.path(root.folder, "1_60_caz_ds.dat"), "\"", sep="")
# spp.file <- paste("\"", file.path(root.folder, "1_60_caz_ds.spp"), "\"", sep="")
# bat.file <- file.path(root.folder, "tee_1_60_caz_ds.bat")
# out.file <- paste("\"", file.path(root.folder, "output/result_1_60_ds_caz.txt"), "\"", sep="")
# 
# # Change this for Z exe location
# exe.file <- paste("\"", "C:/Program Files/zonation 3.1.1/bin/zig3.exe", "\"", 
#                   sep="")
# 
NULL
# wildcard <- "*.img$"
# 
# BDFs <- load.bdf(data.folder, wildcard)
# 
# splist <- c()
# 
# # First with DS
# for (i in 1:length(BDFs)) {
#   spp <- file.path(data.folder, BDFs[i])
#   splist <- c(splist, paste("1.0", alpha, 1, 1, 1.0, spp, sep=" ")) 
# }
# 
# # Then with just the local quality
# for (i in 1:length(BDFs)) {
#   spp <- file.path(data.folder, BDFs[i])
#   splist <- c(splist, paste("1.0", 1, 1, 1, 1.0, spp, sep=" ")) 
# }
# 
# # Save the data in text file format
# write.table(splist, file=file.path(root.folder, "1_60_caz_ds.spp"), quote = F, 
#             row.names= F, col.names = F) 
# 
# # Generate the bat file and run it
# cmd <- c()
# 
# if (.Platform$OS.type == "unix") {
#   cmd <- "#!/bin/bash"
#   exe.file <- paste("\"", file.path(root.folder, "zig3"), "\"", sep="")
#   cmd <- c(cmd, paste(exe.file, "-r", dat.file, spp.file, 
#                       out.file, 0.0, 1, 1, 1))
#   write.table(cmd, file= bat.file, quote = F, row.names= F, col.names = F)
#   system(paste("chmod +x", bat.file))
#   system(bat.file)
# } else {
#   #exe.file <- paste("\"", file.path(root.folder, "zig3.exe"), "\"", sep="")
#   cmd <- paste("call", exe.file, "-r", dat.file, spp.file, out.file, 0.0, 1, 1, 
#                1)
#   write.table(cmd, file= bat.file, quote = F, row.names= F, col.names = F)
#   shell(paste("\"", bat.file, "\"", sep=""))
# }
