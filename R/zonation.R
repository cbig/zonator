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
# data.folder <- file.path(root.folder, "data")
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
