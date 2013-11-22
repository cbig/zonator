# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

create_batch_file <- function(filename = "run_multiple.bat", exe="zig2c.exe",
							  param="-r", dat, spp, output, uc=0.0, ds=0,
							  am=1.0, win=1, append=FALSE) {
	line <- paste("call", exe, param, dat, spp, output, uc, ds, am, win, "\n")

	task <- ifelse(append, "edited", "created")
	cat(line, file=filename, append=append)

	cat(paste("\n<<<", task, "batch file"), filename)
}

create_spp_file <- function(filename="filelist.spp", weight=1.0, alpha=1.0,
							bqp=1, bqp.p=1, cellrem=1, sppfiles) {
	for (i in 1:length(sppfiles)) {
		append <- ifelse (i == 1, FALSE, TRUE)
		line <- paste(weight, alpha, bqp, bqp.p, cellrem, sppfiles[i], "\n")
		cat(line, file=filename, append=append)
	}
	cat(paste("\n<<< Created spp file"), filename)
}

