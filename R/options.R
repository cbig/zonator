# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

.options <- new.env()

# Graphics options --------------------------------------------------------

curve.theme <- theme(plot.title=element_text(face="bold", size=20),
                     axis.title.x=element_text(size=20),
                     axis.title.y=element_text(size=20),
                     axis.text.x=element_text(size=18),
                     axis.text.y=element_text(size=18),
                     axis.ticks = element_line(size = 1),
                     legend.text=element_text(size=14),
                     legend.title=element_text(size=16),
                     panel.background = element_rect(fill='white', colour='black'),
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_line(size = 0.5, colour='lightgrey'),
                     panel.grid.minor.y = element_blank(),
                     legend.background = element_rect(fill='white', colour='white'))
assign("curve.theme", curve.theme, envir=.options)

assign("curve.x.title", "\nProp. of landscape lost", envir=.options)
assign("curve.x.title.invert", "\nProportion of landscape under conservation",
       envir=.options)
assign("curve.y.title", "Prop. of ditributions remaining\n", envir=.options)
assign("curve.legend.title", "Features", envir=.options)
assign("grp.curve.legend.title", "Feature groups", envir=.options)

# Zonation tutorial data paths ----------------------------------------------

# Tutorial directories
assign("tutorial.dir", system.file("extdata", "tutorial", package="zonator"), 
       envir=.options)
assign("setup.dir", file.path(.options$tutorial.dir, "basic"), envir=.options)
assign("data.dir", file.path(.options$tutorial.dir, "data"), envir=.options)
assign("output.dir", file.path(.options$setup.dir, "basic_output"), 
       envir=.options)

# Tutorial run and configuration files
assign("bat.file", file.path(.options$setup.dir, "01_core_area_zonation.bat"), 
       envir=.options)
assign("dat.file", file.path(.options$setup.dir, 
                             "01_core_area_zonation/01_core_area_zonation.dat"),
       envir=.options)
assign("spp.file", file.path(.options$setup.dir, 
                             "01_core_area_zonation/01_core_area_zonation.spp"),
       envir=.options)
assign("species.files", paste0(.options$data.dir, "/species", 1:7, ".tif"),
       envir=.options)
assign("groups.file", file.path(.options$setup.dir, "groups.txt"), 
       envir=.options)