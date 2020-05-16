# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

library(ggplot2)

.options <- new.env()

assign("debug", FALSE, envir = .options)

# Graphics options --------------------------------------------------------

curve.theme <- ggplot2::theme(plot.title=element_text(face="bold", size=16),
                              axis.title.x=element_text(size=16),
                              axis.title.y=element_text(size=16),
                              axis.text.x=element_text(size=14),
                              axis.text.y=element_text(size=14),
                              axis.ticks = element_line(size = 1),
                              legend.text=element_text(size=12),
                              legend.title=element_text(size=14),
                              panel.background = element_rect(fill='white',
                                                              colour='black'),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.grid.major.y = element_line(size = 0.5,
                                                                colour='lightgrey'),
                              panel.grid.minor.y = element_blank(),
                              legend.background = element_rect(fill='white',
                                                               colour='white'))
assign("curve.theme", curve.theme, envir=.options)

assign("curve.x.title", "\nProp. of landscape lost", envir=.options)
assign("curve.x.title.invert", "\nProportion of landscape under conservation",
       envir=.options)
assign("curve.y.title", "Prop. of ditributions remaining\n", envir=.options)
assign("curve.legend.title", "Features", envir=.options)
assign("grp.curve.legend.title", "Feature groups", envir=.options)


# Custom color schemes ----------------------------------------------------

z_colors_spectral <- list(values=c(0.0, 0.2, 0.5, 0.75, 0.9, 0.95, 0.98, 1.0),
                          labels=c("0.00-0.20", "0.20-0.50", "0.50-0.75",
                                   "0.75-0.90", "0.90-0.95", "0.95-0.98",
                                   "0.98-1.00"),
                          colors=c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf",
                                   "#fdc980", "#f07c4a", "#d7191c"))

assign("z_colors_spectral", z_colors_spectral, env=.options)

z_colors_BrBG <- list(values=c(0.0, 0.2, 0.5, 0.75, 0.9, 0.95, 0.98, 1.0),
                      labels=c("0.00-0.20", "0.20-0.50", "0.50-0.75",
                               "0.75-0.90", "0.90-0.95", "0.95-0.98",
                               "0.98-1.00"),
                      colors=c("#5c5c5c", "#D8B365", "#F6E8C3", "#F5F5F5",
                               "#C7EAE5", "#5AB4AC", "#01665E"))

assign("z_colors_BrBG", z_colors_BrBG, env=.options)

z_colors_RdYlBu <- list(values=c(0.0, 0.2, 0.5, 0.75, 0.9, 0.95, 0.98, 1.0),
                        labels=c("0.00-0.20", "0.20-0.50", "0.50-0.75",
                                 "0.75-0.90", "0.90-0.95", "0.95-0.98",
                                 "0.98-1.00"),
                        colors=c("#4575B4", "#91BFDB", "#E0F3F8", "#FFFFBF",
                                 "#FEE090", "#FC8D59", "#D73027"))

assign("z_colors_RdYlBu", z_colors_RdYlBu, env=.options)

# Zonation tutorial data paths ----------------------------------------------

get_options <- function() {
        options <- list()

        # Tutorial directories
        options[["tutorial.dir"]] <-  system.file("extdata", "test_project",
                                                  package = "zonator")
        options[["setup.dir"]] <- file.path(options$tutorial.dir, "zsetup")
        options[["data.dir"]] <- file.path(options$tutorial.dir, "data")
        options[["output.dir"]] <- file.path(options$setup.dir, "output")

        options[["groups.file"]] <- file.path(options$setup.dir, "groups.txt")
        options[["condition.file"]] <- file.path(options$setup.dir, "cond_list.txt")

        # Tutorial run and configuration files
        options[["bat.file"]] <- file.path(options$setup.dir, "01.bat")
        options[["dat.file"]] <- file.path(options$setup.dir, "01/01.dat")
        options[["spp.file"]] <- file.path(options$setup.dir, "01/01.spp")
        options[["results.dir"]] <- file.path(options$setup.dir, "01/01_out")

        # Another file for variant without groups
        options[["bat.file.no.grps"]] <- file.path(options$setup.dir, "03.bat")
        options[["dat.file.no.grps"]] <- file.path(options$setup.dir, "03/03.dat")
        options[["spp.file.no.grps"]] <- file.path(options$setup.dir, "03/03.spp")

        # Another file for a connectivity variant
        options[["bat.file.ds"]] <- file.path(options$setup.dir, "04.bat")
        options[["dat.file.ds"]] <- file.path(options$setup.dir, "04/04.dat")
        options[["spp.file.ds"]] <- file.path(options$setup.dir, "04/04.spp")

        # Another file for variant without results
        options[["bat.file.no.results"]] <- file.path(options$setup.dir,
                                                      "06.batx")
        options[["dat.file.no.results"]] <- file.path(options$setup.dir,
                                                      "06/06.dat")
        options[["spp.file.no.results"]] <- file.path(options$setup.dir,
                                                      "06/06.spp")

        # Tutorial results files, use 02_additive_benefit_function variant so that
        # weights are used
        options[["results.bat.file"]] <- file.path(options$setup.dir, "02.bat")
        options[["results.spp.file"]] <- file.path(options$setup.dir, "02/02.spp")
        options[["results.curves"]] <- file.path(options$setup.dir,
                                           "02/02_out/02.curves.txt")
        options[["results.grp.curves"]] <- file.path(options$setup.dir,
                                           "02/02_out/02.grp_curves.txt")

        # Variant with condition layers
        options[["bat.file.cond"]] <- file.path(options$setup.dir, "07.bat")
        options[["dat.file.cond"]] <- file.path(options$setup.dir, "07/07.dat")
        options[["spp.file.cond"]] <- file.path(options$setup.dir, "07/07.spp")

        return(options)
}
