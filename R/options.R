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
                     axis.title.x=element_text(size=24),
                     axis.title.y=element_text(size=24),
                     axis.text.x=element_text(size=20),
                     axis.text.y=element_text(size=20),
                     axis.ticks = element_line(size = 2),
                     legend.text=element_text(size=20),
                     legend.title=element_text(size=20),
                    panel.border = element_rect(size=2, colour="black"))
assign("curve.theme", curve.theme, env=.options)

assign("curve.x.title", "\nProp. of landscape lost", env=.options)
assign("curve.x.title.invert", "\nProportion of landscape under conservation",
       env=.options)
assign("curve.y.title", "Prop. of ditributions remaining\n", env=.options)
assign("curve.legend.title", "Features", env=.options)
assign("grp.curve.legend.title", "Feature groups", env=.options)

# Zonation tutorial -------------------------------------------------------

assign("tutorial.dir", NULL, env=.options)