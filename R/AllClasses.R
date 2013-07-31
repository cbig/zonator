# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Zonation project ------------------------------------------------------------

setClass("Zproject", representation(root = "character", variants = "list"))

# Zonation variant ------------------------------------------------------------

setClass("Zvariant", representation(name = "character", bat.file = "character",
                                    call.params = "list", results = "list"),
         validity = check.variant)


