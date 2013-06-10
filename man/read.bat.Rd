\name{read.bat}
\alias{read.bat}
\title{read.bat
Read Zonation-specific (MS Windows) batch file.}
\usage{
  read.bat(infile)
}
\arguments{
  \item{infile}{character string input file path}
}
\value{
  a list of parsed bat-parameters
}
\description{
  Batch files include calls to Zonation core and look like
  following: call zig3.exe -r [INPUT_PATH].dat
  [INPUT_PATH].spp [OUTPUT_PATH].txt 0.0 0 1.0 0
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}

