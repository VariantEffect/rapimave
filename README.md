# rapimave
An R-package for API access to MaveDB, a database for Deep Mutational Scanning data (see https://www.mavedb.org/).
This package allows programmatic access to the contents of the database, as well as basic filtering functions. 
For documentation see https://github.com/jweile/rapimave/blob/master/manual.pdf

## Requires:
 * R 3.1.2 or higher
 * hgvsParseR package (see https://github.com/jweile/hgvsParseR)
 
## Recommended:
 * devtools
 * testthat
 * roxygen2

## Installation:
1. Open command line terminal
2. If not already installed, install devtools
install.packages("devtools")
3. Load devtools
library("devtools")
4. If not already installed, install hgvsParseR
install_github("jweile/hgvsParseR")
5. Install rapimave
install_github("jweile/rapimave")

## Usage:
See https://github.com/jweile/rapimave/blob/master/manual.pdf
