# mvgb R package
Bruce Swihart  
Mar 2022

## Submission 3

  * removed \dontrun in examples
  * Deployed par() more respectfully in examples
  * Add \value tag to .Rd
  * Correct DESC; add doi
  * Initial submission to CRAN

## Test environments
Local OS X: R version 4.1.2 (2021-11-01)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Mojave 10.14.6
  
rhub::check(platform = "debian-gcc-devel"): Debian Linux, R-devel, GCC
rhub::check(platform = "windows-x86_64-devel"): Windows Server 2022, R-devel, 64 bit

devtools::check_win_devel()

M1: https://mac.r-project.org/macbuilder/results/1646788339-1b6e8ae9c8be48d4/

## R CMD check results
There were no ERRORs or WARNINGs. 1 NOTE due to New submission to CRAN.


## Downstream dependencies
There are currently no downstream dependencies for this package.
