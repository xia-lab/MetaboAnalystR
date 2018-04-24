## Test environments

* Ubuntu 14.04, R 3.4.4
* Mac OS High Sierra 10.13.4 R 3.4.4

## R CMD check results

0 ERRORS | 0 WARNINGS | 4 NOTES

NOTE 1) * checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘pls’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
  
  - Author's response: if @import pls is added, there will be a warning about caret::R2 being written over by pls::R2
  
