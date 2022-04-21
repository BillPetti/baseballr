Release summary
This is an initial release for baseballr, corrected for CRAN feedback.

* Corrects notes on DESCRIPTION and URL spacing
* Adds notes on missing return documentation on exported functions
* Method of adjusting user options for function `retrosheet_data()` changed to use on.exit() and any downloaded files are only stored in the tempdir()


R CMD check results
0 errors | 0 warnings | 0 notes

revdepcheck results
We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

We saw 0 new problems
We failed to check 0 packages