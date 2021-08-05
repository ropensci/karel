## First submission to CRAN

This is a new release to CRAN, and my first re-submission, having fixed these two things mentioned by CRAN's team:

* Thanks, we see Size of tarball: 6031029 bytes. A CRAN package should not be larger than 5 MB. Please reduce the size.

I have replaced the images used in the vignettes which were the cause of the large size.

*License components with restrictions and base license permitting such: GPL-2 | file LICENSE. We do not need "| file LICENSE" and the file as these are part of R. This is only needed in case of attribution requirements or other
possible restrictions. Hence please omit it.

I ommited "| file LICENSE" in DESCRIPTION and the file itself.

## Test environments
* local ubuntu 20.04, R 4.1.0
* win-builder (devel, release and oldrelease)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs, other than the NEW SUBMISSION note.

## Downstream dependencies
There are currently no downstream dependencies for this package
