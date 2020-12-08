## Test environments
* local OS X install, R 3.6.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

1 NOTE:

New maintainer:
  Jeffrey A. Boatman <jeffrey.boatman@gmail.com>
Old maintainer(s):
  Jeffrey A. Boatman <boat0036@umn.edu>
  
This change is necessary because I am no longer employed at UMN.

Thank you for your speedy review. I have fixed both of these issues:

   Found the following (possibly) invalid URLs:
     URL: https://www.ncbi.nlm.nih.gov/pubmed/29036300 (moved to
https://pubmed.ncbi.nlm.nih.gov/29036300/)
       From: inst/doc/borrowr-package.html
       Status: 200
       Message: OK

Please change http --> https, add trailing slashes, or follow moved
content as appropriate.



In the Description field cited below you have

  <doi10.1093/biostatistics/kxx031>.

where you have to add a clolon after "doi"
