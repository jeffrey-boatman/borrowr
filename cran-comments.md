## Test environments
* local OS X install, R 3.6.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey A. Boatman <boat0036@umn.edu>'
  
  This is the first submission for this package (It is a resubmission of a first submission). Please note that I originally submitted this package under the name combr, but I have decided to name it borrowr since this is a better description of the package. I apologize for any inconvenience this may cause.
  
## Replies to CRAN on submission:

* Please do not start the description with "This package", package name, title or similar.

  The description does not start with the package name or "This package".

* The Description field is intended to be a (one paragraph) description of what the    package does and why it may be useful. Please elaborate.

  I have elaborated on what the package does.

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning() if you really have to write text to the console.

  I have fixed the 'summary' function so that it no longer uses print/cat. The only functions that use print/cat are now the print functions. I still use 'message' in one instance to update the user on a function that may be slow to run if certain arguments are used. 

* Please add references describing the methods in your package also to the description field of your DESCRIPTION file in the form authors (year) <doi:...>, authors (year) <arXiv:...>, authors (year, ISBN:...), with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

  Done.

