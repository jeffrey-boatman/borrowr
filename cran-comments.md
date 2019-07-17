## Test environments
* local OS X install, R 3.6.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey A. Boatman <boat0036@umn.edu>'

## Replies to CRAN on submission:

* Please only capitalize sentence beginnings and names in the description text.

This is fixed, although I continue to capitalize acronyms and the word Bayesian since Bayes is a name. I retained capitalization for the title in the Description file since it's my understanding that this is how it's supposed to be, but I apologize if this is not correct.

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning() if you
really have to write text to the console. f.i.: cat() in wbart.R

I have commented-out the calls to cat in the wbart.R file. I have also commented out calls to printf in the source code.

* Please add \value to .Rd files and explain the functions results in the documentation.

This is done for the pate function. Because this is the only exported function, this is the only function I've done this for. I did not add \value for adapt.Rd since adapt is a data set.
