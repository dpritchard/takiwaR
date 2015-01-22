## Test environments
- Local OS X (Yosemite, v. 10.10.1): R 3.1.2
- Win-Builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Daniel Pritchard <daniel@pritchard.co>'
  New submission

This is expected, as this is a new submission for me.  The initial release is very simple, but we will build on this over time.  

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped

I cannot figure this one out! I have followed all the advice I could find (RE: Repositories and ~./Rprofile), but I still can't suppress this NOTE. Worse, it seems to be intermittent: It appears when running from inside RStudio, but not when running directly from the Terminal, or when checked on WinBuilder... Hmmm. Hopefully this is OK.

## Downstream dependancies
There are none, nor am I expecting there to very many (if any) in the foreseeable future!