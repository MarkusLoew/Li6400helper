[![Build Status](https://travis-ci.org/MarkusLoew/Li6400helper.svg?branch=master)](https://travis-ci.org/MarkusLoew/Li6400helper)

Li6400helper
============

An R package to help with the import of Li6400 files and to help with recalculation of gas exchange parameters when the leaf area is updated after the measurement.

For documentation see the online help:

	help(package = "Li6400helper")

Installation:

	devtools::install_github("MarkusLoew/Li6400helper")

This is an updated version due to changes changes between LPL version 3 and LPL version 6 (yes, I didn't update it in a while...).


## Li6400Import

Imports a csv or tsv text file from a Licor 6400. As I was not happy that the date is not included by default in each sample, this function uses the date from the header and updates the HHMMSS vector to YYYY-MM-DD HH:MM:SS. Makes it easier to keep track of regular, or diurnal measurements.
the file is returned as data frame.


## Li6400Recalc

If the leaf does not fill the full area of the IRGA cuvette, some of the gas exchange paramters need to be recalculated once the actual leaf area is known. This function returns either a data frame with the changed parameters only, or returns the data frame with the updated parameters in place, in context of the full file.
The calculations are taken form the compute list of a Li6400 IRGA. The source code shows the compute list entry and the corresponding R code. Be aware that rounding errors occur! And as usual, no guarantee for error-free recalculation.

