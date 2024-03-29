# curvematching 0.14.0

* `tidyselect` 1.2.0 grammar for the `dplyr::select()` function now forbids `.data$varname` arguments. Update various function to remove deprecation messages and errors.
* Uses the more lenient `any_of()` to prevent hard errors.


# curvematching 0.13.1

* Removes superfluous mutate_() imports
* Removes superfluous LazyData in DESCRIPTION

# curvematching 0.13.0

* Adds GHA `R-CMD-check`
* Merges the `bdsreader` branch so as to remove `minihealth`
* Relocates repo to the `growthchart` organisation

# curvematching 0.12.0

* Adds blended distance matching function `match_bmd()`, which generalises the distance metric used for matching

# curvematching 0.11.0

* Replaces `substitute()` by tidyeval `enexpr()`

# curvematching 0.10.0

* Removes the !!! expression, deprecated in `R 4.0.0`
* Updates a test after requiring `donorloader 0.12.0`

# curvematching 0.9.1

* Update to R 4.0.0

# curvematching 0.9.0

* Repairs a bug in `match_pmm()` that could produce big blob of matches

# curvematching 0.8.0

* Added a `NEWS.md` file to track changes to the package.
