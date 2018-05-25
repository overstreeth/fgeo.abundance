# fgeo.abundance 0.0.0.9004

* New reexports from __dplyr__: `tally()`, `count()`, `add_tally()`, `add_count()`, `group_by()`, and `ungroup()`.
* New `count()` exported from __dplyr__ replaces `abundance()`.
* `basal_area()` quotes argument `dbh` for consistency with `count()`.

# fgeo.abundance 0.0.0.9003

* New `abundance()`, `basal_area()` and friends calculate these metrics by groups and also filter data by status.

# fgeo.abundance 0.0.0.9002

* Rename as package as __fgeo.abundance__.

# forestr 0.0.0.9001

* Move demography functions out of __forestr__ into __fgeo.demography__.


# forestr 0.0.0.9000

## Known issues

* Demography: `devtools::check()` throws the note below. The function `calculateBinModel.DIC()` does not exist in the CTFS R package nor I could find it elsewhere.

```R
checking R code for possible problems ... NOTE
compare.growthbinmodel: no visible global function definition for
  'calculateBinModel.DIC'
```

* Added a `NEWS.md` file to track changes to the package.
