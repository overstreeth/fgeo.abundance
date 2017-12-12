# forestr 0.0.0.9000


## Known issues

* Demography: `devtools::check()` throws the note below. The function `calculateBinModel.DIC()` does not exist in the CTFS R package nor I could find it elsewhere.

```R
checking R code for possible problems ... NOTE
compare.growthbinmodel: no visible global function definition for
  'calculateBinModel.DIC'
```

* Added a `NEWS.md` file to track changes to the package.
