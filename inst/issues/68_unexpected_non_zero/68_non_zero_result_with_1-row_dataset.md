neighbor
================

# Setup

Load CTFSRPackage and confirm that `NeighborDensities()` is
loaded.

``` r
load(here::here("inst/issues/68_unexpected_non_zero/CTFSRPackage.rdata"))
ls(pattern = "Neighbor")
#> [1] "NeighborDensities"
```

Dependencies.

``` r
library(splancs)
#> Loading required package: sp
#> 
#> Spatial Point Pattern Analysis Code in S-Plus
#>  
#>  Version 2 - Spatial and Space-Time analysis
```

# Test

Create a 1 row dataset;

``` r
censdata <- data.frame(
  stringsAsFactors = FALSE,
  gx = 5,
  gy = 5,
  tag = "a",
  sp = "sp1",
  dbh = 20,
  status = "A"
)
censdata
#>   gx gy tag  sp dbh status
#> 1  5  5   a sp1  20      A
```

Run `NeighborDensities()`, expecting result of `con.sp = 0`, `het.sp
= 0`.

``` r
NeighborDensities(censdata)
#> Total elapsed time =  0.08 seconds
#>        con.sp het.sp
#> [1,] 2.339862      0
```

# Question

Is there a bug or is my expectation wrong? Why?

# Reference

Source code.

``` r
get(ls(pattern = "Neighbor"))
#> function (censdata, censdata2 = NULL, r = 20, plotdim = c(1000, 
#>     500), mindbh = 10, type = "count", include = c("A")) 
#> {
#>     ptm <- proc.time()
#>     if (is.null(censdata2)) 
#>         censdata2 <- censdata
#>     n = nrow(censdata2)
#>     output = matrix(NA, ncol = 2, nrow = n)
#>     dimnames(output)[[2]] <- c("con.sp", "het.sp")
#>     if (type == "count") {
#>         spd = subset(censdata, !is.na(gx) & !is.na(gy) & !duplicated(tag) & 
#>             dbh >= mindbh & status %in% include)
#>         for (i in 1:n) {
#>             focal = censdata2[i, ]
#>             if (is.na(focal$gx) | is.na(focal$gy) | duplicated(focal$tag)) 
#>                 output[i, 1:2] = NA
#>             else {
#>                 poly = with(focal, spoints(c(gx - r, gy - r, 
#>                   gx - r, gy + r, gx + r, gy + r, gx + r, gy - 
#>                     r)))
#>                 use = inpip(spoints(rbind(spd$gx, spd$gy)), poly)
#>                 if (length(use) == 0) 
#>                   output[i, 1:2] = 0
#>                 else {
#>                   incircle = sqrt(dsquare(spoints(rbind(spd$gx[use], 
#>                     spd$gy[use])), spoints(rbind(focal$gx, focal$gy)))) <= 
#>                     r
#>                   nn = use[incircle]
#>                   consp <- spd$sp[nn] == focal$sp
#>                   area = CalcRingArea(data.frame(gx = focal$gx, 
#>                     gy = focal$gy), r, plotdim)$each
#>                   output[i, 1] = length(nn[consp]) * (pi * r^2/area)
#>                   output[i, 2] = length(nn[!consp]) * (pi * r^2/area)
#>                 }
#>             }
#>             if (i %in% seq(5000, n + 5000, 5000)) 
#>                 cat(i, "of", n, " elapsed time = ", (proc.time() - 
#>                   ptm)[3], "seconds", "\n")
#>         }
#>     }
#>     if (type == "basal") {
#>         spd = subset(censdata, !is.na(gx) & !is.na(gy) & !is.na(dbh) & 
#>             dbh >= mindbh & status %in% include)
#>         for (i in 1:n) {
#>             focal = censdata2[i, ]
#>             if (is.na(focal$gx) | is.na(focal$gy)) 
#>                 output[i, 1:2] = NA
#>             else {
#>                 poly = with(focal, spoints(c(gx - r, gy - r, 
#>                   gx - r, gy + r, gx + r, gy + r, gx + r, gy - 
#>                     r)))
#>                 use = inpip(spoints(rbind(spd$gx, spd$gy)), poly)
#>                 if (length(use) == 0) 
#>                   output[i, 1:2] = 0
#>                 else {
#>                   incircle = sqrt(dsquare(spoints(rbind(spd$gx[use], 
#>                     spd$gy[use])), spoints(rbind(focal$gx, focal$gy)))) <= 
#>                     r
#>                   nn = use[incircle]
#>                   area = CalcRingArea(data.frame(gx = focal$gx, 
#>                     gy = focal$gy), r, plotdim)$each
#>                   BA = circlearea(spd$dbh[nn]/20)
#>                   consp <- spd$sp[nn] == focal$sp
#>                   output[i, 1] = sum(BA[consp], na.rm = TRUE) * 
#>                     (pi * r^2/area)
#>                   output[i, 2] = sum(BA[!consp], na.rm = TRUE) * 
#>                     (pi * r^2/area)
#>                 }
#>             }
#>             if (i %in% seq(5000, n + 5000, 5000)) 
#>                 cat(i, "of", n, " elapsed time = ", (proc.time() - 
#>                   ptm)[3], "seconds", "\n")
#>         }
#>     }
#>     cat("Total elapsed time = ", (proc.time() - ptm)[3], "seconds", 
#>         "\n")
#>     return(output)
#> }
#> <bytecode: 0x000000000f66efd8>
```
