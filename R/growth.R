#' Annual growth rates accross entire census or within groups.
#' 
#' @inherit demography description
#' 
#' @details
#' `growth()` also requires the columns `status` and `stemID` in order to
#' determine which stems should have dbh change calculated.
#'
#' @inheritParams recruitment
#' @param rounddown If TRUE, all dbh < 55 are rounded down to the nearest
#'   multiple of 5.
#' @param method Use 'I' to calculate annual dbh increment: (dbh2 - dbh1)/time,
#'   or 'E' to calculate the relative growth rate (log(dbh2) - log(dbh1))/time.
#' @param stdev Logical. Default (FALSE) returns confidence limits, otherwise
#'   returns the SD in growth rate per group.
#' @param dbhunit 'cm' or 'mm'.
#' @param growthcol defines how growth is measured, either 'dbh'or
#'   'agb'(agb=biomass)
#' @param err.limit A number. Numbers such as 10000 are high and will return all
#'   measures.
#' @param maxgrow A number. Numbers such as 10000 are high and will return all
#'   measures.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @family functions to compare two censuses
#'
#' @return
#' `growth()` retruns a list with components:
#' * `rate`, the mean annualized growth rate per category selected, either dbh
#'   increment, or relative growth
#' * `N`, the number of individuals included in the mean (not counting any
#'   excluded)
#' * `clim` (or sd with `stdev = TRUE`), width of confidence interval; add this
#'   number to the mean rate to get upper confidence limit, substract to get
#'   lower
#' * `dbhmean`, mean dbh in census 1 of individuals included
#' * `time`, mean time interval in years
#' * `date1`, mean date included individuals were measured in census 1, as
#'   julian object (R displays as date, but treats as integer)
#' * `date2`, mean date in census 2.
#' 
#' @export
#'
#' @examples
#' # Filtering only 200 trees to make the data small
#' few_trees <- sample(unique(bci12s6mini$tag), 200)
#' census1 <- subset(bci12s6mini, tag %in% few_trees)
#' census2 <- subset(bci12s7mini, tag %in% few_trees)
#' 
#' # Across entire dataset
#' growth(census1, census2)
#' 
#' # Within groups defined by one "splitting" variable
#' # Note warning if split variable has grups which dbh is full of NA
#' growth_metrics <- growth(census1, census2, split1 = census1$sp)
#' # View just a few species with `head()`
#' lapply(growth_metrics, head)
#' 
#' # Within groups defined by two "splitting" variables
#' # Also warnings here (one warning per problematic splitting variable)
#' split_by_two <- growth(census1, census2, 
#'   split1 = census1$sp, split2 = census1$quadrat)
#' lapply(split_by_two, function(x) head(x[1:6]))
#' str(split_by_two)
#' 
#' # Use other arguments carefully; read `?growth()`
#' 
#' # Calculate not annual growth rate but relative growth rate
#' growth(census1, census2, method = "E")
#' # Return `sd` instead of `clim`
#' growth(census1, census2, stdev = TRUE)
#' # Include all living trees
#' growth(census1, census2, mindbh = NULL)
#' # Measure growth not based on `dbh` but on `agb`
#' growth(census1, census2, growthcol = "agb")
growth <- function(census1,
                   census2,
                   rounddown = FALSE,
                   method = "I",
                   stdev = FALSE,
                   dbhunit = "mm",
                   mindbh = 10,
                   growthcol = "dbh",
                   err.limit = 4,
                   maxgrow = 75,
                   split1 = NULL,
                   split2 = NULL) {
  check_crucial_names(census1, c("dbh", "pom", "status", "date", "stemID"))
  check_if_all_dbh_is_na(census1, census2, split = split1)
  check_if_all_dbh_is_na(census1, census2, split = split2)

  # Legacy code ---
    size1 = census1[, growthcol]
    size2 = census2[, growthcol]
    if (is.null(split1)) 
        split1 = rep("all", dim(census1)[1])
    if (is.null(split2)) 
        split2 = rep("all", dim(census2)[1])
    if (is.null(census2$codes)) 
        census2$codes = rep(".", length(size2))
    time = (census2$date - census1$date)/365.25
    if (rounddown) {
        sm = ((size1 < 55 | size2 < 55) & !is.na(size1) & !is.na(size2))
        size1[sm] = rndown5(size1[sm])
        size2[sm] = rndown5(size2[sm])
    }
    if (method == "I") 
        growthrate = (size2 - size1)/time
    else if (method == "E") 
        growthrate = (log(size2) - log(size1))/time
    good = trim.growth(census1, census2, time, err.limit = err.limit, 
        maxgrow = maxgrow, mindbh = mindbh)
    growthrate[!good] = NA
    class1 = sort(unique(split1))
    class2 = sort(unique(split2))
    splitgood = list(split1[good], split2[good])
    mean.grow = tapply(growthrate[good], splitgood, mean, na.rm = TRUE)
    sd.grow = tapply(growthrate[good], splitgood, sd, na.rm = TRUE)
    N = tapply(growthrate[good], splitgood, length)
    meandbh = tapply(census1$dbh[good], splitgood, mean, na.rm = TRUE)
    meansize = tapply(size1[good], splitgood, mean, na.rm = TRUE)
    interval = tapply(time[good], splitgood, mean, na.rm = TRUE)
    startdate = tapply(census1$date[good], splitgood, mean, na.rm = TRUE)
    enddate = tapply(census2$date[good], splitgood, mean, na.rm = TRUE)
    mean.grow = fill.dimension(mean.grow, class1, class2, fill = NA)
    sd.grow = fill.dimension(sd.grow, class1, class2, fill = NA)
    N = fill.dimension(N, class1, class2, fill = 0)
    meandbh = fill.dimension(meandbh, class1, class2, fill = NA)
    interval = fill.dimension(interval, class1, class2, fill = NA)
    startdate = fill.dimension(startdate, class1, class2, fill = NA)
    enddate = fill.dimension(enddate, class1, class2, fill = NA)
    ci.grow = sd.grow
    ci.grow[N == 0] = NA
    ci.grow[N > 0] = sd.grow[N > 0] * qt(0.975, N[N > 0])/sqrt(N[N > 
        0])
    if (!stdev) 
        result = list(rate = drp(mean.grow), N = drp(N), clim = drp(ci.grow), 
            dbhmean = drp(meandbh), time = drp(interval), date1 = drp(startdate), 
            date2 = drp(enddate))
    else result = list(rate = drp(mean.grow), N = drp(N), sd = drp(sd.grow), 
        dbhmean = drp(meandbh), time = drp(interval), date1 = drp(startdate), 
        date2 = drp(enddate))
    return(result)
}

#' @rdname growth
#' @export
growth_df <- function(census1, 
                      census2,
                      rounddown = FALSE,
                      method = "I",
                      stdev = FALSE,
                      dbhunit = "mm",
                      mindbh = 10,
                      growthcol = "dbh",
                      err.limit = 4,
                      maxgrow = 75,
                      split1 = NULL) {
  result <- growth(
    census1 = census1,
    census2 = census2,
    rounddown = rounddown,
    method = method,
    stdev = stdev,
    dbhunit = dbhunit,
    mindbh = mindbh,
    growthcol = growthcol,
    err.limit = err.limit,
    maxgrow = maxgrow,
    split1 = split1
    # split 2 is intentionally removed
  )
  demography_df(result)
}



# Internal dependencies of recruitment ------------------------------------

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
binGraphManySpecies <- 
function (fulldataname, darken = 8, xrange = c(-3, 3), yrange = c(0, 
    0.1), xtitle = NULL, ytitle = NULL, export = pdf, outfile = "growth/linearbin.multi.pdf", 
    h = 8, w = 10) 
{
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    fulldata = get(fulldataname)
    result = compare.growthbinmodel(fit = fulldata, export = NULL, 
        makegraph = FALSE)
    sumtable = assembleBinOutput(inputtable = result, fulldata = fulldata, 
        sitename = "bci")
    species = names(fulldata)
    nospp = length(species)
    if (darken < nospp) 
        select = sample.int(nospp, darken)
    else select = 1:nospp
    if (is.null(ytitle)) 
        ytitle = "AGB growth"
    if (is.null(xtitle)) 
        xtitle = "DBH"
    for (i in 1:length(species)) {
        onesp = species[i]
        spdata = fulldata[[onesp]]
        bin = sumtable[onesp, ]$MaxBin
        onebindata = spdata[[bin]]
        if (i == 1) 
            graph.growthmodel.spp(fit = onebindata, xrange = xrange, 
                yrange = yrange, xtitle = xtitle, ytitle = ytitle, 
                add = FALSE, addpts = FALSE, modelclr = "gray", 
                modellwd = 0.5, withSD = NULL, regclr = NULL)
        if (i %in% select) 
            graph.growthmodel.spp(fit = onebindata, add = TRUE, 
                addpts = FALSE, modelclr = "black", modellwd = 1.5, 
                withSD = NULL, regclr = NULL)
        else graph.growthmodel.spp(fit = onebindata, add = TRUE, 
            addpts = FALSE, modelclr = "gray", modellwd = 0.5, 
            withSD = NULL, regclr = NULL)
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
binGraphManySpecies.Panel <- 
function (fulldataname, sitename, darken = 8, xrange = c(-3, 
    3), yrange = c(0, 0.1), export = pdf, outfile = "growth/linearbin.multi.pdf", 
    h = 8, w = 10) 
{
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    par(mfcol = c(2, 2), mai = c(0.75, 0.75, 0.1, 0.1))
    for (i in 1:length(fulldataname)) {
        xtitle = ytitle = ""
        if (i == 1 | i == 2) 
            ytitle = "AGB growth"
        if (i == 2 | i == 4) 
            xtitle = "DBH"
        binGraphManySpecies(fulldataname = fulldataname[i], darken = darken, 
            xrange = xrange, yrange = yrange, xtitle = xtitle, 
            ytitle = ytitle, export = NULL)
        text(-2.5, 0.8 * max(yrange), sitename[i], cex = 1.7, 
            pos = 4)
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
binGraphSampleSpecies <- 
function (fulldataname, species, whichbin, export = pdf, outfile = "growth/linearbin.summary.pdf", 
    h = 8, w = 10) 
{
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    if (length(fulldataname) == 1) 
        fulldataname = rep(fulldataname, length(species))
    if (length(whichbin) == 1) 
        whichbin = rep(whichbin, length(species))
    par(mfcol = c(2, 2), mai = c(0.75, 0.75, 0.1, 0.1))
    for (i in 1:length(species)) {
        xtitle = ytitle = ""
        if (i == 1 | i == 2) 
            ytitle = "AGB growth"
        if (i == 2 | i == 4) 
            xtitle = "DBH"
        onesp = species[i]
        spdata = get(fulldataname[i])[[onesp]]
        onebindata = spdata[[whichbin[i]]]
        graph.growthmodel.spp(fit = onebindata, maintitle = onesp, 
            xtitle = xtitle, ytitle = ytitle)
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
compare.growthbinmodel <- 
function (fit, bins = 1:4, makegraph = TRUE, conflines = 0, newgraph = TRUE, 
    export = pdf, outfile = "growth/linearbin.bestfit.pdf", h = 8, 
    w = 10) 
{
    spp = names(fit)
    bestbin = numeric()
    BIC = DIC = AICopt = AICgibbs = meanllike = optllike = matrix(ncol = length(bins), 
        nrow = length(spp))
    colnames(AICopt) = colnames(AICgibbs) = colnames(BIC) = colnames(DIC) = colnames(meanllike) = colnames(optllike) = pst("bin", 
        bins)
    rownames(AICopt) = rownames(AICgibbs) = rownames(BIC) = rownames(DIC) = rownames(meanllike) = rownames(optllike) = spp
    for (i in 1:length(spp)) {
        fit.onesp = fit[[spp[i]]]
        for (j in 1:length(fit.onesp)) {
            BIC[i, j] = calculateBinModel.BIC(fit.onesp[[j]])
            DIC[i, j] = calculateBinModel.DIC(fit.onesp[[j]])
            AICopt[i, j] = calculateBinModel.AIC(fit.onesp[[j]], 
                type = "optim")
            AICgibbs[i, j] = calculateBinModel.AIC(fit.onesp[[j]], 
                type = "mean")
            meanllike[i, j] = mean(fit.onesp[[j]]$llike[fit$keep])
            optllike[i, j] = fit.onesp[[j]]$optimllike
        }
        bestbin[i] = which.max(AICgibbs[i, ])
    }
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    slope = upper = lower = matrix(ncol = 4, nrow = length(spp))
    rownames(slope) = rownames(upper) = rownames(lower) = spp
    for (i in 1:length(spp)) {
        fit.onesp = fit[[spp[i]]]
        best = bins[bestbin[i]]
        if (best == 1) 
            plural = "bin"
        else plural = "bins"
        if (newgraph & makegraph) 
            x11(height = 5, width = 9)
        if (makegraph) 
            graph.growthmodel.spp(fit = fit.onesp[[best]], whichpred = "pred", 
                graphdiv = 15, modelclr = "blue", maintitle = paste(spp[i], 
                  best, plural), conf = conflines)
        if (best == 1) 
            slopecol = 2
        else if (best == 2) 
            slopecol = 2:3
        else if (best == 3) 
            slopecol = 3:5
        else if (best == 4) 
            slopecol = 4:7
        slope[i, 1:length(slopecol)] = fit.onesp[[bestbin[i]]]$best[slopecol]
        upper[i, 1:length(slopecol)] = fit.onesp[[bestbin[i]]]$CI[2, 
            slopecol]
        lower[i, 1:length(slopecol)] = fit.onesp[[bestbin[i]]]$CI[1, 
            slopecol]
    }
    return(list(slopes = slope, upper = upper, lower = lower))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
dgammadexp <- 
function (z, mean, sd, lambda, draws = 10000, div = 0.1, xrange = c(-10, 
    30), graphit = F) 
{
    sumpart = dgammaPlusdexp(z, mean, sd, lambda)
    diffpart = dgammaMinusdexp(z, mean, sd, lambda)
    result = 0.5 * sumpart + 0.5 * diffpart
    if (graphit) {
        r = mean/(sd^2)
        a = mean * r
        growth = rgamma(draws, shape = a, rate = r)
        error = rsymexp(draws, center = 0, rate = lambda)
        obs = growth + error
        minx = min(obs) - div
        maxx = max(obs) + div
        x = seq(minx, maxx, by = div)
        hist(obs, breaks = x, xlim = xrange)
        lines(z, draws * div * result)
    }
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
extract.growthdata <- 
function (census1, census2, growcol = "incgr", mingrow = 0.1, 
    logit = "x", growthfunc = growth.biomass.indiv, pomcut = 10000, 
    rounddown = FALSE, mindbh = 10, dbhunit = "mm", err.limit = 4, 
    maxgrow = 75, exclude.stem.change = TRUE, returnfull = FALSE) 
{
    growthtable = growthfunc(census1, census2, rounddown = rounddown, 
        mindbh = mindbh, dbhunit = dbhunit, err.limit = err.limit, 
        maxgrow = maxgrow, pomcut = pomcut, exclude.stem.change = exclude.stem.change)
    growthrate = growthtable[, growcol]
    dbh = growthtable$dbh1
    treeID = growthtable$treeID
    agb = growthtable$agb1
    if (logit == "x" | logit == "xy") {
        dbh = log(growthtable$dbh1)
        agb = log(growthtable$agb1)
    }
    if (logit == "y" | logit == "xy") {
        growthrate[growthrate <= 0] = mingrow
        growthrate = log(growthrate)
    }
    result = data.frame(sp = I(growthtable$sp), treeID, dbh = dbh, 
        agb = agb, growth = growthrate)
    cond_1 <- !is.na(result$growth) & !is.na(result$dbh) & !is.na(result$agb) & 
        !is.na(result$sp)
    result <- result[cond_1, , drop = FALSE]
    if (!returnfull) 
        return(result)
    cond_2 <- !is.na(growthtable$growthrate) & !is.na(growthtable$dbh1) & 
        !is.na(growthtable$agb1) & !is.na(growthtable$sp)
    vars <- c("sp", "treeID", "dbh1", "dbh2", "agb1", "agb2", 
        "time", "incgr")
    full <- growthtable[cond_2, vars, drop = FALSE]
    colnames(full)[which(colnames(full) == "incgr")] = "growth"
    if (returnfull) 
        return(full)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
graph.growthmodel <- 
function (spp, fitlist, whichbin = 1, regclr = "green", modelclr = "blue", 
    graphdiv = 10, export = pdf, outfile = "growth/linearbin.fit.pdf", 
    h = 8, w = 10) 
{
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    for (onesp in spp) {
        if (is.null(export)) 
            x11(height = 5, width = 9)
        graph.growthmodel.spp(fit = fitlist[[onesp]][[whichbin]], 
            graphdiv = 20, add = FALSE, modelclr = "blue", maintitle = onesp)
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
graph.outliers <- 
function (full, trimmed, fit = NULL, allspp = NULL, size = "agb", 
    export = NULL, wind = X11) 
{
    if (is.null(allspp)) 
        allspp = names(fit)
    for (onesp in allspp) {
        if (is.null(export)) 
            wind(height = 7, width = 9)
        graph.outliers.spp(full = full, trimmed = trimmed, spname = onesp, 
            fit = fit, size = size, export = export)
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
graph.outliers.spp <- 
function (full, trimmed, spname = "gustsu", fit = NULL, size = "agb", 
    export = NULL, xtitle = "log(agb)", ytitle = "growth") 
{
    full <- full[full$sp == spname, , drop = FALSE]
    trimmed <- trimmed[trimmed$sp == spname, , drop = FALSE]
    missing = !(full$treeID %in% trimmed$treeID)
    outliers = full[missing, ]
    plot(full[, size], full$growth, pch = 16, cex = 0.5, xlab = xtitle, 
        ylab = ytitle)
    points(outliers[, size], outliers$growth, col = "red")
    points(trimmed[, size], trimmed$growth, col = "blue")
    if (!is.null(fit)) 
        overlay.growthbinmodel(fit = fit[spname], add = TRUE, 
            newgraph = FALSE, export = export)
    return(outliers)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
growth.biomass.indiv <- 
function (census1, census2, mindbh = 10, steminfo = FALSE, dbhunit = "mm", 
    err.limit = 4, maxgrow = 75, rounddown = NULL, exclude.stem.change = TRUE, 
    pomcut = 10000) 
{
    time = (census2$date - census1$date)/365.25
    incgrowth = (census2$agb - census1$agb)/time
    expgrowth = (log(census2$agb) - log(census1$agb))/time
    good = trim.growth(census1, census2, time, err.limit = err.limit, 
        maxgrow = maxgrow, mindbh = mindbh, dbhunit = dbhunit, 
        exclude.stem.change = exclude.stem.change)
    pomdiff = (as.numeric(census2$pom) - as.numeric(census1$pom))/as.numeric(census1$pom)
    include = which(census1$dbh >= mindbh & census1$status == 
        "A" & census2$status == "A" & pomdiff < pomcut)
    incgrowth[-include] = expgrowth[-include] = NA
    incgrowth[!good] = NA
    if (steminfo) 
        growthdata = data.frame(treeID = census1$treeID, tag = I(census1$tag), 
            stemID = census1$stemID, stemtag = I(census1$StemTag), 
            sp = I(census1$sp), gx = census1$gx, gy = census1$gy, 
            dbh1 = census1$dbh, dbh2 = census2$dbh, agb1 = census1$agb, 
            agb2 = census2$agb, time = time, incgr = incgrowth, 
            expgr = expgrowth, status1 = I(census1$status), status2 = I(census2$status), 
            codes1 = I(census1$codes), codes2 = I(census2$codes))
    else growthdata = data.frame(treeID = census1$treeID, tag = I(census1$tag), 
        sp = I(census1$sp), gx = census1$gx, gy = census1$gy, 
        dbh1 = census1$dbh, dbh2 = census2$dbh, agb1 = census1$agb, 
        agb2 = census2$agb, time = time, incgr = incgrowth, expgr = expgrowth, 
        status1 = I(census1$status), status2 = I(census2$status), 
        codes1 = I(census1$codes), codes2 = I(census2$codes))
    return(growthdata)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
growth.dbh <- 
function (census1, census2, classbreak = c(10, 100, 300), dbhunit = "mm", 
    growthcol = "dbh", err.limit = 4, maxgrow = 75, rounddown = FALSE, 
    method = "I", stdev = FALSE, mindbh = 10) 
{
    allbreak = c(classbreak, 10000)
    dbhclass = as.numeric(as.character(cut(census1$dbh, breaks = allbreak, 
        right = F, labels = classbreak)))
    result = growth(census1, census2, mindbh = mindbh, dbhunit = dbhunit, 
        rounddown = rounddown, method = method, growthcol = growthcol, 
        err.limit = err.limit, maxgrow = maxgrow, stdev = stdev, 
        split2 = dbhclass)
    ord = order(result$dbhmean)
    for (i in 1:length(result)) result[[i]] = result[[i]][ord]
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
growth.eachspp <- 
function (census1, census2, classbreak = c(10, 100, 300), dbhunit = "mm", 
    mindbh = 10, growthcol = "dbh", err.limit = 4, maxgrow = 75, 
    rounddown = FALSE, method = "I", stdev = FALSE) 
{
    allbreak = c(classbreak, 10000)
    dbhclass = as.numeric(as.character(cut(census1$dbh, breaks = allbreak, 
        right = F, labels = classbreak)))
    sp = census1$sp
    result = growth(census1, census2, rounddown = rounddown, 
        dbhunit = dbhunit, mindbh = mindbh, method = method, 
        growthcol = growthcol, err.limit = err.limit, maxgrow = maxgrow, 
        stdev = stdev, split1 = sp, split2 = dbhclass)
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
growth.flexbin <- 
function (growthtable, sizecol = "dbh", nobin = 2, start = NULL, 
    startsd = NULL, sdmodel = linear.model.ctr, badsdfunc = NULL, 
    method = "Gibbs", rep = 1200, show = 100, burn = 200, ...) 
{
    size = growthtable[, sizecol]
    growthtable$growth
    if (is.null(start)) 
        start = c(defineBinBreaks(size, nobin, ...), rep(0.1, 
            nobin), 0)
    if (is.null(startsd)) 
        startsd = c(0.02, 0)
    bad = bad.binparam(param = start, x = size, pred = NULL, 
        ...)
    if (bad) 
        start = c(defineBinBreaks(size, nobin, ...), rep(0.1, 
            nobin), 0)
    bad = FALSE
    if (!is.null(badsdfunc)) 
        bad = badsdfunc(size, param = startsd)
    if (bad) 
        startsd = defineSDpar(size, length(startsd))
    fit = optim(par = c(start, startsd), fn = llike.linearbin.optim, 
        control = list(fnscale = (-1)), x = size, y = growthtable$growth, 
        predfunc = linearmodel.bin, nomainpar = length(start), 
        llikefunc = llike.GaussModel, sdfunc = sdmodel, badpredpar = bad.binparam, 
        badsdpar = badsdfunc, ...)
    fit$best = best.optim = fit$par[1:length(start)]
    fit$bestSD = bestSD.optim = fit$par[-(1:length(start))]
    bestllike.optim = fit$value
    bestpred = linearmodel.bin(size, param = best.optim, ...)
    fit$model = data.frame(x = size, y = growthtable$growth, 
        pred = bestpred)
    fit$model = fit$model[order(fit$model$x), ]
    if (method == "Gibbs") 
        fit = model.xy(x = size, y = growthtable$growth, predfunc = linearmodel.bin, 
            llikefunc = llike.GaussModel, badpredpar = bad.binparam, 
            start.predpar = best.optim, sdfunc = sdmodel, start.sdpar = bestSD.optim, 
            llikefuncSD = llike.GaussModelSD, badsdpar = badsdfunc, 
            steps = rep, burnin = burn, showstep = show, ...)
    fit$llike.best = llike.linearbin.optim(param = c(fit$best, 
        fit$bestSD), x = size, y = growthtable$growth, predfunc = linearmodel.bin, 
        nomainpar = length(start), llikefunc = llike.GaussModel, 
        sdfunc = sdmodel, badpredpar = bad.binparam, badsdpar = badsdfunc, 
        ...)
    fit$growth = growthtable
    fit$bins = nobin
    fit$optimpar = best.optim
    fit$optimSD = bestSD.optim
    fit$optimllike = bestllike.optim
    optimal = which.max(fit$llike[fit$keep])
    fit$max.param = fit$fullparam[fit$keep, ][optimal, ]
    return(fit)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
growth.indiv <- 
function (census1, census2, rounddown = FALSE, mindbh = 10, dbhunit = "mm", 
    err.limit = 4, maxgrow = 75) 
{
    if (is.null(census2$codes)) 
        census2$codes = rep(".", length(census2$dbh))
    time = (census2$date - census1$date)/365.25
    if (rounddown) {
        sm = ((census1$dbh < 55 | census2$dbh < 55) & !is.na(census1$dbh) & 
            !is.na(census2$dbh))
        census1$dbh[sm] = rndown5(census1$dbh[sm])
        census2$dbh[sm] = rndown5(census2$dbh[sm])
    }
    incgrowth = (census2$dbh - census1$dbh)/time
    expgrowth = (log(census2$dbh) - log(census1$dbh))/time
    good = trim.growth(census1, census2, time, err.limit = err.limit, 
        maxgrow = maxgrow, mindbh = mindbh, dbhunit = dbhunit)
    good[census1$dbh < mindbh] = FALSE
    incgrowth[!good] = expgrowth[!good] = NA
    growthdata = data.frame(treeID = census1$treeID, tag = I(census1$tag), 
        sp = I(census1$sp), gx = census1$gx, gy = census1$gy, 
        dbh1 = census1$dbh, dbh2 = census2$dbh, time = time, 
        incgr = incgrowth, expgr = expgrowth, status1 = census1$status, 
        status2 = census2$status)
    return(growthdata)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
individual_grow.table <- 
function (cnsdata, powertransformation = 0.45, rnd = c(FALSE, 
    TRUE, rep(FALSE, 4)), mingrow = 0.1, mindbh = 10, maxdbh = 10000, 
    maxerrorSD = 4, maxerrorGrow = 75, center = 1992, debug = FALSE) 
{
    for (i in 1:(length(cnsdata) - 1)) {
        cns = i:(i + 1)
        gtbl = growth.indiv(cnsdata[[i]], cnsdata[[i + 1]], rounddown = rnd[i], 
            err.limit = maxerrorSD, maxgrow = maxerrorGrow)
        gtbl$time = (cnsdata[[i + 1]]$date + cnsdata[[i]]$date)/(2 * 
            365.25)
        gtbl$census = i
        gtbl$censusfact = as.factor(i)
        cond_1 <- !is.na(gtbl$incgr) & gtbl$dbh1 < maxdbh & gtbl$dbh1 >= 
            mindbh
        section <- gtbl[cond_1, , drop = FALSE]
        if (i == 1) 
            final = section
        else final = rbind(final, section)
    }
    final$growth = final$incgr
    final$growth[final$incgr <= 0] = mingrow
    final$LnGrowth = log(final$growth)
    final$LnSize = log(final$dbh1) - mean(log(final$dbh1))
    final$CRGrowth = pospower(final$incgr, powertransformation)
    if (debug) 
        browser()
    colnames(final)[which(colnames(final) == "sp")] = "species"
    vars <- c("treeID", "tag", "gx", "gy", "species", "dbh1", 
        "dbh2", "LnSize", "incgr", "LnGrowth", "CRGrowth", "time", 
        "census", "censusfact")
    final <- final[final$status2 != "M", vars, drop = FALSE]
    if (!is.null(center)) 
        final$time = final$time + 1960 - center
    return(final)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
overlay.growthbinmodel <- 
function (fit, bins = 1:4, regclr = "green", modelclr = "blue", 
    graphdiv = 15, add = FALSE, newgraph = TRUE, export = pdf, 
    outfile = "growth/linearbin.overlay.pdf", h = 8, w = 10) 
{
    if (!is.null(export)) {
        on.exit(graphics.off())
        export(file = outfile, height = h, width = w)
    }
    allspp = names(fit)
    clrlist = c("blue", "red", "gray", "black", "orange")
    for (onesp in allspp) {
        if (is.null(export) & newgraph) 
            x11(height = 5, width = 9)
        for (b in bins) {
            if (b == bins[[1]]) 
                graph.growthmodel.spp(fit = fit[[onesp]][[b]], 
                  whichpred = "pred", graphdiv = graphdiv, add = add, 
                  modelclr = "blue", maintitle = onesp)
            else graph.growthmodel.spp(fit = fit[[onesp]][[b]], 
                whichpred = "pred", regclr = NULL, graphdiv = graphdiv, 
                add = TRUE, addpts = FALSE, modelclr = clrlist[b], 
                maintitle = NULL)
        }
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
run.growthbin.manyspp <- 
function (growthdata, size = "dbh", spp = spp20, minabund300 = 15, 
    minTotal = 40, dbhunit = "mm", sdmodel = linear.model.ctr, 
    startpar = c(0.03, 0.005), startsdpar = c(0.04, 0), badsdfunc = NULL, 
    binoption = 1:4, noreps = 5000, noburn = 2500, noshow = 500, 
    outputname = "linear.fit", path = "", ...) 
{
    result = list()
    on.exit(cat(onesp, "\n"))
    outputfile = pst(path, outputname, ".rdata")
    for (onesp in spp) {
        spdata = subset(growthdata, sp == onesp)
        total = dim(spdata)[1]
        if (dbhunit == "mm") 
            nobig = dim(subset(spdata, dbh >= log(300)))[1]
        else if (dbhunit == "cm") 
            nobig = dim(subset(spdata, dbh >= log(30)))[1]
        if (total >= minTotal & nobig >= minabund300) {
            result[[onesp]] = run.growthfit.bin(growthdata = spdata, 
                size = size, binoption = binoption, startpar = startpar, 
                sdmodel = sdmodel, startsdpar = startsdpar, badsdfunc = badsdfunc, 
                noreps = noreps, noburn = noburn, noshow = noshow, 
                ...)
            for (j in 1:length(result[[onesp]])) result[[onesp]][[j]]$summary = list(dbhunit = dbhunit, 
                totalsample = total, totalbig = nobig)
            cat("Finished species ", onesp, " with ", nobig, 
                " big trees\n")
        }
        assign(outputname, result)
        save(list = outputname, file = outputfile)
    }
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
run.growthfit.bin <- 
function (growthdata, size = "dbh", startpar = c(0.03, 0.005), 
    startsdpar = c(0.04, 0), sdmodel = linear.model.ctr, badsdfunc = NULL, 
    binoption = 1:4, noreps = 5000, noburn = 2500, noshow = 500, 
    ...) 
{
    result = vector("list", length(binoption))
    nextModelpar = startpar
    nextSDpar = startsdpar
    for (i in 1:length(binoption)) {
        result[[i]] = growth.flexbin(growthtable = growthdata, 
            sizecol = size, nobin = binoption[i], start = nextModelpar, 
            startsd = nextSDpar, sdmodel = sdmodel, badsdfunc = badsdfunc, 
            rep = noreps, burn = noburn, show = noshow, ...)
        nextModelpar = addBinParam(x = growthdata[, size], result[[i]]$max.param, 
            bin = binoption[i])
        nextSDpar = result[[i]]$bestSD
        cat("Finished bin ", binoption[i], "\n")
    }
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
trim.growth <- 
function (cens1, cens2, time, slope = 0.006214, intercept = 0.9036, 
    err.limit = 4, maxgrow = 75, pomcut = 0.05, mindbh = 10, 
    dbhunit = "mm", exclude.stem.change = TRUE) 
{
    if (dbhunit == "cm") 
        intercept = intercept/10
    stdev.dbh1 = slope * cens1$dbh + intercept
    growth = (cens2$dbh - cens1$dbh)/time
    bad.neggrow = which(cens2$dbh <= (cens1$dbh - err.limit * 
        stdev.dbh1))
    bad.posgrow = which(growth > maxgrow)
    pomdiff = abs(as.numeric(cens2$pom) - as.numeric(cens1$pom))/as.numeric(cens1$pom)
    accept = rep(TRUE, length(cens1$dbh))
    accept[pomdiff > pomcut] = FALSE
    accept[bad.neggrow] = FALSE
    accept[bad.posgrow] = FALSE
    accept[is.na(growth)] = FALSE
    if (exclude.stem.change) 
        accept[cens1$stemID != cens2$stemID] = FALSE
    accept[cens1$dbh < mindbh] = FALSE
    accept[is.na(cens1$dbh) | is.na(cens2$dbh) | cens2$dbh <= 
        0] = FALSE
    return(accept)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
bad.binparam <- 
function (x, param, ...) 
{
    x = as.matrix(x)
    noparam = length(param)
    bins = (noparam)/2
    if (bins == 1) 
        return(FALSE)
    internal = param[1:(bins - 1)]
    if (length(which(internal <= min(x))) > 0) 
        return(TRUE)
    if (length(which(internal >= max(x))) > 0) 
        return(TRUE)
    breaks = c(min(x), internal, max(x))
    if (!enoughSamplePerBin(x = x, b = breaks, ...)) 
        return(TRUE)
    if (!wideEnoughBins(x = x, b = breaks, ...)) 
        return(TRUE)
    return(FALSE)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
biomass.growth <- 
function (census1, census2, rounddown = FALSE, stdev = FALSE, 
    mindbh = 10, split1 = NULL, split2 = NULL) 
{
    size1 = census1[, "agb"]
    size2 = census2[, "agb"]
    if (is.null(split1)) 
        split1 = rep("all", dim(census1)[1])
    if (is.null(split2)) 
        split2 = rep("all", dim(census2)[1])
    time = (census2$date - census1$date)/365.25
    if (rounddown) {
        sm = ((size1 < 55 | size2 < 55) & !is.na(size1) & !is.na(size2))
        size1[sm] = rndown5(size1[sm])
        size2[sm] = rndown5(size2[sm])
    }
    growthrate = (size2 - size1)/time
    class1 = sort(unique(split1))
    class2 = sort(unique(split2))
    good = census1$status == "A" & census2$status == "A" & census1$dbh >= 
        mindbh
    splitgood = list(split1[good], split2[good])
    netgain = tapply(growthrate[good], splitgood, sum, na.rm = TRUE)
    N = tapply(growthrate[good], splitgood, length)
    meanagb = tapply(size1[good], splitgood, mean, na.rm = TRUE)
    meandbh = tapply(census1$dbh[good], splitgood, mean, na.rm = TRUE)
    interval = tapply(time[good], splitgood, mean, na.rm = TRUE)
    startdate = tapply(census1$date[good], splitgood, mean, na.rm = TRUE)
    enddate = tapply(census2$date[good], splitgood, mean, na.rm = TRUE)
    netgain = fill.dimension(netgain, class1, class2, fill = NA)
    N = fill.dimension(N, class1, class2, fill = 0)
    meanagb = fill.dimension(meanagb, class1, class2, fill = NA)
    meandbh = fill.dimension(meandbh, class1, class2, fill = NA)
    interval = fill.dimension(interval, class1, class2, fill = NA)
    startdate = fill.dimension(startdate, class1, class2, fill = NA)
    enddate = fill.dimension(enddate, class1, class2, fill = NA)
    result = list(net = netgain, N = N, agbmean = meanagb, dbhmean = meandbh, 
        time = interval, date1 = startdate, date2 = enddate)
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
defineBinBreaks <- 
function (size, nobin, ...) 
{
    extra = list(...)
    if (is.null(extra$MINBINWIDTH)) 
        MINBINWIDTH = 0.1
    else MINBINWIDTH = extra$MINBINWIDTH
    if (is.null(extra$MINBINSAMPLE)) 
        MINBINSAMPLE = 5
    else MINBINSAMPLE = extra$MINBINSAMPLE
    if (nobin == 1) 
        return(numeric(0))
    quant = (1:(nobin - 1))/nobin
    internal = quantile(size, prob = quant)
    breaks = c(min(size), internal, max(size))
    if (wideEnoughBins(x = size, b = breaks, ...)) 
        return(internal)
    breaks = seq(min(size), max(size), length = nobin + 1)
    internal = breaks[2:nobin]
    if (enoughSamplePerBin(x = size, b = breaks, ...)) 
        return(internal)
    internal = breaks = numeric()
    breaks[1] = min(size)
    minwidth = MINBINWIDTH * diff(range(size))
    size = sort(size)
    for (i in 1:(nobin - 1)) {
        whichnext = MINBINSAMPLE * i + 1
        if (whichnext <= length(size)) 
            enough = size[whichnext]
        else enough = max(size)
        wide = breaks[i] + minwidth
        internal[i] = breaks[i + 1] = IfElse(enough > wide, enough, 
            wide)
    }
    return(internal)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
graph.growthmodel.spp <- 
function (fit, jiggle = 0.001, whichpred = "pred", xrange = NULL, 
    yrange = NULL, xtitle = NULL, ytitle = NULL, includeaxs = TRUE, 
    withSD = "red", regclr = "green", modelclr = "blue", modellwd = 1, 
    graphdiv = 10, add = FALSE, addpts = TRUE, maintitle = NULL, 
    conf = 0) 
{
    size = fit$model$x
    obs = fit$model$y
    if (whichpred == "meanpred") 
        pred = fit$model$meanpred
    else pred = fit$model$pred
    if (is.null(yrange)) 
        yrange = range(c(obs, pred))
    if (is.null(xrange)) 
        xrange = range(size)
    if (is.null(xtitle)) 
        xtitle = "log size"
    if (is.null(ytitle)) 
        ytitle = "growth"
    xjiggle = rnorm(length(size), mean = 0, sd = jiggle)
    yjiggle = rnorm(length(size), mean = 0, sd = jiggle)
    if (addpts) {
        if (!add) 
            plot(size + xjiggle, obs + yjiggle, xlim = xrange, 
                ylim = yrange, pch = 16, cex = 0.8, xlab = xtitle, 
                ylab = ytitle, axes = includeaxs)
        else points(size + xjiggle, obs + yjiggle, pch = 16, 
            cex = 0.8)
    }
    else {
        if (add) 
            lines(size, pred, col = modelclr, lwd = modellwd)
        else plot(size, pred, type = "l", xlim = xrange, ylim = yrange, 
            xlab = xtitle, ylab = ytitle, col = modelclr, lwd = modellwd, 
            axes = includeaxs)
    }
    b = seq(min(size), max(size) + 1e-04, len = graphdiv - 1)
    sizecat = cut(size, breaks = b, right = FALSE)
    if (conf > 0) {
        allparam = fit$fullparam[fit$keep, ]
        noparam = dim(allparam)[1]
        r = sample(1:noparam, conf)
        for (i in 1:conf) {
            onepred = linearmodel.bin(fit$model$x, param = allparam[r[i], 
                ])
            lines(fit$model$x, onepred, col = "gray10", lwd = 0.5)
            if (i == 1) 
                meanpred = onepred
            else meanpred = meanpred + onepred
        }
        lines(fit$model$x, meanpred/conf, lty = "dashed", lwd = 1.5, 
            col = "purple")
    }
    meansize = tapply(size, sizecat, mean)
    meangrowth = tapply(obs, sizecat, mean)
    if (!is.null(regclr)) 
        lines(meansize, meangrowth, col = regclr, lwd = modellwd)
    if (addpts) 
        lines(size, pred, col = modelclr, lwd = modellwd)
    if (!is.null(withSD)) {
        lines(size, pred + fit$model$predSD, col = withSD)
        lines(size, pred - fit$model$predSD, col = withSD)
    }
    ypos = max(obs) - 0.3 * diff(range(obs))
    xpos = min(size) + 0.1 * diff(range(size))
    if (!is.null(maintitle)) 
        text(xpos, ypos, maintitle, cex = 1.7, pos = 4)
    return(data.frame(meansize, meangrowth))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
linearmodel.bin <- 
function (x, param, ...) 
{
    x = as.matrix(x)
    extra = list(...)
    if (is.null(extra$LINEARBINMEDIAN)) 
        medv = 0
    else medv = extra$LINEARBINMEDIAN
    noparam = length(param)
    bins = (noparam)/2
    if (is.null(medv)) 
        medv = median(x)
    if (bins == 1) 
        return(linear.model(x - medv, param))
    b = param[1:(bins - 1)] - medv
    v = x - medv
    N = length(b)
    m = param[bins:(noparam - 1)]
    inter = param[noparam]
    pred = linearmodel.bin.set(v = v, binparam = b, param = c(m, 
        inter))
    return(pred)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
linearmodel.bin.set <- 
function (v, param, binparam) 
{
    m = param[-length(param)]
    inter = param[length(param)]
    failed = rep(NA, length(v))
    if ((length(binparam) + 1) != length(m)) 
        return(failed)
    sorted = sort(binparam)
    if (length(which(sorted != binparam)) > 0) 
        return(failed)
    K = IfElse(diff(range(v)) > diff(range(binparam)), diff(range(v)), 
        diff(range(binparam)))
    if (K == 0) 
        K = 1
    lower = IfElse(min(v) < min(binparam), min(v) - K, min(binparam) - 
        K)
    upper = IfElse(max(v) > max(binparam), max(v) + K, max(binparam) + 
        K)
    b = c(lower, binparam, upper)
    bins = length(m)
    N = length(b)
    pts = data.frame(x = b, y = rep(NA, N))
    if (upper < 0) {
        pts$y[N] = inter + m[bins] * upper
        for (i in (N - 1):1) pts$y[i] = pts$y[i + 1] - m[i] * 
            (b[i + 1] - b[i])
    }
    else if (lower > 0) {
        pts$y[1] = inter + m[1] * lower
        for (i in 2:N) pts$y[i] = pts$y[i - 1] + m[i - 1] * (b[i] - 
            b[i - 1])
    }
    else if (upper > 0) {
        z = which(b > 0)[1]
        pts$y[z] = inter + m[z - 1] * (b[z])
        for (i in (z - 1):1) pts$y[i] = pts$y[i + 1] - m[i] * 
            (b[i + 1] - b[i])
        if (z < N) 
            for (i in (z + 1):N) pts$y[i] = pts$y[i - 1] + m[i - 
                1] * (b[i] - b[i - 1])
    }
    else if (upper == 0) {
        z = which(b == 0)
        pts$y[z] = inter + m[z - 1] * (b[z])
        for (i in (z - 1):1) pts$y[i] = pts$y[i + 1] - m[i] * 
            (b[i + 1] - b[i])
        if (z < N) 
            for (i in (z + 1):N) pts$y[i] = pts$y[i - 1] + m[i - 
                1] * (b[i] - b[i - 1])
    }
    pred = rep(NA, length(v))
    for (i in 1:bins) {
        start = b[i]
        end = b[i + 1]
        insection = v >= start & v <= end
        thisline = pts.to.interceptslope(pts[i, ], pts[i + 1, 
            ])
        pred[insection] = linear.model(v[insection], param = thisline)
    }
    return(pred)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
llike.GaussModel <- 
function (testparam, allparam, whichtest, x, obs, model, badpred, 
    SD, ...) 
{
    param = arrangeParam.llike(testparam, allparam, whichtest)
    pred = model(x = x, param = param, ...)
    if (badpred(x, param, pred, ...)) 
        return(-Inf)
    llike = dnorm(obs, mean = pred, sd = SD, log = TRUE)
    total = sum(llike)
    if (is.na(total) | is.infinite(total) | is.null(total)) {
        cat("Something wrong with llike calculation from observed and predicted; check right now SD, obs, x, and pred\n")
        browser()
    }
    return(total)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
llike.GaussModelSD <- 
function (testparam, allparam, whichtest, x, pred, obs, model, 
    badsd, ...) 
{
    extra = list(...)
    if (is.null(extra$MINIMUM_SD)) 
        MINIMUM_SD = 0
    else MINIMUM_SD = extra$MINIMUM_SD
    param = arrangeParam.llike(testparam, allparam, whichtest)
    sd = model(x = x, param = param)
    if (length(which(sd <= MINIMUM_SD)) > 0) 
        return(-Inf)
    if (!is.null(badsd)) 
        if (badsd(x, param)) 
            return(-Inf)
    llike = dnorm(obs, mean = pred, sd = sd, log = TRUE)
    total = sum(llike)
    if (is.na(total) | is.infinite(total) | is.null(total)) 
        browser()
    return(total)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
metrop1step <- 
function (func, start.param, scale.param, adjust, target, ...) 
{
    origlike = func(start.param, ...)
    newval = rnorm(1, mean = start.param, sd = scale.param)
    newlike = func(newval, ...)
    if (is.na(newlike)) 
        browser()
    AdjExp = (1 - target)/target
    if (origlike == (-Inf)) {
        likeratio = IfElse(newlike == (-Inf), 0, 1)
        browser
    }
    else likeratio = exp(newlike - origlike)
    if (is.na(likeratio)) 
        browser()
    if (runif(1) < likeratio) {
        newscale = scale.param * adjust^AdjExp
        return(c(newval, newscale, 1, newlike, origlike, newval))
    }
    else {
        newscale = scale.param * (1/adjust)
        return(c(start.param, newscale, 0, origlike, newlike, 
            newval))
    }
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
model.xy <- 
function (x, y, predfunc, llikefunc, badpredpar, badsdpar = NULL, 
    start.predpar, sdfunc = NULL, start.sdpar = NULL, llikefuncSD, 
    logx = FALSE, logy = FALSE, add = c(0, 0), steps = 100, showstep = 20, 
    burnin = 50, ...) 
{
    x = x + add[1]
    y = y + add[2]
    runx = x
    runy = y
    if (logx) 
        runx = log(x[x > 0 & y > 0])
    if (logy) 
        runy = log(y[x > 0 & y > 0])
    nopredpar = length(start.predpar)
    predpar = matrix(nrow = steps, ncol = nopredpar)
    predparscale = 0.5 * start.predpar
    predparscale[predparscale < 0] = (-predparscale[predparscale < 
        0])
    predparscale[predparscale < 0.1] = 0.1
    predpar[1, ] = start.predpar
    if (!is.null(start.sdpar)) {
        nosdpar = length(start.sdpar)
        sdpar = matrix(nrow = steps, ncol = nosdpar)
        sdparscale = 0.5 * start.sdpar
        sdparscale[sdparscale <= 0] = 0.1
        sdpar[1, ] = start.sdpar
    }
    extra = list(...)
    if (is.null(extra$MINIMUM_SD)) 
        MINIMUM_SD = 0
    else MINIMUM_SD = extra$MINIMUM_SD
    llike = numeric()
    i = 1
    if (!is.null(sdfunc)) 
        SD = sdfunc(x = runx, param = sdpar[i, ])
    llike[i] = llikefunc(testparam = predpar[i, 1], allparam = predpar[i, 
        ], whichtest = 1, x = runx, obs = runy, model = predfunc, 
        badpred = badpredpar, SD = SD, ...)
    cat(i, ": ", round(predpar[i, ], 4), round(sdpar[i, ], 4), 
        round(llike[i], 4), "\n")
    for (i in 2:steps) {
        if (!is.null(sdfunc)) 
            SD = sdfunc(x = runx, param = sdpar[i - 1, ])
        if (length(which(SD <= MINIMUM_SD)) > 0) 
            browser()
        for (j in 1:nopredpar) {
            param = arrangeParam.Gibbs(i, j, predpar)
            metropresult = metrop1step(func = llikefunc, start.param = param[j], 
                scale.param = predparscale[j], adjust = 1.02, 
                target = 0.25, allparam = param, whichtest = j, 
                x = runx, obs = runy, model = predfunc, badpred = badpredpar, 
                SD = SD, ...)
            predpar[i, j] = metropresult[1]
            predparscale[j] = metropresult[2]
        }
        pred = predfunc(x = x, param = predpar[i, ])
        if (!is.null(sdfunc)) 
            for (j in 1:nosdpar) {
                param = arrangeParam.Gibbs(i, j, sdpar)
                metropresult = metrop1step(func = llikefuncSD, 
                  start.param = param[j], scale.param = sdparscale[j], 
                  adjust = 1.02, target = 0.25, allparam = param, 
                  whichtest = j, x = runx, pred = pred, obs = runy, 
                  model = sdfunc, badsd = badsdpar, ...)
                sdpar[i, j] = metropresult[1]
                sdparscale[j] = metropresult[2]
            }
        if (!is.null(sdfunc)) 
            SD = sdfunc(x = runx, param = sdpar[i, ])
        if (length(which(SD <= MINIMUM_SD)) > 0) 
            browser()
        llike[i] = llikefunc(testparam = predpar[i, 1], allparam = predpar[i, 
            ], whichtest = 1, x = runx, obs = runy, model = predfunc, 
            badpred = badpredpar, SD = SD, ...)
        if (i%%showstep == 0 | i == 2) 
            cat(i, ": ", round(predpar[i, ], 3), round(sdpar[i, 
                ], 4), round(llike[i], 2), "\n")
    }
    keep = (-(1:burnin))
    best = colMedians(predpar[keep, ])
    bestmean = colMeans(predpar[keep, ])
    conf = apply(predpar[keep, ], 2, CI)
    if (!is.null(sdfunc)) {
        bestSD = IfElse(nosdpar == 1, median(sdpar[keep]), colMedians(sdpar[keep, 
            ]))
        bestSDmean = IfElse(nosdpar == 1, mean(sdpar[keep]), 
            colMeans(sdpar[keep, ]))
        confSD = IfElse(nosdpar == 1, CI(sdpar[keep]), apply(sdpar[keep, 
            ], 2, CI))
    }
    else bestSD = confSD = sdpar = NULL
    pred = predfunc(x = x, param = best, ...)
    predSD = sdfunc(x = x, param = bestSD)
    modelresult = data.frame(x = runx, y = runy, pred, predSD)
    modelresult = modelresult[order(modelresult$x), ]
    return(list(best = best, bestmean = bestmean, CI = conf, 
        bestSD = bestSD, bestSDmean = bestSDmean, confSD = confSD, 
        model = modelresult, fullparam = predpar, sdpar = sdpar, 
        llike = llike, burn = burnin, keep = keep))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
pts.to.interceptslope <- 
function (pt1, pt2) 
{
    if (is.null(dim(pt1))) {
        x1 = pt1[1]
        y1 = pt1[2]
    }
    else {
        x1 = pt1[, 1]
        y1 = pt1[, 2]
    }
    if (is.null(dim(pt2))) {
        x2 = pt2[1]
        y2 = pt2[2]
    }
    else {
        x2 = pt2[, 1]
        y2 = pt2[, 2]
    }
    len = IfElse(length(x2) > length(x1), length(x2), length(x1))
    exact = (x1 == x2)
    slope = (y2 - y1)/(x2 - x1)
    inter = y1 - slope * x1
    slope[exact] = Inf
    inter[exact] = x1[exact]
    result = data.frame(b = inter, m = slope)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
enoughSamplePerBin <- 
function (x, b, ...) 
{
    extra = list(...)
    if (is.null(extra$MINBINSAMPLE)) 
        minsample = 5
    else minsample = extra$MINBINSAMPLE
    dbhcat = cut(x, b, right = FALSE)
    Npercat = table(dbhcat)
    if (length(which(Npercat < minsample)) > 0) 
        return(FALSE)
    else return(TRUE)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
wideEnoughBins <- 
function (x, b, ...) 
{
    extra = list(...)
    if (is.null(extra$MINBINWIDTH)) 
        minwidth = 0.1
    else minwidth = extra$MINBINWIDTH
    delta = diff(b)
    if (length(which(delta < minwidth * diff(range(x)))) > 0) 
        return(FALSE)
    else return(TRUE)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
fill.dimension <- 
function (dataarray, class1, class2, fill = 0) 
{
    result = data.frame(matrix(fill, nrow = length(class1), ncol = length(class2)))
    rownames(result) = class1
    colnames(result) = class2
    result[rownames(dataarray), colnames(dataarray)] = dataarray
    result[is.na(result)] = fill
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
rndown5 <- 
function (s) 
return(5 * floor(s/5))



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
calculateBinModel.AIC <- 
function (fit, type = "optim") 
{
    noparam = length(fit$best) + length(fit$bestSD)
    if (type == "optim") 
        return(fit$optimllike - 2 * noparam)
    if (type == "mean") 
        return(mean(fit$llike[fit$keep]) - 2 * noparam)
    if (type == "max") 
        return(max(fit$llike[fit$keep]) - 2 * noparam)
    if (type == "none") 
        return(max(fit$llike[fit$keep]))
    return(NA)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
calculateBinModel.BIC <- 
function (fit) 
{
    N = dim(fit$model)[1]
    noparam = length(fit$best) + length(fit$bestSD)
    error.var = (1/N) * sum((fit$model$pred - fit$model$y)^2)
    return(N * log(error.var) + noparam * log(N))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
CI <- 
function (x, prob = c(0.025, 0.975), na.rm = FALSE) 
return(quantile(x, prob = prob, na.rm = na.rm))



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
pst <- 
function (...) 
{
    s = list(...)
    len = length(s)
    if (len == 0) 
        return(NA)
    result = s[[1]]
    if (len == 1) 
        return(s[[1]])
    for (i in 2:len) result = paste(result, s[[i]], sep = "")
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
IfElse <- 
function (test, a, b) 
{
    if (test) 
        return(a)
    else return(b)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
logit <- 
function (p) 
{
    result = p
    inc = (p > 0 & p < 1 & !is.na(p))
    inc[is.na(p)] = FALSE
    result[inc] = log(p[inc]/(1 - p[inc]))
    result[!inc] = NA
    return(result)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
drp <- 
function (x) 
{
    return(drop(as.matrix(x)))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
defineSDpar <- 
function (x, nopar) 
{
    if (nopar == 2) 
        return(c(0.02, 0))
    midpoint = 0.5 * (min(x) + max(x))
    return(c(0, midpoint, 0.001))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
linear.model.ctr <- 
function (x, param, xcenter = NULL) 
{
    if (is.null(xcenter)) 
        x = x - median(x)
    return(param[2] * x + param[1])
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
llike.linearbin.optim <- 
function (param, x, y, predfunc, nomainpar, badpredpar, llikefunc, 
    sdfunc, badsdpar, ...) 
{
    extra = list(...)
    if (is.null(extra$MINIMUM_SD)) 
        MINIMUM_SD = 0
    else MINIMUM_SD = extra$MINIMUM_SD
    predpar = param[1:nomainpar]
    binparam = predpar[1:(nomainpar/2 - 1)]
    sdpar = param[-(1:nomainpar)]
    SD = sdfunc(x, sdpar)
    if (length(which(SD <= MINIMUM_SD)) > 0) 
        return(-Inf)
    if (!is.null(badsdpar)) 
        if (badsdpar(x, sdpar)) 
            return(-Inf)
    llike = llikefunc(testparam = predpar[1], allparam = predpar, 
        whichtest = 1, x = x, obs = y, model = predfunc, badpred = badpredpar, 
        SD = SD)
    total = sum(llike)
    if (is.na(total)) 
        browser()
    return(total)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
linear.model <- 
function (x, param) 
{
    x = as.matrix(x)
    nopredictor = dim(x)[2]
    a = param[1]
    b = param[2:(nopredictor + 1)]
    return(a + x %*% b)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
arrangeParam.llike <- 
function (testparam, allparam, whichtest) 
{
    param = allparam
    if (whichtest > 0 & !is.na(whichtest) & !is.null(whichtest)) 
        param[whichtest] = testparam
    return(param)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
arrangeParam.Gibbs <- 
function (i, j, allparam) 
{
    if (is.null(dim(allparam))) 
        return(allparam[i - 1])
    param = allparam[i - 1, ]
    if (j > 1) 
        param[1:(j - 1)] = allparam[i, 1:(j - 1)]
    return(param)
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
colMedians <- 
function (mat, na.rm = TRUE) 
return(apply(mat, 2, median, na.rm = na.rm))



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
addBinParam <- 
function (x, best, bin) 
{
    if (bin == 1) 
        return(c(median(x), best[2], best[2], best[1]))
    internal = best[1:(bin - 1)]
    slope = best[bin:(length(best) - 1)]
    intercept = best[length(best)]
    div = c(min(x), internal, max(x))
    widest = which.max(diff(div))
    newbreak = 0.5 * diff(div[c(widest:(widest + 1))]) + div[widest]
    newinternal = sort(c(internal, newbreak))
    if (widest < bin) 
        newslope = slope[c(1:widest, widest, (widest + 1):bin)]
    else newslope = slope[c(1:widest, widest)]
    return(c(newinternal, newslope, intercept))
}



