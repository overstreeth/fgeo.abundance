
<!-- README.md is generated from README.Rmd. Please edit that file -->
forestr
=======

[![Travis-CI Build Status](https://travis-ci.org/forestgeo/forestr.svg?branch=master)](https://travis-ci.org/forestgeo/forestr)

[![codecov](https://codecov.io/github/forestgeo/forestr/branch/master/graphs/badge.svg)](https://codecov.io/github/forestgeo/forestr)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/forestr)](https://cran.r-project.org/package=forestr)

Overview
--------

This package provides tools for the analysis of forest dynamics.

Installation
------------

If you are authorized (via [Stuart Davies](daviess@si.edu)), you can install forestr from ForestGEO's private GitHub repo.

    # From ?devtools::install_github: To install from a private repo, use auth_token
    # with a token from https://github.com/settings/tokens. You only need the repo 
    # scope. Best practice is to save your PAT in env var called GITHUB_PAT.

    # install.packages("devtools")
    devtools::install_github("forestgeo/forestr", auth_token = "abc")
    library(forestr)

Acknowledgements.
-----------------

The BCI forest dynamics research project was founded by S.P. Hubbell and R.B. Foster and is now managed by R. Condit, S. Lao, and R. Perez under the Center for Tropical Forest Science and the Smithsonian Tropical Research in Panama. Numerous organizations have provided funding, principally the U.S. National Science Foundation, and hundreds of field workers have contributed.

References
----------

Anderson-Teixeira, K. J., Davies, S. J., Bennett, A. C., Gonzalez-Akre, E. B., Muller-Landau, H. C., Joseph Wright, S., . . . Zimmerman, J. 2015. CTFS-ForestGEO: a worldwide network monitoring forests in an era of global change. Global Change Biology, 21(2), 528-549. <doi:10.1111/gcb.12712> Hubbell, S.P., Condit, R., and Foster, R.B. 2010. Barro Colorado Forest Census Plot Data. [URL](http://ctfs.si.edu/webatlas/datasets/bci).

Condit, R. 1998. Tropical Forest Census Plots. Springer-Verlag and R. G. Landes Company, Berlin, Germany, and Georgetown, Texas.

Hubbell, S.P., R.B. Foster, S.T. O'Brien, K.E. Harms, R. Condit, B. Wechsler, S.J. Wright, and S. Loo de Lao. 1999. Light gap disturbances, recruitment limitation, and tree diversity in a neotropical forest. Science 283: 554-557.
