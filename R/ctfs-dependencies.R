#' Internal. Adapted from its original version.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
neighbor_densities <- function(.data,
                               .subset,
                               r, 
                               plotdim,
                               mindbh = 0,
                               type,
                               include = unique(.data$status)) {
  ptm <- proc.time()
  if (is.null(.subset)) {
    .subset <- .data
  }
  n <- nrow(.subset)
  output <- matrix(NA, ncol = 2, nrow = n)
  dimnames(output)[[2]] <-  c("conspecific", "heterospecific")
  if (type == "count") {
    cond_1 <- !is.na(.data$gx) & !is.na(.data$gy) &
      !duplicated(.data$tag) & .data$dbh >= mindbh &
      .data$status %in% include
    spd <- .data[cond_1, , drop = FALSE]
    spd <- rm_na_row(spd)
    for (i in 1:n) {
      focal <- .subset[i, ]
      if (is.na(focal$gx) | is.na(focal$gy) | duplicated(focal$tag)) {
        output[i, 1:2] <- NA
      } else {
        # FIXME: DON'T USE WITH
        poly <- with(focal, splancs::spoints(c(
          gx - r,
          gy - r, gx - r, gy + r, gx + r, gy + r, gx +
            r, gy - r
        )))
        use <- splancs::inpip(splancs::spoints(rbind(
          spd$gx,
          spd$gy
        )), poly)
        if (length(use) == 0) {
          output[i, 1:2] <- 0
        } else {
          incircle <- sqrt(splancs::dsquare(splancs::spoints(rbind(
            spd$gx[use],
            spd$gy[use]
          )), splancs::spoints(rbind(
            focal$gx,
            focal$gy
          )))) <= r
          nn <- use[incircle]
          consp <- spd$sp[nn] == focal$sp
          area <- CalcRingArea(data.frame(
            gx = focal$gx,
            gy = focal$gy
          ), r, plotdim)$each
          output[i, 1] <- length(nn[consp]) * (pi * r^2 / area)
          output[i, 2] <- length(nn[!consp]) * (pi * r^2 / area)
        }
      }
      if (i %in% seq(5000, n + 5000, 5000)) {
        message(space_dots(
          i, "of", n, " elapsed time = ", (proc.time() - ptm)[3], "seconds", "\n"
        ))
      }
    }
  }
  if (type == "basal") {
    cond_2 <- !is.na(.data$gx) & !is.na(.data$gy) &
      !is.na(.data$dbh) & .data$dbh >= mindbh & .data$status %in%
      include
    spd <- .data[cond_2, , drop = FALSE]
    spd <- rm_na_row(spd)
    for (i in 1:n) {
      focal <- .subset[i, ]
      if (is.na(focal$gx) | is.na(focal$gy)) {
        output[i, 1:2] <- NA
      } else {
        poly <- with(focal, splancs::spoints(c(
          gx - r,
          gy - r, gx - r, gy + r, gx + r, gy + r, gx +
            r, gy - r
        )))
        use <- splancs::inpip(splancs::spoints(rbind(
          spd$gx,
          spd$gy
        )), poly)
        if (length(use) == 0) {
          output[i, 1:2] <- 0
        } else {
          incircle <- sqrt(splancs::dsquare(splancs::spoints(rbind(
            spd$gx[use],
            spd$gy[use]
          )), splancs::spoints(rbind(
            focal$gx,
            focal$gy
          )))) <= r
          nn <- use[incircle]
          area <- CalcRingArea(data.frame(
            gx = focal$gx,
            gy = focal$gy
          ), r, plotdim)$each
          BA <- circlearea(spd$dbh[nn] / 20)
          consp <- spd$sp[nn] == focal$sp
          output[i, 1] <- sum(BA[consp], na.rm = TRUE) *
            (pi * r^2 / area)
          output[i, 2] <- sum(BA[!consp], na.rm = TRUE) *
            (pi * r^2 / area)
        }
      }
      if (i %in% seq(5000, n + 5000, 5000)) {
        message(space_dots(
          i, "of", n, " elapsed time = ", (proc.time() - ptm)[3], "seconds", "\n"
        ))
      }
    }
  }
  message(space_dots(
    "Total elapsed time = ", (proc.time() - ptm)[3], "seconds", "\n"
  ))
  
  tibble::as.tibble(output)
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
CalcRingArea <- function(data, radius, plotdim) {
    nopts <- dim(data)[1]
    internalArea <- numeric()
    for (i in 1:nopts) {
      xdist <- data$gx[i]
      if (plotdim[1] - data$gx[i] < xdist) {
        xdist <- plotdim[1] - data$gx[i]
      }
      internalArea[i] <- partialcirclearea(
        radius, xdist, data$gy[i],
        plotdim[2] - data$gy[i]
      )
    }
    return(list(total = sum(internalArea), each = internalArea))
  }

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
partialcirclearea <- function(r, c2, cy1, cy2) {
    if (cy1 <= cy2) {
      c1 <- cy1
      c3 <- cy2
    }
    else {
      c1 <- cy2
      c3 <- cy1
    }
    if (r > c1) {
      alpha1 <- acos(c1 / r)
      y1 <- sqrt(r * r - c1 * c1)
    }
    if (r > c2) {
      alpha2 <- acos(c2 / r)
      y2 <- sqrt(r * r - c2 * c2)
    }
    if (r > c3) {
      alpha3 <- acos(c3 / r)
      y3 <- sqrt(r * r - c3 * c3)
    }
    cornerdist1 <- sqrt(c1 * c1 + c2 * c2)
    cornerdist3 <- sqrt(c2 * c2 + c3 * c3)
    if (r <= c1 && r <= c2) {
      area <- circlearea(r)
    } else if (r > c1 && r <= c2 && r <= c3) {
      area <- r * r * (pi - alpha1) + c1 * y1
    } else if (r <= c1 && r > c2 && r <= c3) {
      area <- r * r * (pi - alpha2) + c2 * y2
    } else if (r > c1 && r > c2 && r <= c3) {
      if (r > cornerdist1) {
        area <- r * r * (3 * pi / 4 - alpha1 / 2 - alpha2 / 2) +
          c1 * c2 + (c1 * y1 + c2 * y2) / 2
      } else {
        area <- r * r * (pi - alpha1 - alpha2) + c1 * y1 +
          c2 * y2
      }
    }
    else if (r <= c2 && r > c1 && r > c3) {
      area <- r * r * (pi - alpha1 - alpha3) + c1 * y1 + c3 *
        y3
    } else if (r > c2 && r > c1 && r > c3) {
      if (r > cornerdist3) {
        area <- r * r * (pi - alpha1 - alpha3) / 2 + c1 * c2 +
          c2 * c3 + (c1 * y1 + c3 * y3) / 2
      } else if (r > cornerdist1 && r <= cornerdist3) {
        area <- r * r * (3 * pi / 4 - alpha1 / 2 - alpha2 / 2 -
          alpha3) + (c1 * y1 + c2 * y2) / 2 + c3 * y3 + c1 *
          c2
      } else if (r <= cornerdist1 && r <= cornerdist3) {
        area <- r * r * (pi - alpha1 - alpha2 - alpha3) +
          c1 * y1 + c2 * y2 + c3 * y3
      }
    }
    return(area)
  }

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
rm_na_row <- function(.data) {
    stopifnot(length(.data) > 0)
    stopifnot(any(is.matrix(.data) || is.data.frame(.data)))
    .data[!is_na_row(.data), ]
  }



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
circlearea <- function(r) {
    return(pi * r^2)
  }

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
is_na_row <- function(.data) {
    stopifnot(length(.data) > 0)
    stopifnot(any(is.matrix(.data) || is.data.frame(.data)))
    is_na_vector <- function(x) all(is.na(x))
    apply(.data, 1, is_na_vector)
  }

