abundance_byyr2 <- function(vft) {
  crucial <- c("plotname", "tag")
  vft %>%
    set_names(tolower) %>%
    check_crucial_names(crucial) %>%
    prepare_byyr2() %>%
    group_by(.data$plotname, .data$year, .data$family, .data$species) %>%
    count_distinct_treeid() %>%
    ungroup() %>%
    select(-.data$plotname) %>%
    select(.data$species, .data$family, dplyr::everything()) %>%
    tidyr::spread(.data$year, n, fill = 0) %>%
    arrange(.data$species, .data$family) %>%
    rename_matches(vft)
}

basal_area_byyr2 <- function(vft, status_a = "alive", status_d = "dead") {
  vft %>%
    set_names(tolower) %>%
    prepare_byyr2() %>%
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area(dbh = .data$dbh) %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0) %>%
    rename_matches(vft)
}

prepare_byyr2 <- function(vft) {
  vft %>%
    check_prepare_byyr() %>%
    drop_if_missing_dates() %>%
    mean_years() %>%
    fgeo.base::drop_if_na("year") %>%
    ungroup()
}
