# @param ... passed to pick_woods(). Expression are case-insensitive

abundance_byyr2 <- function(vft, ...) {
  crucial <- c("plotname", "tag")
  low_nms  <- check_crucial_names(set_names(vft, tolower), crucial)
  prep <- prepare_byyr2(low_nms, ...)
  
  out <- prep %>% 
    group_by(.data$plotname, .data$year, .data$family, .data$species) %>%
    count_distinct_treeid() %>%
    ungroup() %>%
    select(-.data$plotname) %>%
    select(.data$species, .data$family, dplyr::everything()) %>%
    tidyr::spread(.data$year, n, fill = 0) %>%
    arrange(.data$species, .data$family)
  
  rename_matches(out, vft)
}

basal_area_byyr2 <- function(vft, ...) {
  low_nms <- set_names(vft, tolower)
  prep <- prepare_byyr2(low_nms, ...)
  
  out <- prep %>% 
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area(dbh = .data$dbh) %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0)
  
  rename_matches(out, vft)
}

prepare_byyr2 <- function(vft, ...) {
  lowercase_dbh <- function(x) {
    x <- gsub("dbh", "dbh", rlang::expr_deparse(x), ignore.case = TRUE)
    rlang::parse_expr(x)
  }
  dots <- lapply(exprs(...), lowercase_dbh)
  
  vft %>%
    check_prepare_byyr() %>%
    
    fgeo.tool::pick_largest_hom_dbh() %>%
    fgeo.tool::pick_woods(!!! dots) %>% 
    
    drop_if_missing_dates() %>%
    mean_years() %>%
    fgeo.base::drop_if_na("year") %>%
    ungroup()
}
