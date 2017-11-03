# Internal helper of recruitment_df, mortality_df and growth_df
# For documentation see recruitment() in recruitment.R
demography_df <- function(demography_list) {
  # Transform result from list to dataframe
  nested <- tibble::enframe(demography_list, "metric")
  unnested <- tidyr::unnest(
    dplyr::mutate(nested, value = purrr::map(.data$value, tibble::enframe))
  )
  # Reorganize columns
  df_long <- dplyr::select(unnested, .data$name, .data$metric, .data$value)
  arranged <- dplyr::arrange(df_long, .data$name)
  dplyr::rename(arranged, split = .data$name)
}
