#' Get census profiles for all census tracts in a cma
#'
#' @param cma
#' @param topic
#'
#' @return
#' @export
#'
#' @examples
get_census_tract_profiles <- function(cma = NULL, topic = 9) {

  geo <- get_2016_census_geographies(geos = "CT")
  shp <- get_boundaries(geos = "CT")
  shp <- shp %>%
    dplyr::rename(geo_id_code = ctuid) %>%
    dplyr::left_join(
      geo %>%
        dplyr::select(geo_id_code, geo_uid)
    )

  if(!is.null(cma)) {

    shp <- shp %>% dplyr::filter(cmaname == cma)
  }

  res <- shp %>%
    mutate(
      census_profile = geo_uid %>% purrr::map(~ get_2016_census_profile(.x, topic = topic))
    )

  res


}
