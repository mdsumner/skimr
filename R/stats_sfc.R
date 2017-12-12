#' @include stats_sfc.R
NULL


# Summary statistic functions for sfc

#' ncoords_sf - the complete count of coordinate instances
#' npaths_sf - complete count of paths, (a ring that is a hole is a path)
#' @param x A vector
#' @return count of coordinates, all instances
#' @export
ncoords_sf <- function(x) {
  gibble::gibble(x) %>% 
    dplyr::summarize(ncoords = sum(nrow)) %>% 
    dplyr::pull(ncoords) 
}
#' @name ncoords_sf
#' @export
npaths_sf <- function(x) {
  nrow(gibble::gibble(x))
}
#' @name nsegments_sf
#' @export
nsegment_sf <- function(x) {
  nrow(silicate::sc_segment(silicate::PATH(x)))
}
#' @name nvertex_sf
#' @export
nvertex_sf <- function(x) {
  nrow(dplyr::distinct(silicate::sc_coord(x)))
}


sfc_point_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf, 
  nvertex = nvertex_sf,
  npaths = npaths_sf
)

sfc_linestring_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf, 
  nvertex = nvertex_sf,
  npaths = npaths_sf,
  nsegments = nsegment_sf
)

sfc_polygon_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf, 
  nvertex = nvertex_sf,
  npaths = npaths_sf,
  nsegments = nsegment_sf
  
)

sfc_multipoint_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf,
  nvertex = nvertex_sf,
  npaths = npaths_sf
  
)

sfc_multilinestring_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf,
  nvertex = nvertex_sf,
  npaths = npaths_sf,
  nsegments = nsegment_sf
)

sfc_multipolygon_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf, 
  nvertex = nvertex_sf,
  npaths = npaths_sf,
  nsegments = nsegment_sf
  
)

sfc_geometry_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  ncoords = ncoords_sf, 
  nvertex = nvertex_sf,
  npaths = npaths_sf,
  nsegments = nsegment_sf
)


skim_with(
  sfc_POINT = sfc_point_funs,
  sfc_LINESTRING = sfc_linestring_funs,
  sfc_POLYGON = sfc_polygon_funs,
  sfc_MULTIPOINT = sfc_multipoint_funs,
  sfc_MULTILINESTRING = sfc_multilinestring_funs,
  sfc_MULTIPOLYGON = sfc_multipolygon_funs,
  sfc_GEOMETRY = sfc_geometry_funs
)
