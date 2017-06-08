

sfc_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)



# numeric_funs <- list(
#   missing = n_missing,
#   complete = n_complete,
#   n = length,
#   mean = purrr::partial(mean, na.rm = TRUE),
#   sd = purrr::partial(sd, na.rm = TRUE),
#   min = purrr::partial(min, na.rm = TRUE),
#   median = purrr::partial(median, na.rm = TRUE),
#   quantile = purrr::partial(quantile, probs = c(.25, .75), na.rm = TRUE),
#   max = purrr::partial(max, na.rm = TRUE),
#   hist = inline_hist
# )

## no. rook/queen neighbours

to_prim <- function(x) {
  PRIMITIVE(sf::st_sf(geometry = x, dummy = seq_along(x)))
}
n_path <- function(x) {
  x <- to_prim(x)
  nrow(x$path)
}
n_hole <- function(x) {
  x <- to_prim(x)
  if (is.null(x$path$island_)) 0 else sum(x$path$island_ > 1, na.rm = TRUE)
}
n_segment <- function(x) {
  x <- to_prim(x)
  nrow(x$segment)
}
psort <- function(x) paste(sort(x), collapse = "-")
n_edge <- function(x) {
  x <- to_prim(x)
  seg <- x$segment
  edge <- dplyr::distinct(dplyr::mutate(x$segment, edge = apply(cbind(seg$.vertex0, seg$.vertex1), 1, psort)), edge)
  nrow(edge)
}
n_coord <- function(x) {
  x <- to_prim(x)
  nrow(x$path_link_vertex)
}
n_vertex <- function(x) {
  x <- to_prim(x)
  nrow(x$vertex)
}


sfc_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)

 , n_path = n_path
  
  , n_hole = n_hole
  ,  n_segment = n_segment
  ,  n_edge = n_edge
  ,  n_coord = n_coord 
  ,  n_vertex = n_vertex
)

# library(sc)
# library(scsf)
# 
# library(skimr)
# library(sf)
# sfp <- st_read(system.file("shape/nc.shp", package="sf"))
# sfc_funs <- list(
#   missing = n_missing,
#   complete = n_complete,
#   n = length,
#   n_unique = purrr::compose(length, n_unique),
#   valid = purrr::compose(sum, sf::st_is_valid)
# )
# skim_with(sfc = sfc_funs , append = TRUE)
# 
# skim_v(st_geometry(sfp))
# 
# skim(minimal_mesh)
# 



# library(skimr)
# 
# 
# adhoc_funs <- list(
#   missing = n_missing,
#   complete = n_complete,
#   n = length,
#   n_unique = purrr::compose(length, n_unique),
#   funny = function(x) length(x) + 1
# )
# 
# d <- structure(list(a = 1:4, b = structure(as.list(letters[1:4]), class = "adhoc")), class = "data.frame", row.names = letters[1:4])
# 
# skim_with(adhoc = adhoc_funs , append = TRUE)
# 
# skim_v(d$b)
# 
# ## how do we get this to work? 
# skim(d)

