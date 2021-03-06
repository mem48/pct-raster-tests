#' Overlay duplicated lines
#' @param x SF data frame of linestrings
#' @param attrib character, column name to be summed
#' @param ncores integer, how many cores to use in parallel processing
#' @param simplify, logical, if TRUE group final segments back into lines
#' @param regionalise, integer, during simplification regonalisation is used if the number of segments exceeds this value
overline2 = function(x, attrib, ncores = 1, simplify = TRUE, regionalise = 1e4){
  if(all(sf::st_geometry_type(x) != "LINESTRING")){
    message("Only LINESTRING is supported")
    stop()
  }
  if("matchingID" %in% names(x)){
    message("matchingID is not a permitted column name, please rename that column")
    stop()
  }
  
  x = x[, attrib]
  x_crs = sf::st_crs(x)
  
  message(paste0(Sys.time(), " constructing segments"))
  c1 = sf::st_coordinates(x) # Convert SF to matrix
  l1 = c1[, 3] # Get which line each point is part of
  c1 = c1[, 1:2]
  l1_start = duplicated(l1) # find the break points between lines
  l1_start = c(l1_start[2:length(l1)], FALSE)
  c2 = c1[2:nrow(c1), 1:2] # Create new coords offset by one row
  c2 = rbind(c2, c(NA, NA))
  c2[nrow(c1), ] = c(NA, NA)
  c2[!l1_start, 1] = NA
  c2[!l1_start, 2] = NA
  c3 = cbind(c1, c2) # make new matrix of start and end coords
  rm(c1, c2)
  c3 = c3[!is.na(c3[, 3]), ]
  message(paste0(Sys.time(), " transposing 'B to A' to 'A to B'"))
  c3 = split(c3, 1:nrow(c3))
  c3 = pbapply::pblapply(c3, function(y) {
    if(y[1] != y[3]){
      if (y[1] > y[3]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    } else{
      if (y[2] > y[4]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    }
    
  })
  c3 = matrix(unlist(c3), byrow = TRUE, nrow = length(c3))
  message(paste0(Sys.time(), " removing duplicates"))
  c3_dup = duplicated(c3) # de-duplicate
  c3_nodup = c3[!c3_dup, ]
  c3_df = as.data.frame(c3)
  names(c3_df) = c("X1", "Y1", "X2", "Y2")
  c3_nodup_df = as.data.frame(c3_nodup)
  names(c3_nodup_df) = c("X1", "Y1", "X2", "Y2")
  c3_nodup_df$matchID = 1:nrow(c3_nodup_df)
  matchID = dplyr::left_join(c3_df,
                             c3_nodup_df,
                             by = c(
                               "X1" = "X1",
                               "Y1" = "Y1",
                               "X2" = "X2",
                               "Y2" = "Y2"
                             ))
  matchID = matchID$matchID
  rm(c3_df, c3_nodup_df, c3, c3_dup)

  # Calcualte the attributes
  message(paste0(Sys.time(), " restructuring attributes"))
  x_split = x # extract attributes
  sf::st_geometry(x_split) = NULL
  x_split = as.data.frame(x_split)
  x_split = x_split[l1[l1_start], , drop = FALSE] # repeate attriibutes
  x_split$matchingID = matchID
  x_split = x_split %>%
    group_by(matchingID) %>%
    summarise_all(funs(sum), na.rm = TRUE) %>%
    arrange(matchingID)
  x_split$matchingID = NULL
  
  # Make Geometry
  message(paste0(Sys.time(), " building geometry"))
  geoms = pbapply::pblapply(1:nrow(c3_nodup), function(y) {
    sf::st_linestring(matrix(c3_nodup[y, ], ncol = 2, byrow = T))
  })
  rm(c3_nodup, l1, l1_start, matchID)
  geoms = st_as_sfc(geoms, crs = st_crs(x))
  st_geometry(x_split) = geoms # put together
  rm(geoms, x)
  
  # Recombine into fewer lines
  if(simplify){
    
    message(paste0(Sys.time(), " simplifying geometry"))
    if (nrow(x_split) > regionalise) {
      message(paste0("large data detected, using regionalisation"))
      suppressWarnings( cents <- sf::st_centroid(x_split))
      grid = sf::st_make_grid(cents, what = "polygons")
      inter = unlist(lapply(sf::st_intersects(cents, grid), `[[`, 1))
      x_split$grid = inter
      rm(cents, grid, inter)
      # split into a list of df by grid
      x_split = split(x_split , f = x_split$grid)
      message(paste0(Sys.time(), " regionalisation complete"))
      if (ncores > 1) {
        cl = parallel::makeCluster(ncores)
        parallel::clusterExport(cl = cl,
                                varlist = c("attrib"),
                                envir = environment())
        parallel::clusterEvalQ(cl, {
          library(sf)
          library(dplyr)
        })
        overlined_simple = pbapply::pblapply(x_split, function(y) {
          y %>% dplyr::group_by_at(attrib) %>%  dplyr::summarise()
        }, cl = cl)
        parallel::stopCluster(cl)
        rm(cl)
      } else{
        overlined_simple = pbapply::pblapply(x_split, function(y) {
          y %>% dplyr::group_by_at(attrib) %>%  dplyr::summarise()
        })
      }
      rm(x_split)
      suppressWarnings(overlined_simple <-
                         dplyr::bind_rows(overlined_simple))
      overlined_simple = as.data.frame(overlined_simple)
      overlined_simple = sf::st_sf(overlined_simple)
      sf::st_crs(overlined_simple) = x_crs
      overlined_simple$grid = NULL
    } else{
      overlined_simple = x_split %>%
        dplyr::group_by_at(attrib) %>%
        dplyr::summarise()
      rm(x_split)
    }
    
    #Separate our the linestrings and the mulilinestrings
    message(paste0(Sys.time(), " rejoining segments into linestrings"))
    geom_types = sf::st_geometry_type(overlined_simple)
    overlined_simple_l = overlined_simple[geom_types == "LINESTRING", ]
    overlined_simple_ml = overlined_simple[geom_types == "MULTILINESTRING", ]
    rm(overlined_simple, geom_types)
    overlined_simple_ml = sf::st_line_merge(overlined_simple_ml)
    suppressWarnings(overlined_simple_ml <-
                       sf::st_cast(
                         sf::st_cast(overlined_simple_ml, "MULTILINESTRING"),
                         "LINESTRING"
                       ))
    
    overlined_fin = rbind(overlined_simple_l, overlined_simple_ml)
    
    return(overlined_fin)
  }else{
    return(x_split)
  }
  
}

#' #' @examples 
#' library(sf)
#' library(dplyr)
#' library(mapview)
#' 
#' region = "london"
#' 
#' u = paste0(
#'   "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/",
#'    region,
#'   "/rf.geojson"
#' )
#' 
#' sl = read_sf(u)
#' system.time({rnet = overline2(sl, "bicycle", ncores = 4)})
#' mapview(rnet, lwd = rnet$bicycle / mean(rnet$bicycle))


