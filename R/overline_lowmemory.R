#' Convert series of overlapping lines into a route network (new method)
#' 
#' @description This function is intended as a replacement for overline() and is significantly faster
#' especially on large datasets. However, it also uses more memory.
#' 
#' @param x An SF data.frame of LINESTRINGS
#' @param attrib character, column names in x to be summed
#' @param ncores integer, how many cores to use in parallel processing, default = 1
#' @param simplify logical, if TRUE group final segments back into lines, default = TRUE
#' @param regionalise integer, during simplification regonalisation is used if the number of segments exceeds this value
#' @family rnet
#' @author Malcolm Morgan
#' @export
#' @return 
#' An SF data.frame of LINESTRINGS
#' @details 
#' The overline2 function breaks each line into many straight segments and then looks for duplicated segments.
#' Attributes are summed for all duplicated segments, and if simplify is TRUE the segments with identical attributes are
#' recombined into linestrings.
#' 
#' Regionalisation breaks the dataset into a 10 x 10 grid and then performed the simplification across each grid. 
#' This significantly reduces computation time for large datasets, but slightly increases the final file size.
#' 
#' A known limitation of this method is that overlapping segments of different lengths are not aggregated. 
#' This can occur when lines stop halfway down a road. Typically these errors are small, 
#' but some artefacts may remain within the resulting data.
#' 
#' Multicore support is only enabled for the egionalised simplification stage as it does not help with other stages.
#' 
#' @examples
#' sl = routes_fast_sf[routes_fast_sf$length > 0, ]
#' sl$bicycle = 1
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' \donttest{
#' region = "isle-of-wight"
#'
#' u = paste0(
#'   "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/",
#'    region,
#'   "/rf.geojson"
#' )
#'
#' sl = sf::read_sf(u)
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' }
overline2 = function(x, attrib, ncores = 1, simplify = TRUE, regionalise = 1e4){
  if(all(sf::st_geometry_type(x) != "LINESTRING")){
    stop("Only LINESTRING is supported")
  }
  if("matchingID" %in% attrib){
    stop("matchingID is not a permitted column name, please rename that column")
  }
  
  x = x[, attrib]
  x_crs = sf::st_crs(x)
  
  message(paste0(Sys.time(), " constructing segments"))
  c1 = sf::st_coordinates(x) 
  sf::st_geometry(x) = NULL
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
  x = x[l1[l1_start], , drop = FALSE] # repeate attributes
  rm(l1, l1_start)
  
  message(paste0(Sys.time(), " transposing 'B to A' to 'A to B'"))
  attributes(c3)$dimnames <- NULL
  c3 <- t(apply(c3, MARGIN = 1, FUN = function(y) {
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
    
  }))
  
  message(paste0(Sys.time(), " removing duplicates"))
  c3 <- as.data.frame(c3)
  names(c3) = c("X1", "Y1", "X2", "Y2")
  c3 <- dplyr::group_by(c3, X1, Y1, X2, Y2)
  matchID <- dplyr::group_indices(c3)
  c3$matchingID <- matchID
  c3 <- c3[!duplicated(c3$matchingID),]
  c3 <- c3[order(c3$matchingID),]
  c3_nodup <- as.matrix(c3[,c("X1", "Y1", "X2", "Y2")])
  rm(c3)
  
  # Calcualte the attributes
  message(paste0(Sys.time(), " restructuring attributes"))
  x$matchingID = matchID
  x <- x %>%
    dplyr::group_by(matchingID) %>%
    dplyr::summarise_all(funs(sum), na.rm = TRUE) %>%
    dplyr::arrange(matchingID)
  x$matchingID = NULL
  
  # Make Geometry
  message(paste0(Sys.time(), " building geometry"))
  sf::st_geometry(x) <- sf::st_as_sfc(pbapply::pblapply(1:nrow(c3_nodup), function(y) {
       sf::st_linestring(matrix(c3_nodup[y, ], ncol = 2, byrow = T))
    }), crs = x_crs)
  
  rm(matchID, c3_nodup)
  # Recombine into fewer lines
  if(simplify){
    
    message(paste0(Sys.time(), " simplifying geometry"))
    if (nrow(x) > regionalise) {
      message(paste0("large data detected, using regionalisation ",nrow(x)))
      suppressWarnings( cents <- sf::st_centroid(x))
      grid <- sf::st_make_grid(cents, what = "polygons")
      inter <- unlist(lapply(sf::st_intersects(cents, grid), `[[`, 1))
      x$grid <- inter
      rm(cents, grid, inter)
      # split into a list of df by grid
      x = split(x , f = x$grid)
      message(paste0(Sys.time(), " regionalisation complete, aggregating flows"))
      if (ncores > 1) {
        cl = parallel::makeCluster(ncores)
        parallel::clusterExport(cl = cl,
                                varlist = c("attrib"),
                                envir = environment())
        parallel::clusterEvalQ(cl, {
          library(sf)
          #library(dplyr)
        })
        overlined_simple = pbapply::pblapply(x, function(y) {
          y <- dplyr::group_by_at(y, attrib)
          y <- dplyr::summarise(y)
        }, cl = cl)
        parallel::stopCluster(cl)
        rm(cl)
      } else{
        overlined_simple = pbapply::pblapply(x, function(y) {
          y <- dplyr::group_by_at(y, attrib)
          y <- dplyr::summarise(y)
        })
      }
      rm(x)
      suppressWarnings(overlined_simple <-
                         dplyr::bind_rows(overlined_simple))
      overlined_simple = as.data.frame(overlined_simple)
      overlined_simple = sf::st_sf(overlined_simple)
      sf::st_crs(overlined_simple) = x_crs
      overlined_simple$grid = NULL
    } else{
      message(paste0(Sys.time(), " aggregating flows"))
      overlined_simple <- dplyr::group_by_at(overlined_simple, attrib)
      overlined_simple <- dplyr::summarise(overlined_simple)
      rm(x)
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
    return(rbind(overlined_simple_l, overlined_simple_ml))
  }else{
    return(x)
  }
  
}



# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()

