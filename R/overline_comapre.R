#########
od2line.sf <- function(flow, zones, destinations = NULL,
                       zone_code = names(zones)[1],
                       origin_code = names(flow)[1],
                       dest_code = names(flow)[2],
                       zone_code_d = NA, silent = TRUE) {
  if (grepl(pattern = "POLYGON", x = unique(sf::st_geometry_type(zones)))) {
    zones <- sf::st_centroid(zones)
  }
  
  coords_o <- sf::st_coordinates(zones)[, 1:2]
  
  origin_points <- coords_o[match(flow[[origin_code]], zones[[zone_code]]), ]
  
  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }
    
    dest_points <- coords_o[match(flow[[dest_code]], zones[[zone_code]]), ]
  } else {
    dest_points <- coords_o[match(flow[[dest_code]], destinations[[zone_code_d]]), ]
  }
  
  l <- lapply(1:nrow(flow), function(x)
    sf::st_linestring(rbind(origin_points[x, ], dest_points[x, ]))) %>%
    sf::st_sfc(crs = sf::st_crs(zones))
  sf::st_sf(flow, geometry = l)
}

################################
od2line.sf2 <- function(flow, zones, destinations = NULL,
                       zone_code = names(zones)[1],
                       origin_code = names(flow)[1],
                       dest_code = names(flow)[2],
                       zone_code_d = NA, silent = TRUE) {
  if (grepl(pattern = "POLYGON", x = unique(sf::st_geometry_type(zones)))) {
    zones <- sf::st_centroid(zones)
  }
  
  coords_o <- sf::st_coordinates(zones)[, 1:2]
  
  origin_points <- coords_o[match(flow[[origin_code]], zones[[zone_code]]), ]
  
  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }
    
    dest_points <- coords_o[match(flow[[dest_code]], zones[[zone_code]]), ]
  } else {
    dest_points <- coords_o[match(flow[[dest_code]], destinations[[zone_code_d]]), ]
  }
  
  mat1 <- cbind(origin_points, dest_points)
  l <- split(mat1,1:nrow(flow))
  l <- lapply(l,function(x){sf::st_linestring(matrix(x,ncol = 2, byrow = T))})
  names(l) <- NULL
  l <- sf::st_sfc(l, crs = sf::st_crs(zones))
  res <- sf::st_sf(flow, geometry = l)
  #names(res$geometry) <- NULL
  return(res)
}

#########################
flow = stplanr::flow
zones = stplanr::cents_sf
flow = flow[rep(1:nrow(flow),10000),]


system.time(res1 <- od2line.sf(flow, zones))
system.time(res2 <- od2line.sf2(flow, zones))
identical(res1, res2)
identical(res1$geometry[1], res2$geometry[1])
plot(res1$geometry[1])
plot(res2$geometry[1])
