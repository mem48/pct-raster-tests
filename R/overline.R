# Convert linestrings to many straight lines
line2segments = function(x, ncores = 1){
  c1 = st_coordinates(x) # Convert SF to matrix
  l1 = c1[,3] # Get which line each point is part of
  l1_start = duplicated(l1) # find the break points between lines
  l1_start = c(l1_start[2:length(l1)],FALSE)
  seqs = seq(1,length(l1),1)[l1_start] # Make list of linestrings
  if(ncores == 1){
    geoms = pbapply::pblapply(seqs, function(y){sf::st_linestring(c1[c(y,y+1),c(1,2)])})
  }else{
    cl = parallel::makeCluster(ncores)
    parallel::clusterExport(cl=cl, varlist=c("c1"), envir = environment())
    #parallel::clusterEvalQ(cl, {library(sf)})
    #geoms = pbapply::pblapply(seqs, function(y){sf::st_linestring(c1[c(y,y+1),c(1,2)])}, cl = cl)
    geoms = parallel::parLapply(cl = cl, seqs, function(y){sf::st_linestring(c1[c(y,y+1),c(1,2)])})
    parallel::stopCluster(cl)
  }
  geoms = st_as_sfc(geoms, crs = st_crs(x))
  dat = x # extract attributes
  st_geometry(dat) = NULL
  dat = as.data.frame(dat)
  dat = dat[l1[l1_start],, drop = FALSE] # repeate attriibutes
  st_geometry(dat) = geoms # put together
  return(dat)
}

# Overlay duplicated lines
#' @param x SF data frame of linestrings
#' @param attrib column name to be summed
#' @param drop_0 remove geometry of lines where summed values is Zero
#' 
overline_malcolm = function(x, attrib, ncores = 1){
  if(all(st_geometry_type(x) != "LINESTRING")){
    message("Only LINESTRING is supported")
    stop()
  }
  if("matchingID" %in% names(attrib)){
    message("matchingID is not a permitted column name, please rename that column")
    stop()
  }
  x = x[,attrib]
  x_split = line2segments(x, ncores = ncores) # Split into segments
  x_dupe = duplicated(x_split$geometry)
  times = unique(x_split[,"geometry", drop = FALSE]) # make lookup table
  times$id = seq(1,nrow(times))
  x_split$matchingID = times$id[match( x_split$geometry, times$geometry)]
  x_group = x_split[,c("matchingID",attrib)]
  st_geometry(x_group) = NULL 
  x_group = x_group %>%
    group_by(matchingID) %>%
    summarise_all(funs(sum), na.rm = TRUE)
  x_nodupe = x_split[!x_dupe,]
  #x_nodupe$value = NULL
  #x_nodupe[[attrib]] = NULL
  x_nodupe = x_nodupe[,"matchingID"]
  x_nodupe = left_join(x_nodupe,x_group,"matchingID" )
  x_nodupe = x_nodupe[,attrib]
  return(x_nodupe)
}


overline_malcolm2 = function(x, attrib){
  if(all(sf::st_geometry_type(x) != "LINESTRING")){
    message("Only LINESTRING is supported")
    stop()
  }
  if("matchingID" %in% names(attrib)){
    message("matchingID is not a permitted column name, please rename that column")
    stop()
  }
  x = x[,attrib]
  # Reduce copying by importing function directly
  ############
  c1 = st_coordinates(x) # Convert SF to matrix
  l1 = c1[,3] # Get which line each point is part of
  c1 = c1[,1:2]
  l1_start = duplicated(l1) # find the break points between lines
  l1_start = c(l1_start[2:length(l1)],FALSE)
  c2 = c1[2:nrow(c1),1:2] # Create new coords offset by one row
  c2 = rbind(c2,c(NA,NA))
  c2[nrow(c1),] = c(NA,NA)
  c2[!l1_start,1] = NA
  c2[!l1_start,2] = NA
  c3 = cbind(c1,c2) # make new matrix of start and end coords
  rm(c1,c2)
  c3 = c3[!is.na(c3[,3]),]
  c3_dup = duplicated(c3) # de-duplicate
  c3_nodup = c3[!c3_dup,]
  matchID = match(data.frame(t(c3)), data.frame(t(c3_nodup))) # make looup 
  rm(c3,c3_dup)
  
  # Calcualte the attributes
  x_split = x # extract attributes
  st_geometry(x_split) = NULL
  x_split = as.data.frame(x_split)
  x_split = x_split[l1[l1_start],, drop = FALSE] # repeate attriibutes
  x_split$matchingID = matchID
  x_split = x_split %>%
    group_by(matchingID) %>%
    summarise_all(funs(sum), na.rm = TRUE) %>%
    arrange(matchingID)
  x_split$matchingID = NULL
  
  # Make Geometry
  geoms = pbapply::pblapply(1:nrow(c3_nodup), function(y){sf::st_linestring(matrix(c3_nodup[y,], ncol = 2, byrow = T))})
  rm(c3_nodup)
  geoms = st_as_sfc(geoms, crs = st_crs(x))
  st_geometry(x_group) = geoms # put together
  return(x_group)
}
