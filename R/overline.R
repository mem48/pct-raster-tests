# Convert linestrings to many straight lines
line2segments = function(x, ncores = 1){
  c1 = st_coordinates(x) # Convert SF to matrix
  l1 = c1[,3] # Get which line each point is part of
  l1_start = duplicated(l1) # find the break points between lines
  l1_start = c(l1_start[2:length(l1)],FALSE)
  seqs = seq(1,length(l1),1)[l1_start] # Make list of linestrings
  if(ncores == 1){
    geoms = lapply(seqs, function(y){sf::st_linestring(c1[c(y,y+1),c(1,2)])})
  }else{
    cl = parallel::makeCluster(ncores)
    parallel::clusterExport(cl=cl, varlist=c("c1"), envir = environment())
    #parallel::clusterEvalQ(cl, {library(sf)})
    geoms = pbapply::pblapply(seqs, function(y){sf::st_linestring(c1[c(y,y+1),c(1,2)])}, cl = cl)
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



