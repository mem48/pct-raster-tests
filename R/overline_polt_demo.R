library(stplanr)
library(sf)
library(tmap)
#> Loading required package: sp
routes_fast_sf$val <- 1:nrow(routes_fast_sf)
routes_fast_sf <- routes_fast_sf[,c("val")]
tmap_mode("view")
foo = routes_fast_sf[!routes_fast_sf$val %in% 
                       c(38,44,39, 27, 45, 38, 31, 20, 24, 19, 21, 18, 32),]
foo = lwgeom::st_make_valid(foo)
qtm(foo, lines.col = "val", lines.lwd = 3)


#sl <- routes_fast_sf[c(38,44,26,16,46), ]
#sl <- routes_fast_sf[c(38,44,26,16,20), ]
sl <- routes_fast_sf[c(38,44,26,16,30), ]
sl$val <- 1:5
sl$col <- c("red","orange","yellow","green","cadetblue1")

rnet1 <- overline2(sl[1:2,], attrib = "val")
rnet2 <- overline2(sl[1:3,], attrib = "val")
rnet3 <- overline2(sl[1:5,], attrib = "val")

if(TRUE){
  tm1a <- tm_shape(sl[2:1,]) + 
    tm_lines("col", lwd = "val", scale = 8) + 
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  tm1b <- tm_shape(rnet1) + 
    tm_lines("val", lwd = 6,
             style = "fixed", breaks = c(1:11),
             palette = "Spectral", midpoint = 4) +  
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  tm2a <- tm_shape(sl[3:1,]) + 
    tm_lines("col", lwd = "val", scale = 16) +  
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  tm2b <- tm_shape(rnet2) + 
    tm_lines("val", lwd = 6,
             style = "fixed", breaks = c(1:11),
             palette = "Spectral", midpoint = 4) +  
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  tm3a <- tm_shape(sl[5:1,]) + 
    tm_lines("col", lwd = "val", scale = 20) + 
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  tm3b <- tm_shape(rnet3) + 
    tm_lines("val", lwd = 6,
             style = "fixed", breaks = c(1:11),
             palette = "Spectral", midpoint = 4) +  
    tm_text("val") +
    tm_layout(legend.show = FALSE)
  
  
}
tmap_mode("plot")
png("fig3.png", width = 2048, height = 1024, pointsize = 36)
tmap_arrange(tm1a,tm2a,tm3a,tm1b,tm2b,tm3b)
dev.off()
tmap_mode("view")
qtm(tm3b, lines.col = "val", lines.lwd = 3)
