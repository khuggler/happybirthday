#' @title makeMaps
#' @description create maps of most recent locations and movement for bighorn sheep
#' @param tempdir temporary directory for storing maps
#' @param gpsdat data.frame of GPS data
#' @param id_df lookup table of animal IDs and serial numbers
#' @return Does not return any values. This function will save maps of the most recent locations in KML and GPS locations as well as the last three days of locations and movement in a leaflet map
#' @details some details here 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  makeMaps(tempdir = 'directory', gpsdat = gpsdat, id_df = id_df)
#'  }
#' }
#' @seealso 
#'  \code{\link[assertthat]{assert_that}}
#'  \code{\link[purrr]{keep}}, \code{\link[purrr]{map}}
#'  \code{\link[htmlwidgets]{saveWidget}}
#' @rdname makeMaps
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr keep walk
#' @importFrom htmlwidgets saveWidget
makeMaps<-function(tempdir, gpsdat, id_df){
  
  savedir = paste0(tempdir, "/", 'Products/')
  if(!dir.exists(savedir)){
    dir.create(savedir)
  }
  

  assertthat::assert_that(class(gpsdat$tdate)[1] == "POSIXct", msg = "TelemDate column must be in POSIXct format")
  
  assertthat::assert_that('Frequency' %in% unique(names(id_df)), msg = "Lookup data must include Frequency")
  assertthat::assert_that('Serial' %in% unique(names(id_df)), msg = "Lookup data must include Serial")
  assertthat::assert_that('IdCol' %in% unique(names(id_df)), msg = "Lookup data must include IdCol")
  
  
  
  
  
uni<-unique(gpsdat$AID)

lastpoint<-data.frame()
for(i in 1:length(uni)){
  sub<-gpsdat[gpsdat$AID==uni[i],]
  sub<-sub[order(sub$tdate,decreasing = T),]
  sub<-as.data.frame(sub)
  
  lastpoint<-rbind(lastpoint,sub[1,])
}
lastpoint$AID<-as.character(lastpoint$AID)


lastpoint<-lastpoint[complete.cases(lastpoint$x),]

lastpoint<-sf::st_as_sf(lastpoint, coords = c('x', 'y'), crs = 4326) 
names(lastpoint)[names(lastpoint) == 'AID']<-'name'


cut<-Sys.time()-lubridate::days(2)
lastpoint$Flag<-ifelse(lastpoint$tdate >= cut, 'http://maps.google.com/mapfiles/kml/pal2/icon18.png', 'http://maps.google.com/mapfiles/kml/pal4/icon48.png')


kmlfile<-paste0(savedir, 'LatestLocs.kml')
kmlname<-'BHS Locations'


# maptools::kmlPoints(lastpoint, kmlfile = kmlfile, name = lastpoint$name, icon = lastpoint$Flag, kmlname = kmlname)
# # plotKML::kml_open(file.name = paste0(savedir, 'LatestLocs.kml'), overwrite = T)
# # plotKML::kml_layer(lastpoint, 
# #               file.name = paste0(savedir, 'LatestLocs.kml'), 
# #              colour = lastpoint$Flag,
# #              alpha = 1.0, 
# #              shape = 'http://maps.google.com/mapfiles/kml/pal2/icon18.png',
# #              points_names = lastpoint$name, 
# #              balloon = FALSE,
# #              labels = 2,
# #              size = 1)
# # plotKML::kml_close(file.name = paste0(savedir, 'LatestLocs.kml'))
# # 
# 
lastpoint<-lastpoint[,names(lastpoint) == 'name']
sf::st_write(lastpoint,paste(savedir,'LatestLocs.kml',sep=''),layer='locs',driver='KML',append = F)





# gg<-gpsdat
# sp::coordinates(gg)<-c('x', 'y')
# sp::proj4string(gg)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 
# # create mapView map
# ids=unique(gg$AID)
# trajectory <- list()
# 
# lasttwelve<-data.frame()
# for (i in ids){
#   spdf<-subset(gg, AID==i)
#   spdf<-spdf[order(spdf$tdate, decreasing = T),]
#   spdf<-spdf[1:12,]
#   spdf$Category<-c(rep("4", 11), "8")
#   bt<-sp::SpatialLines(list(sp::Lines(list(sp::Line(spdf)), "id")))
#   trajectory[[i]]<-sp::Lines(list(sp::Line(spdf)), ID=paste(i))
#   #trajectory[[i]]<-birdtrajectory
#   print(i)
#   
#   lasttwelve<-rbind(data.frame(spdf), lasttwelve)
# }
# 
# traj.sp<-sp::SpatialLines(trajectory)
# 
# trajectory.sp.data <- sp::SpatialLinesDataFrame(traj.sp,
#                                                 data = data.frame(ID = ids), match.ID = FALSE)
# 
# 
# sp::proj4string(trajectory.sp.data)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 
# sp::coordinates(lasttwelve)<-c('x', 'y')
# sp::proj4string(lasttwelve)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
# 
# 
# x<-mapview::mapview(trajectory.sp.data, map.types = 'Esri.WorldImagery', color = "black", legend = FALSE)+ mapview::mapview(lasttwelve, cex = "Category", zcol = "AID", legend = FALSE)
# 
# x<-leafem::addStaticLabels(map = x, label = trajectory.sp.data$ID, no.hide = FALSE, direction = 'top', textOnly = TRUE, textsize = "20px", color = "white")
# mapview::mapshot(x, url = paste0(savedir, "LastTwelve.html"))
# 
# 
# rm(x)


pal <- leaflet::colorFactor(palette = 'Paired',domain = gpsdat$AID)

#create custom icons for most recent locations
icon.active <- makeAwesomeIcon(icon = "star", markerColor = "lightgray", spin=TRUE,
                               iconColor = "black", library = "fa",
                               squareMarker =  FALSE)
icon.inactive <- makeAwesomeIcon(icon = "star", markerColor = "lightgray", spin=FALSE,
                                 iconColor = "black", library = "fa",
                                 squareMarker =  FALSE)

# icon.mortality <- makeIcon("https://www.svgrepo.com/svg/404123/skull-and-crossbones",
#                            iconWidth= 30, iconHeight= 30)

a <- NULL

sheepmap<-gpsdat[gpsdat$tdate>= Sys.time()-lubridate::days(3),]
sheepmap<-sheepmap[complete.cases(sheepmap$x),]
uni<-unique(sheepmap$AID)
sheepmap$popup<-paste0(signif(sheepmap$y, digits = 6), ",", signif(sheepmap$x, digits = 7))
for(n in seq_along(uni)) {
  
  out <- sheepmap[sheepmap$AID == uni[n],]
  out <- out[order(out$tdate, decreasing = FALSE),]
  
  if(nrow(out) > 0) {
    
    f_name <- out$AID[1]
    
    out$Label <- as.character(out$popup)
    out$Popup <- f_name
    
    if(is.null(a)) {
      
      a <- leaflet::leaflet(out) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.NatGeoWorldMap
        ) |>
        leaflet::addCircleMarkers(
          lng = ~x,
          lat = ~y,
          label = ~Label,
          popup = ~Popup,
          color = ~pal(AID),
          radius = 1.5,
          opacity = 1
        ) |>
        leaflet::addPolylines(
          lng = ~x,
          lat = ~y,
          weight = 0.5,
          color = "black",
          opacity = 1
        )
      
    }
    
    cat("\nITERATION:", n, "\n")
    cat("class(a):", paste(class(a), collapse = ", "), "\n")
    cat("typeof(a):", typeof(a), "\n")
    cat("inherits leaflet:", inherits(a, "leaflet"), "\n")   
    
    else{
      
      a <- leaflet::addCircleMarkers(
        map = a,
        data = out,
        lng = ~x,
        lat = ~y,
        label = ~Label,
        popup = ~Popup,
        color = ~pal(AID),
        radius = 1.5,
        opacity = 1
      )
      
      a <- leaflet::addPolylines(
        map = a,
        data = out,
        lng = ~x,
        lat = ~y,
        weight = 0.5,
        color = "black",
        opacity = 1
      )
    }
  }
}
#add mortality locations
#a<-addCircleMarkers(map=a,data=elk2,lng=~longitude, lat=~latitude,popup=~popup, col="red", radius=1.5, opacity=100)

#add layer control
# Take out ESRI provided tiles
esri <- leaflet::providers |>
  purrr::keep(\(x) grepl("^Esri", x))

esri[[11]] <- NULL
esri[[9]]  <- NULL
esri[[8]]  <- NULL
esri[[7]]  <- NULL
esri[[6]]  <- NULL
esri[[2]]  <- NULL

esri <- esri[c(
  "Esri.DeLorme",
  "Esri.WorldImagery",
  "Esri.WorldTopoMap",
  "Esri.NatGeoWorldMap",
  "Esri"
)]

for (x in esri) {
  a <- leaflet::addProviderTiles(
    map = a,
    provider = x,
    group = x
  )
}

a <- leaflet::addLayersControl(
  map = a,
  baseGroups = names(esri),
  options = leaflet::layersControlOptions(collapsed = TRUE)
)

a <- leaflet::addLegend(
  map = a,
  pal = pal,
  values = sheepmap$AID,
  group = "sheepmap",
  opacity = 1,
  position = "bottomleft"
)
#a #plot

#a<- a%>% addTitle(text=paste('Updated:', Sys.time()), color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
# 



htmlwidgets::saveWidget(a, file=paste(savedir, 'Last3Days.html', sep = ""),
                        title="SheepMovement", selfcontained=TRUE)


rm(a)











a<-NULL

sheepmap<-gpsdat[gpsdat$tdate>= Sys.time()-lubridate::hours(12),]
sheepmap<-sheepmap[complete.cases(sheepmap$x),]
uni<-unique(sheepmap$AID)
sheepmap$popup<-paste0(signif(sheepmap$y, digits = 6), ",", signif(sheepmap$x, digits = 7))
for(n in seq_along(uni)) {
  
  out <- sheepmap[sheepmap$AID == uni[n],]
  out <- out[order(out$tdate, decreasing = FALSE),]
  
  if(nrow(out) > 0) {
    
    f_name <- out$AID[1]
    
    out$Label <- as.character(out$popup)
    out$Popup <- f_name
    
    if(is.null(a)) {
      
      a <- leaflet::leaflet(out) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.NatGeoWorldMap
        ) |>
        leaflet::addCircleMarkers(
          lng = ~x,
          lat = ~y,
          label = ~Label,
          popup = ~Popup,
          color = ~pal(AID),
          radius = 1.5,
          opacity = 1
        ) |>
        leaflet::addPolylines(
          lng = ~x,
          lat = ~y,
          weight = 0.5,
          color = "black",
          opacity = 1
        )
      
    }else{
      
      a <- leaflet::addCircleMarkers(
        map = a,
        data = out,
        lng = ~x,
        lat = ~y,
        label = ~Label,
        popup = ~Popup,
        color = ~pal(AID),
        radius = 1.5,
        opacity = 1
      )
      
      a <- leaflet::addPolylines(
        map = a,
        data = out,
        lng = ~x,
        lat = ~y,
        weight = 0.5,
        color = "black",
        opacity = 1
      )
    }
  }
}
#add mortality locations
#a<-addCircleMarkers(map=a,data=elk2,lng=~longitude, lat=~latitude,popup=~popup, col="red", radius=1.5, opacity=100)

#add layer control
# Take out ESRI provided tiles
esri <- leaflet::providers |>
  purrr::keep(\(x) grepl("^Esri", x))

esri[[11]] <- NULL
esri[[9]]  <- NULL
esri[[8]]  <- NULL
esri[[7]]  <- NULL
esri[[6]]  <- NULL
esri[[2]]  <- NULL

esri <- esri[c(
  "Esri.DeLorme",
  "Esri.WorldImagery",
  "Esri.WorldTopoMap",
  "Esri.NatGeoWorldMap",
  "Esri"
)]

for (x in esri) {
  a <- leaflet::addProviderTiles(
    map = a,
    provider = x,
    group = x
  )
}

a <- leaflet::addLayersControl(
  map = a,
  baseGroups = names(esri),
  options = leaflet::layersControlOptions(collapsed = TRUE)
)

a <- leaflet::addLegend(
  map = a,
  pal = pal,
  values = sheepmap$AID,
  group = "sheepmap",
  opacity = 1,
  position = "bottomleft"
)

#a<- a%>% addTitle(text=paste('Updated:', Sys.time()), color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
# 



htmlwidgets::saveWidget(a, file=paste(savedir, 'Last12Hours.html', sep = ""),
                        title="SheepMovement", selfcontained=TRUE)


rm(a)

}




