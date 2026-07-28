#' @importFrom collar ats_login fetch_ats_positions ats_logout get_paths fetch_vectronics
#' @importFrom dplyr bind_rows


getData<-function(id_df, tempdir = NA, veckeys = NA, telonic_usrs = NA, telonic_pass = NA, ATS_usrs = NA, ATS_pass = NA, lotek_usrs = NA, lotek_pass = NA, tzone = 'America/Los_Angeles', subsetmonth = "02"){
  
  require(dplyr)
  require(collar)
  
  if(!dir.exists(tempdir)){
    dir.create(tempdir)
  }
  
  mans<-id_df$Brand
  
  full.tel<-data.frame()
  tel<-NA
if('Telonics' %in% mans){
  
  for(l in 1:length(telonic_usrs)){
    username = telonic_usrs[l]
    password =telonic_pass[l]   # must be \\ slashes
    TDC_path = "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe"
    keep.reports = FALSE
    
    # create a folder for telonics data to go
    fldr_out<-paste0(tempdir, "/", 'TelonicsGPSData')
    if(!dir.exists(fldr_out)){
      dir.create(fldr_out)
    }
    
    if(all(c("processx","sf") %in% installed.packages()[, 1]) == FALSE) 
      stop("You must install the following packages: processx and sf")
    
    # some tests
    if(length(dir(fldr_out))>1)
      stop("Your fldr_out must be empty to proceed!")
    
    # create a reports folder
    fldr_reports <- paste0(fldr_out, "\\reports")
    dir.create(fldr_reports)
    
    
    # create the xml file
    txt <- paste("<BatchSettingsV2>",
                 "\t<Globalstar>", 
                 paste0("\t\t<Username>",username,"</Username>"),
                 paste0("\t\t<Password>",password,"</Password>"),
                 "\t</Globalstar>", 
                 "\t<DownloadData>true</DownloadData>",
                 "\t<ConvertAllData />",
                 paste0("\t<BatchLog>",fldr_out,"\\BatchLog.txt</BatchLog>"),
                 paste0("\t<OutputFolder>",fldr_reports,"</OutputFolder>"),
                 "<ReportFileMode>overwrite</ReportFileMode>",
                 "</BatchSettingsV2>",
                 sep="\n")
    
    Batch_path <- paste0(fldr_out, "\\TelonicsBatchFile.xml")
    # save the xml file
    cat(txt, file=Batch_path)
    
    print("Downloading data from Telonics")
    Batch_path = paste0("/batch:", Batch_path)  # create new batch path for processx
    processx::run(TDC_path, Batch_path)  # TDC should be closed on your computer
    
    print("Importing CSV files")
    #Import the csv files from batch ####
    # Create a list of the data you will import
    fls <- list.files(fldr_reports, ".csv$")
    
    ## Run a loop that goes over the list, cleans and merges the data
    # Create an empty data frame where all the individuals will be merged in
    fixes <- do.call(rbind, lapply(1:length(fls), function(i){
      # The skip parameter is because there is some meta information above where the recordings begin
      df.i = read.csv(paste0(fldr_reports,"/",fls[i]), skip = 22, header = TRUE)
      
      # Get the ID and add it as a column (I am using the name the file is saved under and extracting the
      # component that will match with the way it is saved in my meta data column)
      df.i$CollarSerialNumber <- substr(fls[i], 1, 7)
      
      # Isolate the cases with a successful fix attempt
      df.i <- df.i[which(df.i$GPS.Fix.Attempt=="Succeeded"),]
      
      print(paste0(nrow(df.i), " rows of data imported for individual: ", substr(fls[i], 1, 7)))
      
      # Work on the DateTime stamp. It is a character string so I will first convert it to POSIXct
      # I always try to avoid deleting raw data (you never know when you will need it) so I will create a new DateTime Column
      df.i$GPS.Fix.Time = as.POSIXct(df.i$GPS.Fix.Time, format="%Y.%m.%d %H:%M:%S", tz = "UTC")
    
      return(df.i)

    }))
    
    # order by serial number and then by date
    fixes <- fixes[order(fixes$CollarSerialNumber, fixes$GPS.Fix.Time),]
    
    
    tel<-data.frame(fixes)
    
    tel <- tel %>%
      rename(tdate = "GPS.Fix.Time", x = "GPS.Longitude", y = "GPS.Latitude", SN = "CollarSerialNumber") %>%
      dplyr::select(SN, tdate, x, y)
    
    full.tel<-rbind(tel, full.tel)
    

    
  }
  
  # remove all the temporary files if keep.reports = FALSE
  
  fls = dir(fldr_out, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  unlink(fls, force=TRUE, recursive = TRUE)
  
  
}
   
    
   
  
  
  
  # ATS data 
  ats<-NA
  ats.full<-data.frame()
  if('ATS' %in% mans){
    
    for (i in 1:length(ATS_usrs)){
    collar::ats_login(ATS_usrs[i], ATS_pass[i])
    sns<-id_df[id_df$Brand == "ATS",]$Serial
    sns<-paste0("0", sns) # missing leading zero
    
    out.acct <- collar::fetch_ats_positions(device_id = sns)
    
  
    
    # ats timezones are programmed to individual collars, all of our as of Jan 2021 are programmed to Pacific time
    programmed_pacific <- c("043205", "043202", "043204", "043207",
                            "042803", "042804","046977","047204", "047205", "047207", "047208", "047212", "047213", "047214")
    programmed_mountain <- c("030230", "042800", "048782", "048783", "048784", "048785", "048786", "048787", "048788", "048789", "048790", "048791", "048792", "048793", "048794", "048796", "048797") # we can add collar ids as new collars are deployed
    
    #out <- dplyr::bind_rows(out.acct.1, out.acct.2, out.acct.3) 
    
    out.acct$JulianDay = formatC(out.acct$JulianDay, width = 3, format = "d", flag = "0")
    out.acct$Hour = formatC(out.acct$Hour, width = 2, format = "d", flag = "0")
    out.acct$Minute = formatC(out.acct$Minute, width = 2, format = "d", flag = "0")
    
    
    ats <- out.acct %>%
      rename(tdate = "DateLocal", SN = 'CollarSerialNumber', x = 'Longitude', y = 'Latitude') %>%
      dplyr::select(SN, tdate, x, y)
    ats<-data.frame(ats)
    
    collar::ats_logout()
    
    ats.full<-rbind(ats, ats.full)
    
    
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  # Lotek data 
  lotek<-NA
  lotek.full<-data.frame()
  if('Lotek' %in% mans){
    
    
    tim<-as.character(paste(as.numeric(strftime(Sys.time(),format='%Y'))-1,'-', subsetmonth, '-01 00:00:00',sep=''))
    
    for (i in 1:length(lotek_usrs)){
      collar::lotek_login(lotek_usrs[i], lotek_pass[i])
      #sns<-id_df[id_df$Brand == "ATS",]$Serial
      #sns<-paste0("0", sns) # missing leading zero
      sns<-id_df[id_df$Brand == "Lotek",]$Serial
      out.acct <- collar::fetch_lotek_positions(device_id = sns, start_date = tim, end_date = as.character(Sys.time()))
      
      
    
      
      lotek <- out.acct %>%
        rename(tdate = "UploadTimeStamp", SN = 'DeviceID', x = 'Longitude', y = 'Latitude') %>%
        dplyr::select(SN, tdate, x, y)
      lotek<-data.frame(lotek)
      
      collar::lotek_logout()
      
    lotek.full<-rbind(lotek, lotek.full)
      
      
    }
    
  }
  
  
  
  
  
  
  
  
  vec<-NA
  if('Vectronic' %in% mans){
    
    key_path <- collar::get_paths(veckeys)
    vecdat<-collar::fetch_vectronics(key_path, type = "gps")
    
    vec <- vecdat %>%
      rename(tdate = "acquisitiontime", SN = 'idcollar', x = 'longitude', y = 'latitude') %>%
      dplyr::select(SN, tdate, x, y)
    
    vec$tdate <-as.POSIXct(vec$tdate,
                                       format = paste0("%Y-%m-%d", "T", "%H:%M:%S"),
                                       tz = "UTC",
                                       origin = vec$tdate)
    vec<-data.frame(vec)
    
    vec$tdate<-lubridate::with_tz(vec$tdate, tzone = tzone)
  }
 
  # bind all the data together 
  gps<-rbind(ats.full, vec, full.tel, lotek.full)
  
  # add animal IDs to data-- remove data before 2023
   tim<-paste(as.numeric(strftime(Sys.time(),format='%Y'))-1,'-', subsetmonth, '-01 00:00:00',sep='')
  
  gps<-gps[gps$tdate >= tim,]
  
  id_df$Serial<-ifelse(id_df$Brand == "ATS", paste0("0", id_df$Serial), id_df$Serial)
  id_df$IdCol<-id_df$Serial
  
  gps<-gps[gps$SN %in% id_df$Serial,]
  gps<-merge(gps, id_df[, c('Serial', 'VitFreq', 'Frequency', 'Species', 'IdCol', 'AID', 'Sex')], by.x = "SN", by.y = "Serial")
  
  gps<-gps[complete.cases(gps$x),]
  
 
  return(gps)
}
  
x<-getData(id_df = id_df, tempdir = tempdir, veckeys = veckeys, telonic_usrs = telonic_usrs, telonic_pass = telonic_pass, ATS_usrs = ATS_usrs, ATS_pass =ATS_pass, lotek_usrs= lotek_usrs, lotek_pass = lotek_pass, tzone = tzone, subsetmonth = subsetmonth)

gpsdat=x
require(leaflet)

savedir = paste0(tempdir, "/", 'Products/')
if(!dir.exists(savedir)){
  dir.create(savedir)
}


assertthat::assert_that(class(gpsdat$tdate)[1] == "POSIXct", msg = "TelemDate column must be in POSIXct format")

assertthat::assert_that('Frequency' %in% unique(names(id_df)), msg = "Lookup data must include Frequency")
assertthat::assert_that('Serial' %in% unique(names(id_df)), msg = "Lookup data must include Serial")
assertthat::assert_that('IdCol' %in% unique(names(id_df)), msg = "Lookup data must include IdCol")


library(sf)
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

sp::coordinates(lastpoint)<-~x+y
sp::proj4string(lastpoint)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

names(lastpoint)[names(lastpoint) == 'AID']<-'name'

#' add in a conditional coloring
cut<-Sys.time()-lubridate::days(2)
lastpoint$Flag<-ifelse(lastpoint$tdate >= cut, 'http://maps.google.com/mapfiles/kml/pal2/icon18.png', 'http://maps.google.com/mapfiles/kml/pal4/icon48.png')


kmlfile<-paste0(savedir, 'LatestLocs.kml')
kmlname<-'BHS Locations'

lastpoint<-st_as_sf(lastpoint)
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





-# gg<-gpsdat
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



sheepmap<-gpsdat[gpsdat$tdate>= Sys.time()-lubridate::days(3),]
sheepmap<-sheepmap[complete.cases(sheepmap$x),]
uni<-unique(sheepmap$AID)
sheepmap$popup<-paste0(signif(sheepmap$y, digits = 6), ",", signif(sheepmap$x, digits = 7))
for(n in 1:length(uni)){
  out<-sheepmap[sheepmap$AID == uni[n],]
  out<-out[order(out$tdate, decreasing = FALSE),]
  
  if(nrow(out)>0){
    f_name<-out$AID[1] #create unique filename
    
    #set up leaflet options
    out$Label<-NA
    out$Label<-as.character(out$popup) #Create hover over layer
    out$Popup<-NA
    out$Popup<-f_name #Create hover over layer
    if(exists("a")==FALSE){ #build leaflet with first animal
      a<-out %>%
        leaflet() %>%
        #addTiles() %>%
        #addProviderTiles("Esri.WorldImagery") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  #choose base layer
        addCircleMarkers(lng=~x, lat=~y, label=~Label, popup=~Popup,color=~pal(AID), radius=1.5, opacity=100) %>% #add as circles
        addPolylines(lng=~x, lat=~y, weight=0.5, color="black", opacity=200)
    }
    
    
    a<-addCircleMarkers(map=a,data=out,lng=~x, lat=~y, label=~Label, popup=~Popup,color=~pal(AID), radius=1.5, opacity=100)
    a<-addPolylines(map=a, data=out,lng=~x, lat=~y, weight=0.5, color="black", opacity=200)
    
    # if(out$idmortalitystatus[nrow(out)] == "5"){
    #   #add mortality markers
    #   a<-addMarkers(map=a, data=out[nrow(out),],lng=~longitude, lat=~latitude,label=~Label, popup=~Popup, icon=icon.mortality)
    # }
    
    #add active/inactive/mort icons ---- inactive defined as no iridium uplink in last 2 days
    
    if(out$tdate[nrow(out)] <= Sys.time() - as.difftime(2, unit= "days")){ #if its inactive
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label=out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.inactive)
    }
    
    if(out$tdate[nrow(out)] >= Sys.time() - as.difftime(2, unit = "days")) { #make it active
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label= out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.active)
    }
  }
  
}

#add mortality locations
#a<-addCircleMarkers(map=a,data=elk2,lng=~longitude, lat=~latitude,popup=~popup, col="red", radius=1.5, opacity=100)

#add layer control
# Take out ESRI provided tiles
esri <- providers %>%
  purrr::keep(~ grepl('^Esri',.))
#remove a bunch of worthless esri layers
esri[[11]]<-NULL
esri[[9]]<-NULL
esri[[8]]<-NULL
esri[[7]]<-NULL
esri[[6]]<-NULL
esri[[2]]<-NULL
#reorder list so desired list is on top of legend and as primary basemap
esri <- esri[c("Esri.DeLorme", "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.NatGeoWorldMap", "Esri")]
esri %>%
  purrr::walk(function(x) a <<- a %>% addProviderTiles(x,group=x))
a<-a %>%
  addLayersControl(
    baseGroups = names(esri),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend(pal = pal, values = sheepmap$AID, group = "sheepmap", opacity=100, position = "bottomleft")
#a #plot

#a<- a%>% addTitle(text=paste('Updated:', Sys.time()), color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
# 



htmlwidgets::saveWidget(a, file=paste(savedir, 'Last3Days.html', sep = ""),
                        title="SheepMovement", selfcontained=TRUE)


rm(a)













sheepmap<-gpsdat[gpsdat$tdate>= Sys.time()-lubridate::hours(12),]
sheepmap<-sheepmap[complete.cases(sheepmap$x),]
uni<-unique(sheepmap$AID)
sheepmap$popup<-paste0(signif(sheepmap$y, digits = 6), ",", signif(sheepmap$x, digits = 7))
for(n in 1:length(uni)){
  out<-sheepmap[sheepmap$AID == uni[n],]
  out<-out[order(out$tdate, decreasing = FALSE),]
  
  if(nrow(out)>0){
    f_name<-out$AID[1] #create unique filename
    
    #set up leaflet options
    out$Label<-NA
    out$Label<-as.character(out$popup) #Create hover over layer
    out$Popup<-NA
    out$Popup<-f_name #Create hover over layer
    if(exists("a")==FALSE){ #build leaflet with first animal
      a<-out %>%
        leaflet() %>%
        #addTiles() %>%
        #addProviderTiles("Esri.WorldImagery") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  #choose base layer
        addCircleMarkers(lng=~x, lat=~y, label=~Label, popup=~Popup,color=~pal(AID), radius=1.5, opacity=100) %>% #add as circles
        addPolylines(lng=~x, lat=~y, weight=0.5, color="black", opacity=200)
    }
    
    
    a<-addCircleMarkers(map=a,data=out,lng=~x, lat=~y, label=~Label, popup=~Popup,color=~pal(AID), radius=1.5, opacity=100)
    a<-addPolylines(map=a, data=out,lng=~x, lat=~y, weight=0.5, color="black", opacity=200)
    
    # if(out$idmortalitystatus[nrow(out)] == "5"){
    #   #add mortality markers
    #   a<-addMarkers(map=a, data=out[nrow(out),],lng=~longitude, lat=~latitude,label=~Label, popup=~Popup, icon=icon.mortality)
    # }
    
    #add active/inactive/mort icons ---- inactive defined as no iridium uplink in last 2 days
    
    if(out$tdate[nrow(out)] <= Sys.time() - as.difftime(2, unit= "days")){ #if its inactive
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label=out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.inactive)
    }
    
    if(out$tdate[nrow(out)] >= Sys.time() - as.difftime(2, unit = "days")) { #make it active
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label= out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.active)
    }
  }
  
}

#add mortality locations
#a<-addCircleMarkers(map=a,data=elk2,lng=~longitude, lat=~latitude,popup=~popup, col="red", radius=1.5, opacity=100)

#add layer control
# Take out ESRI provided tiles
esri <- providers %>%
  purrr::keep(~ grepl('^Esri',.))
#remove a bunch of worthless esri layers
esri[[11]]<-NULL
esri[[9]]<-NULL
esri[[8]]<-NULL
esri[[7]]<-NULL
esri[[6]]<-NULL
esri[[2]]<-NULL
#reorder list so desired list is on top of legend and as primary basemap
esri <- esri[c("Esri.DeLorme", "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.NatGeoWorldMap", "Esri")]
esri %>%
  purrr::walk(function(x) a <<- a %>% addProviderTiles(x,group=x))
a<-a %>%
  addLayersControl(
    baseGroups = names(esri),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend(pal = pal, values = sheepmap$AID, group = "sheepmap", opacity=100, position = "bottomleft")
#a #plot

#a<- a%>% addTitle(text=paste('Updated:', Sys.time()), color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
# 



htmlwidgets::saveWidget(a, file=paste(savedir, 'Last12Hours.html', sep = ""),
                        title="SheepMovement", selfcontained=TRUE)


rm(a)

#attachments. This is going to include the ParturitionMetrics PDF and html files if you want them to send as well.
attach = c(paste0(tempdir, "/Products/Last3Days.html"),paste0(tempdir, "/Products/Last12Hours.html"), paste0(tempdir, "/Products/LatestLocs.kml"))

from = 'katey.huggler@gmx.com'
to = 'katey.huggler@gmx.com'
user = 'katey.huggler@gmx.com'
pass = "Kateyh1957!"
mailR::send.mail(from = from,
                 to = to,
                 subject = "Remaining sheep for Fall 2024 surveys",
                 body = "This email contains locations for sheep that have not been observed during Fall 2024 surveys",
                 authenticate = TRUE,
                 smtp = list(host.name = "mail.gmx.com", port = 587, user.name = user, passwd = pass, tls = T), attach.files = attach)




  
