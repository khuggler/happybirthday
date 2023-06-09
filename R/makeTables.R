#' @title makeTables
#' @description create tables of most recent 12 locations
#' @param gpsdat data.frame of gps data
#' @param id_df data.frame of lookup table
#' @param tempdir temporary directory to save files
#' @return No return value. Function will save tables for rendering in rmarkdown
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[sp]{coordinates}}, \code{\link[sp]{is.projected}}, \code{\link[sp]{spTransform}}
#' @rdname makeTables
#' @export 
#' @importFrom sp coordinates proj4string spTransform
makeTables<-function(gpsdat, id_df, tempdir){
  
  gpsdat<-gpsdat[complete.cases(gpsdat$x),]

  sp::coordinates(gpsdat)<-c('x', 'y')
  sp::proj4string(gpsdat)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  data<-data.frame(gpsdat)
  
  uni<-unique(data$AID)  
  
  all.locs<-data.frame()
  for(k in 1:length(uni)){
      sub<-data[data$AID == uni[k],]
      sub<-sub[complete.cases(sub$tdate),]
      
      sub<-sub[order(sub$tdate, decreasing = T),]
      sub<-sub[1,]
      
      all.locs<-rbind(sub, all.locs)
    }
    
    all.locs<-all.locs[, c('VitFreq', 'Frequency', 'SN', 'Species', 'IdCol', 'AID', 'Sex', 'x', 'y', 'tdate')]
    
    tabby.file<-paste0(tempdir, "/Tables")
    if(dir.exists(tabby.file)){
      fls = dir(tabby.file, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
      unlink(fls, force=TRUE, recursive = TRUE)
    }else{
      dir.create(tabby.file)
    }
    
    saveRDS(all.locs, paste0(tabby.file, "/RecentLocs.RDS"))
    
    
    outs<-data.frame()
    for(i in 1:length(uni)){
      sub<-data[data$AID == uni[i],]
      sub<-sub[complete.cases(sub$tdate),]
      sub<-sub[order(sub$tdate, decreasing = T),]
      
      sub<-sub[1:12,]
      
      sub<-sub[, c('VitFreq', 'Frequency', 'SN', 'Species', 'IdCol', 'AID', 'Sex', 'x', 'y', 'tdate')]
      
      outs<-rbind(outs, sub)
    }
    
    outs$MatchFreq <- gsub('\\.','',as.character(outs$Frequency))
    
    pretty.dat<-paste0(tempdir, "/PrettyData")
    if(dir.exists(pretty.dat)){
      fls = dir(pretty.dat, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
      unlink(fls, force=TRUE, recursive = TRUE)
    }else{
      dir.create(pretty.dat)
    }
    
    saveRDS(outs, paste0(pretty.dat, "/PrettyData.RDS"))
    
  
  }
