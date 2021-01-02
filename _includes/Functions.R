# This file includs all functions used for studying the relationship between VEG and PRE in AU

# List of functions----
# f_lib_check(libs) ##libs is a vector of required libraries
#	f_parallel(data=null, fun=null, type="parallel")		for setting up parallel methods
#f_cor(x,y) ## Calculate the correlationship between two vectors ("x" and "y") and return ("r" and "p")
#	f_m2y(data, fun="mean")			for transfering monthly frame data to annual data by using fun="sum" or "mean"
#	f_summary()					for outputing summary infos of all "data frame objects" in memory
#	f_dp(data,seasonal=TRUE,year_start,year_end)	for seasonal or annual changepoint and MK trend analysis
#	f_plot<-function(data,info,annual=FALSE,monthly=FALSE) for monthly or annual grid data plot
#	f_grid2basin(data,type="annual",fun="mean")	#Transfer grid frame data to basin data by fun="mean"


## Function for load multiple libraries----
#' This function allows you to check whether the required library has been installed, otherwise it will be installed and load.
#' @param libs A character vector of names of required libraries.
#' @keywords libraries
#' @export
#' @examples
#' libs<-c("ggplot2","caTools")
#' f_lib_check(libs)
f_lib_check<-function(libs){
  for (lib in libs ){
    if(lib %in% rownames(installed.packages())){

    }else{
      install.packages(lib,repos='http://cran.us.r-project.org')
    }
  }

  a<-lapply(libs, require, character.only = TRUE)
}

# Theme for ggplot ----
theme_ning<-function(size.axis=8,size.title=10,base_family="sans"){
  theme_bw(base_family = base_family) %+replace% 
  theme(axis.title = element_text(face="bold", colour="black", size=size.title),
        axis.text= element_text(angle=0, vjust=0.3, size=size.axis),
        legend.title = element_text(colour="black", size=size.axis, face="bold"),
        legend.text = element_text(colour="black", size = size.axis),
        strip.text.x = element_text(size = size.axis,margin=margin(4, 2, 6, 2), face="bold"),
        strip.text.y = element_text(size = size.axis,margin=margin(4, 2, 4, 6), face="bold",angle=-90),
        legend.key.size=unit(1.2, "lines"),
        legend.box.spacing=unit(1, "mm"),
        strip.background = element_blank(),
        plot.title = element_text(vjust = 2.5,hjust = 0.5,face="bold")
  )
}


#
## Function for clipping raster file by shapefile----
#' @param daRaster A raster object.
#' @param ROI The shapefile.
#' @keywords clipping
#' @export
#' @examples
#' daRaster<-brick("datafilename.tif")
#' ROI<-readOGR("shpfilename.shp")
#' f_crop_roi(daRaster,ROI=ROI)
f_crop_roi<-function(daRaster,ROI,.mask=FALSE,.plot=FALSE){
  if (!compareCRS(ROI,daRaster))  ROI<-spTransform(ROI,crs(daRaster))
  daRaster<-crop(daRaster,ROI)
  if (.mask) daRaster<-mask(daRaster,ROI)
  if(.plot) {plot(daRaster[[1]]);plot(ROI,add=T)}
  return(daRaster)
}


# Function for validation

## RMSE
f_RMSD<-function(obs,sim){
	da<-data.frame(obs=obs,sim=sim)
	da<-na.rm(da)
	sqrt(sum((da$obs-da$sim)^2)/(length(da$obs)-1))

}

f_RMSE<-function(obs,sim){

  sqrt(mean((obs-sim)^2,na.rm=T))
  
}
## NSE
f_NSE<-function(obs,sim){

	1 - sum( (obs - sim)^2 ,na.rm=T) / sum( (obs - mean(obs,na.rm=T))^2,na.rm=T )
}

## Pbias
f_Pbias<-function(obs,sim){

	100 * ( sum( sim - obs,na.rm=T ) / sum( obs,na.rm=T ) )
}


#
## Function for calculating Hamon PET----
#' @param tavg Mean temperature.
#' @param mon The value of month.
#' @param lat The Latitude of the location.
#' @keywords PET, Hamon
#' @export
#' @examples
#' f_hamon_PET(tavg=10.5,mon=1,lat=34.334)
f_hamon_PET <- function(tavg, mon, lat) {
  jdate<-c(1,32,61,92,122,153,183,214,245,275,306,336)[mon]
  ndays<-c(31,28,31,30,31,30,31,31,30,31,30,31)[mon]
  var_theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (jdate - 186)))
  var_pi <- asin(0.39795 * cos(var_theta))
  daylighthr <- 24 - 24/pi * acos((sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(var_pi))/(cos(lat *pi/180) * cos(var_pi)))

  esat <- 0.611 * exp(17.27 * tavg/(237.3 + tavg))

  return(29.8 * daylighthr * (esat/(tavg + 273.2))*ndays)

}
f_hamon_PET_daily <- function(tavg, day, lat) {
  var_theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (day - 186)))
  var_pi <- asin(0.39795 * cos(var_theta))
  daylighthr <- 24 - 24/pi * acos((sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(var_pi))/(cos(lat *pi/180) * cos(var_pi)))

  esat <- 0.611 * exp(17.27 * tavg/(237.3 + tavg))

  return(29.8 * daylighthr * (esat/(tavg + 273.2)))

}

## Calculate stream level ----
#' https://usgs-mrs.cr.usgs.gov/NHDHelp/WebHelp/NHD_Help/Introduction_to_the_NHD/Feature_Attribution/Stream_Levels.htm
#' stream level increase from outlet (1) to the top
#' @param FlowDir The file includes flow direction from a unique ID to the other ID. 
#' @keywords stream_level
#' @export
#' @examples
#' f_stream_level('flowdir.txt')
f_stream_level<-function(FlowDir=NA){

  stream<-read.csv(FlowDir)
  if(sum(c("FROM","TO")%in% names(stream))<2) return ("It should has 'FROM' and 'TO' fields")
  #stream_level<-stream[c("FROM","TO")]
  
  stream$LEVEL<-NA
  
  # store assigned hucs
  index_assigned<-NULL
  
  lev<-1
  for (i in c(1:500)){

	# The first level is defined as no upstream hucs
    if(lev==1){
      index_lev_down<-which(!stream$TO %in% stream$FROM)
      stream$LEVEL[index_lev_down]<-lev
      lev<-lev+1
    
      #Get the assigned index
      index_assigned<-union(index_assigned,index_lev_down)
    }
	
	# get the upstream of these hus
    index_lev_up<-which(stream$TO %in% stream$FROM[index_lev_down])
    
    # Check the loop, which is defined as water flows from a huc to a lower level huc
    loops_index<-intersect(index_lev_up,index_assigned)
    
	if(length(loops_index)) return (paste0("There are loops",stream$FROM[loops_index]))
    
    # assign level to next level
    stream$LEVEL[index_lev_up]<-lev
    index_lev_down<-index_lev_up
    lev<-lev+1
	
    #Get the assigned index
    index_assigned<-union(index_assigned,index_lev_down)
  }

  return(stream)
}

## Routing flow based on stream direction ----
#' return the accumulated flow
#' @param datain The dataframe for calculation. It should has the unique ID and the variable to be accumulated.
#' @param byfield The unique ID, which should be the same as the flow direction file.
#' @param varname The variable to be accumulated
#' @param routpar The stream level that caculated from f_stream_level. 
#' @keywords flow accumulation
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_hrurouting(datain=Flwdata,byfield="HUC8",varname="flow",routpar=routpar)
f_hrurouting<-function(datain,byfield,varname,routpar,mc_cores=1){
  library(parallel)
  
  # get the input variables
  datain["flow"]<-datain[varname]
  datain["HUC"]<-datain[byfield]
  
# function for sum the upstream HUCs
	hru_accm<-function(hru,water,routpar){
		hru<-as.numeric(hru)
		water$flow[water$HUC==hru] +sum(water$flow[water$HUC %in% routpar$FROM[which(routpar$TO==hru)]])
	}

	# Get the maximum LEVEL
	max_level<-max(routpar$LEVEL)
	
	# calculate accumulated flow by Level
	for (level in c(max_level:1)){
		
		# Get the HUC IDs in this Level
		hrus<-unique(routpar$TO[routpar$LEVEL==level])
		
		# calculate accumulated flow for each HUC in this Level
		flowaccu<-lapply(hrus,hru_accm,water=datain,routpar=routpar)
		
		# update the accumulated flow for each HUC in this level
		for (i in c(1:length(hrus))) datain$flow[datain$HUC==hrus[i]]<- flowaccu[[i]]
	}

	# return the accumulated flow only
	return(datain$flow)
}


## Get the upstream HUCs of a HUC ----
#' return HUCIDs of this HUC
#' @param HUCID The unique ID of this HUC, which should be the same as the flow direction file.
#' @param routpar The stream level that caculated from f_stream_level. 
#' @keywords upstream detection
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_upstreamHUCs(HUCID=HUCID,routpar=routpar)
f_upstreamHUCs<-function(HUCID,routpar){
  
  # Get the Stream LEVEL of this HUC
  level_to<-routpar$LEVEL[routpar$TO==HUCID]
  
  upHUCs<-NULL
  To<-HUCID
  if(length(level_to)>0){
	
	# look for upstream HUCs 
    while (length(level_to)>0){
      FROM_HUCs<-routpar$FROM[routpar$TO %in% To]

      upHUCs<-c(upHUCs,FROM_HUCs)

      To<-routpar$FROM[routpar$TO %in% To]
	
	# Update the list of upstream HUCs
      level_to<-routpar$LEVEL[routpar$TO %in% To]

    }

    return(upHUCs)
  }else{
    return(NULL)
  }

}

## Get the downstream HUCs of a HUC ----
#' return HUCIDs of this HUC
#' @param HUCID The unique ID of this HUC, which should be the same as the flow direction file.
#' @param routpar The stream level that caculated from f_stream_level. 
#' @keywords upstream detection
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_downstreamHUCs(HUCID=HUCID,routpar=routpar)
f_downstreamHUCs<-function(HUCID,routpar){
  
  # Get the Stream LEVEL of this HUC
  level_from<-routpar$LEVEL[routpar$FROM==HUCID]
  donwhucids<-NULL
  
  if (length(level_from)>0){
    
    FROM_HUC<-HUCID
    level_from_from<-level_from
	
	# look for upstreams HUCs
    while (length(level_from_from)>0) {
      #print(level_to)
      TO_HUC<-routpar$TO[routpar$FROM==FROM_HUC]
      
	  donwhucids<-c(donwhucids,TO_HUC)
      
	  FROM_HUC<-routpar$TO[routpar$FROM==FROM_HUC]
	  
	  # Update the list of downstream HUCs
	  
      level_from_from<-routpar$LEVEL[routpar$FROM==FROM_HUC]
    }
	
    return(donwhucids)
  }else{
    return(NULL)
  }
  
}


## Calculate the water demand for each HUC ----
#' This water demand means that the accumulated flow is less than water use.
#' @param datain The dataframe for calculation. It should has the unique ID and the variable to be accumulated.
#' @param byfield The unique ID, which should be the same as the flow direction file.
#' @param varname The variable to be accumulated
#' @param routpar The stream level that caculated from f_stream_level. 
#' @keywords flow accumulation
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_WaterDemand(datain=Flwdata,byfield="HUC8",varname="flow",routpar=routpar)
f_WaterDemand<-function(datain,byfield,varname,routpar,mc_cores=1){
  library(parallel)
  
    # get the input variables
	datain["flow"]<-datain[varname]
	datain["HUC"]<-datain[byfield]
	
	# Get the maximum LEVEL
	max_level<-max(routpar$LEVEL)
	
	# Setup water demand variable
    datain$WD<-0
	
	# update the headstream HUCs
		# get the headstream HUCs in flow direction file
		hucs_all<-unique(c(routpar$FROM,routpar$TO))
		
		# Get all HUCs in the headstream (no getting water from other HUCs)(there could be some inland HUCs)
		# The inland HUCs are generally not listed in the flow direction file
		hrus<-c(datain$HUC[!datain$HUC %in% hucs_all],routpar$FROM[which(!routpar$FROM %in% routpar$TO)])
		
		# Get the negative flow HUCs
		hrus<-hrus[hrus %in% datain$HUC[datain$flow<0]]
		
		# update WD by giving this extra water back to all affected HUCs
		if(length(hrus)>0){
		  for(hru in hrus){
		  # update WD based on the water shortage
			datain$WD[datain$HUC==hru]<-abs(datain$flow[datain$HUC==hru])
			datain$flow[datain$HUC==hru]<-0
			
			## update all Downstreams HUCs + WD
			downhurids_from<-f_downstreamHUCs(hru,routpar)
			datain$flow[datain$HUC %in% downhurids_from]<-datain$flow[datain$HUC %in% downhurids_from]+datain$WD[datain$HUC==hru]
	  
		  }
		}

	# update all other HUCs downstream of headstream
		# Update WD by each stream level from upstream to the downstream
		  for (level in c(max_level:1)){
			
			hrus<-unique(routpar$TO[routpar$LEVEL==level])
			hrus<-hrus[hrus %in% datain$HUC[datain$flow<0]]
			
			#print(paste0("There are ",length(hrus)," hrus in level ",level))
			if(length(hrus)==0) next()
			for(hru in hrus){
			  datain$WD[datain$HUC==hru]<-abs(datain$flow[datain$HUC==hru])
			  datain$flow[datain$HUC==hru]<-0
			  
			  ## Downstreams
					downhurids_from<-f_downstreamHUCs(hru,routpar)
					datain$flow[datain$HUC %in% downhurids_from]<-datain$flow[datain$HUC %in% downhurids_from]+datain$WD[datain$HUC==hru]

			}
		  }
	  return(datain)
	}


# get the number of days for each month----

# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' This function allows you to get the number of days for a specific month.
#' @param date A date object.
#' @keywords cats
#' @export
#' @examples
#' date<-as.Date("2001-01-01")
#' numberOfDays(date)
numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

# Zonal a variable for each Hru in dWaSSI-C----
#' @title Zonal a variable for each Hru in dWaSSI-C
#' @description FUNCTION_DESCRIPTION
#' @param classname  a raster of each hru it can be vegetation type or soil type
#' @param daname The input brick data that will be zonaled by hru
#' @param varname The name of the zonaled variable
#' @param shp the zonal boundary
#' @param field the field of the shp boundary that will used for zonal
#' @details This is a function for zonal hru data
#' @examples
#' \dontrun{
#'ha<-hru_lc_zonal(classname = "inputs/Landcover/LUCC_Sun_IGBP.nc",
#'              daname = "inputs/LAI/LAI_BNU.nc",
#'             shp = Basins,
#'             field="Station")
#' }
#' @rdname hru_zonal
#' @export
# this function is used for zonal LAI of each HRU in by a shp file
hru_lc_zonal<-function(classname,daname,shp,fun='mean',field=NULL,plot=T){
  require(raster)
  # read the class and data by their names
  class<-raster(classname)
  da<-brick(daname)

  # crop data based on the input shp
  class<-crop(class,shp)
  da<-crop(da,shp)

  # resample class data based on the input data (Their geo reference could be different)
  class<-resample(class,da[[1]],method='ngb')
  class<-raster::mask(class,shp)
  da<-raster::mask(da,shp)

  shp@data[,field]<-as.character(shp@data[,field])

  # get the number class and their percentage and plot some base map
  print(raster::unique(class))
  if(plot){
    nclass<-raster::unique(class)
    print(table(matrix(class)))
    plot(class)
    plot(da[[1]],add=T,alpha=0.5)
    plot(shp,add=T)
  }

  # funtion for zonal each polygon
  f_zonal<-function(i){
    polygon1<-shp[i,]
    class1<-crop(class,polygon1)
    da1<-crop(da,polygon1)
    class1<-raster::mask(class1,polygon1)
    da1<-raster::mask(da1,polygon1)
    da_zonal1<-zonal(da1, class1, fun)
    namesls<-paste0("Lc_",da_zonal1[,1])
    da_zonal1<-t(da_zonal1[,-1])
    colnames(da_zonal1)<-namesls
    return(da_zonal1)
  }

  # Run sta
  if(length(shp)>1){
    da_zonal<- lapply(c(1:length(shp)), f_zonal)
    names(da_zonal)<-shp@data[,field]
  }else{
    da_zonal<-zonal(da, class, fun)
    namesls<-paste0("Lc_",da_zonal)
    da_zonal<-t(da_zonal[,-1])
    colnames(da_zonal)<-namesls
  }

  return(da_zonal)
}

# Zonal vegetation coverage for each Hru in dWaSSI-C----
#' @title Zonal vegetation coverage for each Hru in dWaSSI-C
#' @description FUNCTION_DESCRIPTION
#' @param classname  a raster of each hru it can be vegetation type or soil type
#' @param varname The name of the zonaled variable
#' @param shp the zonal boundary
#' @param field the field of the shp boundary that will used for zonal
#' @details This is a function for zonal hru data
#' @examples
#' \dontrun{
#'ha<-hru_lc_ratio(classname = "inputs/Landcover/LUCC_Sun_IGBP.nc",
#'             shp = Basins,
#'             field="Station")
#' }
#' @rdname hru_lc_ratio
#' @export
# this function is used for zonal vegetation ratio of each HRU in by a shp file

hru_lc_ratio<-function(classname,shp,field=NULL,mcores=1){
  library(raster)
  class<-raster(classname)
  class<-crop(class,shp)
  class<-mask(class,shp)
  print(table(matrix(class)))

  # zonal_for each polygon
  f_zonal<-function(i){
    polygon1<-shp[i,]
    class1<-crop(class,polygon1)
    class_ratio<-as.data.frame(table(matrix(class1))/sum(table(matrix(class1))))
    names(class_ratio)<-c("Class","Ratio")
    class_ratio$Ratio<-round(class_ratio$Ratio,2)
    class_ratio$Count<-table(matrix(class1))
    class_ratio[field]<-polygon1@data[field]
    return(class_ratio)
  }

  # Run sta
  if(length(shp)>1){
    lc_ratio<- mclapply(c(1:length(shp)), f_zonal,mc.cores=mcores)
    lc_ratio<-do.call(rbind,lc_ratio)
  }else{
    class_ratio<-as.data.frame(table(matrix(class))/sum(table(matrix(class))))
    names(class_ratio)<-c("Class","Ratio")
    class_ratio$Ratio<-round(class_ratio$Ratio,2)
    class_ratio$Count<-table(matrix(class))
    class_ratio[field]<-polygon1@data[field]
    lc_ratio<-class_ratio
  }

  return(lc_ratio)
}


## Trim the anomaly for a variable----
### treat +0.5% and -0.5% value as anomaly
cutAnomalies <- function(x){
  # Cut the anomolies
  toPlot <- c(x)
  toPlot1<- toPlot[!is.na(toPlot)]
  toPlot <-toPlot1
  sortedLow<-sort(toPlot)
  lowCut<-sortedLow[length(sortedLow)*0.005]
  sortedHigh<-sort(toPlot, decreasing=T)
  highCut<-sortedHigh[length(sortedHigh)*0.005]
  x[which(x>highCut)]<-NA
  x[which(x<lowCut)]<-NA
  x
}

f_cut<-function(x){

  low<-quantile(x,0.005,na.rm=T)
  high<-quantile(x,0.995,na.rm=T)
  x[x>high]<-NA
  x[x<low]<-NA
  x
}

## Setup parallel with multiple cors----
#' Setup up parallel using FORK
#' @param name A filename for storing parallel log.
#' @param ncores How many cores will be used for parallelization
#' @keywords Parallel
#' @export
#' @examples
#' f_Parallel_set(name="zeus",ncores=10)
#'
f_Parallel_set<-function(name="zeus",ncores=NA){
  library(pryr)
  library(parallel)
  if (name=="magnus"){
    print("using input cores for processing")
    cl<<-makeCluster(ncores, type="FORK", outfile = paste0("parallel_log",name,".txt"))  # set up parallel
    print(mem_used())#detectCores()-1
    print(detectCores())

  }else{
    print("using max-1 cores for processing")
    cl<<-makeCluster(detectCores()-1, type="FORK", outfile = paste0("parallel_log",name,".txt"))   # set up parallel
    print(mem_used())#
    print(detectCores())
  }
}

## Plot theme is for ggplot----
theme_grid <- function(base_size = 12, base_family = "Times"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      axis.title = element_text(size = 14,face="bold"),
      axis.text = element_text(colour="black", size=12),
      #strip.text = element_text(size=12),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black", size=2),
      panel.background = element_rect(fill = "grey70", colour = "black"),
      strip.background = element_rect(fill = NA),
      legend.position="right",
      legend.background = element_blank()

    )
}

# function for MK trend analysis and change points detection ("trend" and "changepoint" packages)
## ts_in is the input time serie; name is the output pdf name; seasonal is wether for seasonal data; plot is whether plot result; main is the title for plot; Y_name is the title for y_axiel; sig is the sig threhold
f_MK_CP<-function(ts_in,name="",seasonal=F,plot=F,main="",Y_name="Streamflow (mm)",sig=0.05){
  require(trend)
  require(changepoint)
  ##  MK and CP analysis
  if (! seasonal){

    #    print("Annual trend analysis")
    #    print(name)
    # changepoint detect
    cp_mean<-cpt.mean(ts_in)
    means<-cp_mean@param.est$mean
    cp_year<-cp_mean@cpts[1]
    ## get changepoint info if it eixts
    if(length(means)>1){

      cp_year<-time(ts_in)[cp_year]
      change<-round((means[2]-means[1])/means[1],3)*100

    }else{

      means<-NA
      cp_year<-NA
      change<-NA
    }

    # MK test
    mk.t<-mk.test(ts_in)
    sen.res <- sens.slope(ts_in)

    # plot for selected stations
    if(plot & sen.res$b.sen !=0 & mk.t$pvalg< sig){
      #print("plot data")
      pdf(paste("result/pdf/",name,".pdf",sep=""),family="Times",width=10)
      t <- (1:(length(ts_in)))
      s.pred <- sen.res$intercept + sen.res$b.sen * t
      s.pred.ts <- ts(s.pred)
      tsp(s.pred.ts) <- tsp(ts_in)
      plot(cp_mean,xlab="Year",ylab=Y_name,main=main)
      lines(s.pred.ts, lty=2)
      dev.off()
    }

    # return
    return(list(CP_M=means,CP_Pec=change,CP_Y=cp_year,MK_P=round(mk.t$pvalg,3),MK_Slope=round(sen.res$b.sen,2)))

  }else{

    print("Seasonal trend analysis")

    # MK test
    mk.t<-smk.test(ts_in)
    #cmk.t<-csmk.test(ts_in)
    sen.res <- sea.sens.slope(ts_in)
    print(names[i])
    print(sen.res)
    print(mk.t)

  }
}


## Calculate annual mean and anomaly (SAI or pecentage change) of a dataset (in array) in parallel (return a list with ("MEAN", "ANOM"))
f_SAI<-function(data=data,method="SAI",mask=NA,plot=F,anom_ab=5){

  #  mask data base on mask
  if(length(mask)>1){
    for (i in 1:dim(data)[3]){
      data[,,i][mask]<-NA
    }
  }

  # Get the mean and sd for the dataset for each pixel
  data_mean<- parApply(cl,data,c(1,2),mean,na.rm=T)
  data_sd<- parApply(cl,data,c(1,2),sd,na.rm=T)

  print("Start anomaly")
  #  annomly
  anom<-array(0,c(nrow(data),ncol(data),dim(data)[3]))
  if(method=="SAI"){
    for (i in 1:dim(data)[3]){anom[,,i]<-(data[,,i]-data_mean)/data_sd}
  }else{
    for (i in 1:dim(data)[3]){anom[,,i]<-(data[,,i]-data_mean)/data_mean*100}
  }

  # abnormal control
  anom[abs(anom)>anom_ab]<-NA
  anom[is.infinite(anom)]<-NA
  print("anom range")
  print(range(anom,na.rm=T))

  # plot annomly of dataset
  if(plot==T){
    require(ggplot2)
    nrows<-nrow(data_mean)
    ncols<-ncol(data_mean)
    a<-raster(data_mean, xmn=112.9, xmx=154, ymn=-43.74, ymx=-8.98)

    # with raster plot or by dataframe
    if(1){
      for (i in 1:dim(anom)[3]){

        nrows<-nrow(anom)
        ncols<-ncol(anom)
        a<-raster(anom[,,i], xmn=112.9, xmx=154, ymn=-43.74, ymx=-8.98)
        plot(a)
        pdf(paste("result/images/anom_",i+1999,".pdf",sep=""))
        plot(a)
        dev.off()
      }
    }else{
      LAT<-rep(seq(-10, by=-0.05, length.out = nrows),ncols)
      LONG<-rep(seq(112.5, by=0.05, length.out = ncols),each=nrows)
      anom_frame<-data.frame(ID=rep(c(1:(nrows*ncols)),15),YEAR=rep(c(2000:2014),each=(nrows*ncols)),LONG=rep(LONG,15),LAT=rep(LAT,15),ANOM=as.vector(anom))

      gplot<-ggplot(aes(x = LONG, y = LAT), data = anom_frame) +
        geom_raster(aes(LONG, LAT, fill=anom_frame[[5]]))+
        facet_wrap( ~ YEAR, ncol=5)+
        scale_fill_gradient(low = 'red', high = 'green',name=names(anom_frame)[5],na.value = "white") +
        coord_equal()+ #xlim=c(102.4, 104.1),ylim = c(30.72,33.18)
        labs(x="Latitude",y="Longitude")+
        theme_grid()

      ggsave(gplot,file =paste("Ann_",names(anom_frame),".pdf",sep="")[5],dpi = 300)

    }
  }
  # return result
  return(list(MEAN=data_mean,ANOM=anom))
}

## Correlationship between two vectors----
#' Calculate the correlation coefficient between two vectors
#' @param da The input is a vector, which is combination of c(x,y)
#' @param method One of "pearson", "kendall", or "spearman", can be abbreviated.
#' @keywords correlationship
#' @export
#' @examples
#' x<-c(1:10);y<-c(1:10)
#' damerge<-c(x,y)
#' f_cor(da=damerge,method="spearman")
#'
## Calculate the correlation coefficient between two vectors ("x" and "y") and return ("r" and "p")
f_cor<-function(da,method="spearman") {
  y_e<-length(da)
  x_e<-y_e/2
  y_s<-x_e+1
  x<-da[1:x_e];y<-da[y_s:y_e]

  res <- try(cor.test(x,y,method =method), silent=TRUE)

  if (class(res)=="try-error") {
    res <- setNames(c(NA, NA), c("estimate","p.value"))

  }else{
    .res<-unclass(res)[c("estimate","p.value")]
    res<-unlist(.res)
  }
  res
}

## Trend using lm (f_trend)----
#' Trend analysis using linear regression
#' @param da The input vector
#' @keywords trend
#' @export
#' @examples
#' x<-c(1:10)
#' f_trend(data=x)
f_trend<-function(data){
  if(sum(is.na(data))>0 | sum(is.infinite(data))>0){
    c(NA,NA,NA)
  }else{
    .lm<-lm(data~c(1:length(data)))
    a<-summary(.lm)
    #a$coefficients
    c(a$r.squared,a$coefficients[2,4],a$coefficients[2,1])
  }
}


## Monthly Array to annual (f_mon2annual_array)----
#' Convert a monthly array to a annual array
#' @param da The input monthly array
#' @param fun The function for aggregation: "mean", "sum"
#' @keywords trend
#' @export
#' @examples
#' da<-array(c(1:288),c(4,3,24))
#' f_mon2annual_array(da=da,fun="mean")

f_mon2annual_array<-function(da,fun="mean"){
  dims<-dim(da)
  da_ann<-array(0,c(dims[1],dims[2],dims[3]/12))

  a<-1
  b<-1
  for (y in c(1:(dims[3]/12))){

    linshi<-matrix(0,ncol=dims[2],nrow=dims[1])

    for (m in 1:12){

      linshi<-linshi+da[,,a]
      #print(range(linshi,na.rm=T))
      a<-a+1
    }

    if( fun == "mean"){

      da_ann[,,y]<-linshi/12
    }else{
      da_ann[,,y]<-linshi
    }

    b<-b+1
    print(y)
  }
  print(range(da_ann,na.rm=T))
  return(da_ann)
}


##Transfer monthly frame data to annual data by fun="sum" ot "mean"
f_m2y<-function(data, fun="mean"){

  .linshi<-melt(data,id=c(1,2,3))
  .out<-dcast(.linshi, ID+YEAR~variable, get(fun), na.rm=TRUE)
  return(.out)

}

##Transfer grid frame data to basin data by fun="mean"
f_grid2basin<-function(data,type="annual",fun="mean"){
  if(type=="HUC"){

    .linshi<-melt(data,id=c(1,2))
    .out<-dcast(.linshi, BASIN~variable, get(fun), na.rm=TRUE)
    return(.out)
  }else if(type=="annual"){

    .linshi<-melt(data,id=c(1,2,3))
    .out<-dcast(.linshi, BASIN+YEAR~variable, get(fun), na.rm=TRUE)
    return(.out)

  }else if(type=="month"){

    .linshi<-melt(data,id=c(1,2,3,4))
    .out<-dcast(.linshi, BASIN+YEAR+MONTH~variable, get(fun), na.rm=TRUE)
    return(.out)
  }
}
## summary funtion which can output summary information for all data frame objects in memory
f_summary<-function(){
  print("print info for all dataframe objects")
  a<-ls(envir=.GlobalEnv)
  print(a)
  for (i in c(1:length(a))){
    if ( is.data.frame(get(a[i]))){
      print(a[i])
      str(get(a[i]))
      print(summary.data.frame(get(a[i])))
    }
  }

}

## Summary funtion for lists----
### which can output summary information for all list objects in memory
f_list_summary<-function(){
  print("print info for all list objects")
  a<-ls(envir=.GlobalEnv)
  #print(a)
  for (i in c(1:length(a))){
    if (is.list(get(a[i]))){
      print(a[i])
      str(get(a[i]))
      len<-length(get(a[i]))
      for (j in 1:len){
        if (is.data.frame(get(a[i])[[j]])){
          print(names(get(a[i])[j]))
          print(summary.data.frame(get(a[i])[[j]]))
        }
      }
    }
  }
}

####################################################################
## changepoint detection using "bfast" package and MK test using "trend" package
## in seasonal (default) and annual scale
## changepoint detection using "bfast" package
## http://www.sciencedirect.com/science/article/pii/S003442570900265X
######################################################################

f_dp<-function(data,seasonal=TRUE,year_start,year_end){
  require(bfast)
  require(trend)
  require(pryr)
  #.start<-(n-1)*((year_end-year_start+1)*12)+1
  #.end<-n*((year_end-year_start+1)*12)
  #print(n)
  print(mem_used())

  if(seasonal){
    # seasonal changepoint and trend detection
    print("seasonal scale process")
    # IF all data are NA or equal, there would be no trend
    if (!(any(is.na(data)) | isTRUE(all.equal(data, rep(data[1], length(data)))))){
      .linshi<-ts(data,frequency = 12,start = c(year_start,1))

      rdist<-12/length(.linshi)
      # ratio of distance between breaks (time steps) and length of the time series

      fit <- bfast(.linshi,h=rdist, season="harmonic", max.iter=1,breaks=2)
      .out<-fit$output[[1]]
      if ( is.list(.out$bp.Vt)){.trend_change<-.out$bp.Vt$breakpoints}else{.trend_change<-NA}
      if ( is.list(.out$ci.Wt)){.season_change<-.out$ci.Wt[[1]][2]}else{.season_change<-NA}

      ## MK trend detection using "trend" package

      .outmk<-smk.test(.linshi)
      .outslope<-sea.sens.slope(.linshi)

    }else{

      ##---change point detect result

      .trend_change<-NA
      .season_change<-NA

      ## MK test result

      .outmk<-data.frame(tautot=NA,pvalue=NA)
      .outslope<-data.frame(b.sen=NA)

    }
    return(list(CP_trend=.trend_change,CP_season=.season_change,TAU=.outmk$tautot,PMK=.outmk$pvalue,SLOPE=.outslope$b.sen))
  }else{
    # annual changepoint and trend detection
    print("annual scale process")
    # IF all data are NA or equal, there would be no trend
    if (!(any(is.na(data)) | isTRUE(all.equal(data, rep(data[1], length(data)))))){
      .linshi<-ts(data,frequency = 1, start = year_start)

      rdist<-3/length(.linshi)
      #print(.linshi)
      fit <- bfast(.linshi,h=rdist,season = "none", max.iter=1,breaks=2)

      .out<-fit$output[[1]]

      if ( is.list(.out$bp.Vt)){.trend_change<-.out$bp.Vt$breakpoints}else{.trend_change<-NA}

      ## MK trend detection using "trend" package

      .outmk<-mk.test(.linshi)
      .outslope<-sens.slope(.linshi)

    }else{

      ##---change point detect result

      .trend_change<-NA

      ## MK test result

      .outmk<-data.frame(tautot=NA,pvalue=NA)
      .outslope<-data.frame(b.sen=NA)

    }
    return(list(CP_trend=.trend_change,TAU=.outmk$tautot,PMK=.outmk$pvalue,SLOPE=.outslope$b.sen))
  }
}

####################################################################
## this function is used for plot Brick result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_grid_plot<-function(data,info,annual=FALSE,monthly=FALSE,plot=FALSE){
  #info<-c("latmin"=1, "latmax"=1, "longmin"=1, "longmax"=1, "ncols"=1, "nrows"=1, "nbands"=1,"year_start"=1, "year_end"=1,"annual"=0,"monthly"=0)
  ## data is the original data frame and info consists the required infor mation for transfering data from frame to brick
  require(raster)
  require(plyr)
  require(rasterVis)
  require(ggplot2)

  .brick.plot<-function(bricks,name){

    .gplot<-gplot(bricks) + geom_tile(aes(fill = value)) +
      facet_wrap(~ variable) + #,ncol=3
      #scale_fill_gradient(low = 'white', high = 'blue') +
      scale_fill_gradientn(colours=c("blue","green","yellow","red"),name=name,na.value = "grey70")+ #limits=c(500,1000),
      coord_equal()+
      theme_set(theme_bw())

    #ggsave(filename, plot = last_plot(), device = NULL, path = NULL, scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
    #ggsave(gplot,file =paste("trend/","P_Q.pdf",sep=""),width = 10,  units = c("cm"),dpi = 300)
    filename<-paste("images/",name,".pdf",sep="")
    if (plot){print(.gplot)}
    ggsave(.gplot,file=filename,width = 20,  units = c("cm"),dpi=300)

    #print(class(a))
  }

  if(annual){

    if(monthly){
      ## this is for monthly result
      data<-arrange(data,YEAR,MONTH,ID)
      for (var in 4:length(data)){
        nbands=(info["year_end"]-info["year_start"]+1)*12
        .array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
        .brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
        names(.brick)<-paste("Monthly",names(data)[var],paste(rep(c(info["year_start"]:info["year_end"]),each=12),c(1:12)))
        print(paste("Monthly",names(data)[var]))
        .name<-paste("Monthly",names(data)[var],sep="_")
        .brick.plot(.brick,.name)

        #print(.brick)
      }
    }else{
      ## this is for annual result
      data<-arrange(data,YEAR,ID)
      for (var in 3:length(data)){
        nbands=info["year_end"]-info["year_start"]+1
        .array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
        .brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
        names(.brick)<-paste("Annual",names(data)[var],c(info["year_start"]:info["year_end"]))
        #print(.brick)
        print(paste("Annual",names(data)[var]))
        .name<-paste("Annual",names(data)[var],sep="_")
        .brick.plot(.brick,.name)
      }
    }
  }else{
    ## this is for HUC result
    for (var in 2:length(data)){
      nbands=1
      .array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
      .brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
      names(.brick)<-paste("HUC",names(data)[var])
      #print(.brick)
      .name<-paste("HUC",names(data)[var],sep="_")
      print(paste("HUC",names(data)[var]))
      .brick.plot(.brick,.name)

    }
  }

}

####################################################################
## this function is used for plot scatter valid result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_scatter_plot<-function(data,info,annual=FALSE,monthly=FALSE){
  cof<-coef(lm(Q[3:9] ~ Observed[3:9], data = ann_mean_MJ))

  ggplot(ann_mean_MJ, aes(x=Observed[3:9], y=Q[3:9])) +
    geom_point(size=4) +    # Use hollow circles
    geom_abline(intercept = cof[1], slope = cof[2]) +   # Don't add shaded confidence region
    #geom_abline(intercept = 0, slope = 1,linetype="dashed") +
    scale_x_continuous(name="Observed annual runoff (mm)") +
    scale_y_continuous(name="Simulated annual runoff (mm)")+#limits=c(300, 700)
    theme(axis.title.x = element_text(family="Times",face="bold", colour="black", size=12),
          axis.title.y  = element_text(family="Times",face="bold", colour="black", size=12),
          axis.text.x  = element_text(family="Times",face="bold",size=10),
          axis.text.y  = element_text(family="Times",face="bold",size=10))+
    annotate("text",family="Times", x = 500, y = 525, label = "Y = 0.94 * X - 93.8", fontface="italic",size=8)+
    annotate("text",family="Times", x = 500, y = 500, label="R^2 = 0.75\n RMSE = 135 mm", size=6)

  #ylab(expression("today's temperature is "*-5~degree*C))
  #qplot(1,1) + ylab(expression(Temp^2))

  ####-------plot veg water balance box
}

f_box_plot<-function(name1){
  g_plot<-ggplot(data = ann_mean_main_veg, aes(x = VEG, y = ann_mean_main_veg[[a]])) +
    stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) +
    geom_boxplot() + xlab("Vegetation types") +
    ylab(name1) + theme_bw(base_size = 16, base_family = "Times")
  ggsave(g_plot,file =paste("box/",names(ann_mean_MJ),".pdf",sep="")[a],dpi = 300)
  #print(g_plot)
}

## Plot annual mean line----
#' Plot annual mean line
#' @param ... ggplot plots.
#' @param cols Number of columns
#' @examples
#' multiplot(p1,p2,p3,p4,cols=2)
#'
f_line_plot<-function(name1){
  r<-coef(lm(mean_ann_MJ_Y[[a]] ~ YEAR, data = mean_ann_MJ_Y))
  print(r[2])
  l_plot<- ggplot(data = mean_ann_MJ_Y, aes(x = YEAR, y = mean_ann_MJ_Y[[a]])) + geom_point(size=4,shape=21, fill="white") +
    geom_line(size = 1) + scale_x_continuous(breaks=2002:2014)+
    xlab("YEAR") + ylab(name1) + theme_bw(base_size = 14, base_family = "Times") +
    geom_abline(intercept = r[1], slope = r[2])
  ggsave(l_plot,file =paste("line/",names(mean_ann_MJ_Y),".pdf",sep="")[a],dpi = 300)
  print(names(mean_ann_MJ_Y)[a])
}

## Function for ggplot multiple plot----
#' multiplot for ploting multi row and cols ggplot function
#' @param ... ggplot plots.
#' @param cols Number of columns
#' @examples
#' multiplot(p1,p2,p3,p4,cols=2)
#'

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## convert daily data to annual (zoo)----
#' Convert daily data to annual (zoo)----
#' @param x Input zoo variable.
#' @param FUN function for aggregation
#' @keywords aggregation
#' @export
#' @examples
#' daily2annual(x, FUN=mean)
#'
daily2annual<-function (x, FUN, na.rm = TRUE, out.fmt = "%Y-%m-%d")
{
  sfreq <- function(x, min.year=1800) {

    # Checking that 'class(x)'
    valid.class <- c("xts", "zoo")
    if (length(which(!is.na(match(class(x), valid.class )))) <= 0)
      stop("Invalid argument: 'x' must be in c('xts', 'zoo')" )

    out <- periodicity(x)$scale # xts::periodicity

    if (out == "yearly") out <- "annual"

    return(out)

  }

  if (missing(FUN))
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")
  if (sfreq(x) %in% c("annual"))
    stop("Invalid argument: 'x' is already an annual ts !!")
  if (is.na(match(out.fmt, c("%Y", "%Y-%m-%d"))))
    stop("Invalid argument: 'out.fmt' must be in c('%Y', '%Y-%m-%d')")
  dates <- time(x)
  y <- as.numeric(format(dates, "%Y"))
  years <- factor(y, levels = unique(y))
  tmp <- aggregate(x, by = years, FUN, na.rm = na.rm)
  nan.index <- which(is.nan(tmp))
  if (length(nan.index) > 0)
    tmp[nan.index] <- NA
  inf.index <- which(is.infinite(tmp))
  if (length(inf.index) > 0)
    tmp[inf.index] <- NA
  if (out.fmt == "%Y") {
    time(tmp) <- format(time(tmp), "%Y")
  }
  else time(tmp) <- as.Date(paste(time(tmp), "-01-01", sep = ""))
  if (NCOL(tmp) == 1)
    tmp <- zoo(as.numeric(tmp), time(tmp))
  return(tmp)
}

# Convert daily data to monthly (zoo)----
#' Convert daily data to monthly (zoo)----
#' @param x Input zoo variable.
#' @param FUN function for aggregation
#' @keywords aggregation
#' @export
#' @examples
#' daily2monthly(x, FUN=mean)
#'
daily2monthly<-function (x, FUN, na.rm = TRUE, ...)
{
  sfreq <- function(x, min.year=1800) {

    # Checking that 'class(x)'
    valid.class <- c("xts", "zoo")
    if (length(which(!is.na(match(class(x), valid.class )))) <= 0)
      stop("Invalid argument: 'x' must be in c('xts', 'zoo')" )

    out <- periodicity(x)$scale # xts::periodicity

    if (out == "yearly") out <- "annual"

    return(out)

  }
  if (missing(FUN))
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")
  if (sfreq(x) %in% c("monthly", "quarterly", "annual"))
    stop("Invalid argument: 'x' is not a (sub)daily/weekly ts. 'x' is a ",
         sfreq(x), " ts")
  dates <- time(x)
  months <- as.Date(as.yearmon(time(x)))
  tmp <- aggregate(x, by = months, FUN, na.rm = na.rm)
  nan.index <- which(is.nan(tmp))
  if (length(nan.index) > 0)
    tmp[nan.index] <- NA
  inf.index <- which(is.infinite(tmp))
  if (length(inf.index) > 0)
    tmp[inf.index] <- NA
  if (NCOL(tmp) == 1)
    tmp <- zoo(as.numeric(tmp), time(tmp))
  return(tmp)
}


## Convert array to raster----
#' Transfering matrix/array to raster/brick
#' @param data A matrix or array object.
#' @param infonc A filename of a raster object for getting the raster extent info.
#' @keywords cats
#' @export
#' @examples
#' rc<-f_2raster(darray,infonc="/Dataset/backup/CABLE/ET_ann_82_14.nc")
f_2raster<-function(data,infonc=NA){
  #infonc is a target raster
  require(raster)
  if(is.na(infonc)){
    info<-raster("/Dataset/backup/CABLE/ET_ann_82_14.nc")
  }else{
    info<-raster(infonc)
  }

  if(is.matrix(data)){
    .grid<-raster(data,xmn=info@extent@xmin,xmx=info@extent@xmax,ymn=info@extent@ymin,ymx=info@extent@ymax,crs=crs(info))

  }else{
    .grid<-brick(data,xmn=info@extent@xmin,xmx=info@extent@xmax,ymn=info@extent@ymin,ymx=info@extent@ymax,crs=crs(info))
  }
  return(.grid)
}

## Zonal raster or brick file----
#' Zonal raster/brick based on a shapefile
#' @param ncfilename A filename for a "raster*" type object.
#' @param basin A ploygon object.
#' @param fun function for doing zonal (fun="mean"/"sum")
#' @param varname define the variable name
#' @param zonal_field select the field from shapefile file for naming the result
#' @param start the start year for the time series of the input raster
#' @param scale the time step the the input raster
#' @param weight Whether weight polygon for mean
#' @keywords zonal
#' @export
#' @examples
#' sta_shp<-f_sta_shp_nc(ncfilename="/Dataset/backup/CABLE/ET_ann_82_14.nc",
#' basin,fun="mean",varname="ET",zonal_field="Station",start=1982,scale="annual")
#'
f_sta_shp_nc<-function(ncfilename=NULL,da=NULL,basin,fun="mean",varname,zonal_field,start,scale="month",weight=T,plot=T){
  require(dplyr)
  require(raster)
  require(tidyr)
 if(is.null(da)) da<-brick(ncfilename)
  da<-crop(da,basin)
  #NAvalue(da)<- 0
  if(plot) {
    plot(da[[1]],basin)
    plot(basin,add=T)
  }
  if(fun=="mean" | fun=="Mean" | fun=="MEAN"){
    ex <- raster::extract(da, basin, fun=mean, na.rm=TRUE, weights=weight)
  }else{
    ex <- raster::extract(da, basin, fun=sum, na.rm=TRUE)
  }

  if(scale=="month" | scale=="Month" | scale=="MONTH"){
    dates<-seq(as.Date(paste0(start,"-01-01")),by="1 month",length.out = dim(da)[3])
    sta_catchment<-t(ex)%>%
      round(digits = 5)%>%
      as.data.frame()%>%
      mutate(Year=as.integer(format(dates,"%Y")),
             Month=as.integer(format(dates,"%m")))%>%
      melt(id=c("Year","Month"))%>%
      mutate(BasinID=rep(basin[[zonal_field]],each=length(dates)))%>%
      dplyr::select(BasinID,Year,Month,value)
    names(sta_catchment)<-c(zonal_field,"Year","Month",varname)

  }else if(scale=="annual" | scale=="Annual" | scale=="ANNUAL"){
    dates<-seq(as.Date(paste0(start,"-01-01")),by="1 year",length.out = dim(da)[3])
    sta_catchment<-t(ex)%>%
      round(digits = 5)%>%
      as.data.frame()%>%
      mutate(Year=as.integer(format(dates,"%Y")))%>%
      melt(id=c("Year","Month"))%>%
      mutate(BasinID=rep(basin[[zonal_field]],each=length(dates)))%>%
      dplyr::select(BasinID,Year,value)

    names(sta_catchment)<-c(zonal_field,"Year",varname)

  }else{
    dates<-seq(as.Date(paste0(start,"-01-01")),by="1 day",length.out = dim(da)[3])

    sta_catchment<-t(ex)%>%
      round(digits = 5)%>%
      as.data.frame()%>%
      mutate(Year=as.integer(format(dates,"%Y")),
             Month=as.integer(format(dates,"%m")),
             Day=as.integer(format(dates,"%d")))%>%
      melt(id=c("Year","Month"))%>%
      mutate(BasinID=rep(basin[[zonal_field]],each=length(dates)))%>%
      dplyr::select(BasinID,Year,Month,Day,value)
    names(sta_catchment)<-c(zonal_field,"Year","Month","Day",varname)

  }

  sta_catchment
}
## Paste one to one for two vectors, matrixes or arrays----
#' Paste by value one to one for two vectors, matrixes or arrays
#' @param x The first object, which can be vector, matrix or array.
#' @param y The second object, which can be vector, matrix or array.
#' @param sep The separate letter
#' @keywords paste
#' @export
#' @examples
#' x<-c(1,2,3)
#' y<-c("A","B","C")
#' f_paste(x,y,sep="-")
f_paste<-function(x,y,sep=""){
  dimx<-dim(x)
  if(is.null(dimx)){
    sapply(c(1:length(x)),function(a) paste(x[a],y[a],sep=sep))
  }else{
    pas<-sapply(c(1:length(x)),function(a) paste(as.vector(x)[a],as.vector(y)[a],sep=sep))
    if(length(dimx)==2){
      matrix(pas,dimx)
    }else{
      array(pas,dimx)
    }
  }
}

## Zonal catergory raster file based on shp----
#' Zonal catergory raster file based on shp
#' @param ncfilename The input nc file.
#' @param shp The input polygon.
#' @param zonal_field elect the field from shapefile file for naming the result
#' @keywords zonal
#' @export
#' @examples
#' zonal_shp<-f_zonal_shp_nc(ncfilename="/Dataset/backup/CABLE/ET_ann_82_14.nc",
#' basin,,zonal_field="Station")
f_zonal_shp_nc<-function(ncfilename,shp,zonal_field,category=T,mcores=10){
  require(raster)
  require(dplyr)
  # Function for get the ratio of one polygon
  f_ratio<-function(extracts,levs,zonal_field){
    class_ratio<-data.frame("Levels"=levs,"Ratio"=NA)
    a<-extracts %>%
      table()  %>%
      as.data.frame()
    a$Levels<-as.integer(as.character(a$.))
    class_ratio<-merge(class_ratio,a[-1],all.x=T)
    class_ratio$Ratio<-round(class_ratio$Freq/sum(class_ratio$Freq,na.rm = T),2)
    class_ratio
  }
  # read the raster file
  brick_input<-raster(ncfilename)
  extract_shps<-extract(brick_input,shp)

  # Get all categories
  levs<-raster::unique(brick_input)
  levs<-levs[!is.na(levs)]

  # Sta the ratio of each category for all polygons
  .aa<-mclapply(extract_shps,f_ratio,levs=levs,mc.cores=mcores)
  .ab<-do.call(rbind,.aa)
  .ab[zonal_field]<-rep(as.character(shp[[zonal_field]]),each=length(levs))
  .ab$Ratio[is.na(.ab$Ratio)]<-0
  .ab$Freq[is.na(.ab$Freq)]<-0
  .ab
}


## Plot spatial data----
#' Plot spatial data with overlap shpfile
#' @param da The input nc file.
#' @param filename The input polygon.
#' @param colstyle "RdYlGn",
#' @param varnames This is the var names for the plot variable
#' @param pretty Logic
#' @param margin list or logic. This is the Statistic information for the data showed on the margin
#' @param shpname It can be any sting if a shp will be ploted
#' @param cuts This is how many segments of the plot
#' @param ranges This is the min and max of the data
#' @param width This is the width of the pdf in "inch"
#' @param height This is the height of the pdf in "inch"
#' @keywords plot spatial
#' @export
#' @examples
#' # the shpfile has to be loaded before the plot
#' library(maptools)
#' shp<-readShapeLines("data/shp/AU_STATES.shp")
#' f_plot_sp(da = WTD,
#'        filename = "/Dataset/www/images/WUE_AU/tt1.pdf",
#'        width=7,height=7,
#'        cuts = 10,
#'        ranges = c(3,100),
#'        shpname = "shp")
f_plot_sp<-function(da,filename,colstyle="RdYlGn",pretty=T,margin=list(),shpname=NA,varnames=NA,cuts=NA,ranges=NA,width=7,height=7,plot=T){
  library(rasterVis)
  library(RColorBrewer)
  if(!is.na(varnames)) names(da)<-varnames
  if(!is.na(ranges)){
    da[da>=ranges[2]]<-ranges[2]
    da[da<=ranges[1]]<-ranges[1]
    zlim<-ranges
  }else{
    zlim<-c(min(cellStats(da,min)),max(cellStats(da,max)))
  }

  if(!is.na(cuts)) n<-cuts else n<-5

  pdf(filename,width = width,height = height,family = "Times")
  p1<-levelplot(da,par.settings=RdBuTheme(region=brewer.pal(n,colstyle)),margin=margin,pretty=pretty,cuts = n,at=seq(zlim[1],zlim[2],length.out = n+1))

  if(!is.na(shpname))   {
    p1<-p1+ layer(sp.lines(shp, col="gray", lwd=0.5))
  }
  print(p1)
  dev.off()
  if(plot) print(p1)
}

## Write a NetCDF file ----
#' Write a file to netcdf file
#' @param filename The input nc file
#' @param da       The input raster object
#' @param ncfname The output nc file
#' @param varname This is the var names for the data
#' @param dlname The long name of the variable
#' @param varunit The unit of the variable
#' @param start_date Optional. The start date for the input data ("1982-01-01")
#' @param scale Optional. This scale of the time series. ("1 year","1 month", "1 day")
#' @param attrs Optional. Extra attributes for the data. c("name"="value")
#' @param fillvalue Optional. The default fill value for the missing data.
#' @param plot Logical. Whether plot the output nc"
#' @keywords NetCDF write
#' @export
#' @examples
#' path and file name, set dname
#' # add global attributes
#' attr_global<-c("Author"="Ning Liu",
#'               "Email"="LN1267@GMAIL.COM",
#'               "References"="Reference")
#' f_2nc(filename="/mnt/Ning/GPP_Tr_anomaly_ann_Fixed_CO2_82_14.nc",
#' ncfname="/mnt/Ning/tt.nc",
#' varname = "GPP_Tr",
#' varunit= "g C kg-1 H2O"
#' lname = "Anomaly of annual GPP-Tr with Fixed CO2 from CALBE model",
#' start_date = "1982-01-01",
#' scale = "1 month")

f_2nc<-function(filename=NULL,da=NULL,ncfname,varname,start_date=NULL,scale="1 year",attrs=NULL,fillvalue=NULL,dlname=NULL,varunit=NULL,plot=T){
  require(ncdf4)
  require(raster)

  # read original data from a filename or a raster object
  if(!is.null(filename)){
    a<-brick(filename)
    print(paste0("read data from ",filename))
    da<-as.array(a)
  }else{
    if (length(da)<=1) return("Please provide a raster filename or a raster object")
    a<-da
    da<-as.array(a)
  }

  if(is.matrix(da)) da<- t(da) else da<-aperm(da,c(2,1,3))

  bands<-dim(a)[3]

  # define dimensions
  # define time
  if(is.null(start_date) | is.null(scale)) {
    print("There is no time information in the nc file")
    timedim <- ncdim_def("Bands","", c(1:bands))
  }else{
    times<-seq(as.Date(start_date),by=scale,length.out = bands)
    timedim <- ncdim_def("Time","days since 1970-01-01", as.integer(times))
  }

  # define latitude
  xy_dataframe<-as.data.frame(a[[1]],xy=T)
  Latdim <- ncdim_def("Latitude","degrees_north",unique(xy_dataframe$y))
  # define longitude
  Longdim <- ncdim_def("Longitude","degrees_east",unique(xy_dataframe$x))
  if(is.null(dlname)) dlname<-varname
  # define variables
  var_def <- ncvar_def(varname,varunit,list(Longdim,Latdim,timedim),fillvalue,dlname,compression =5)

  # create netCDF file and put arrays
  ncout <- nc_create(ncfname,var_def)

  # put variables
  ncvar_put(ncout,var_def,da)

  # put additional attributes into dimension and data variables
  attr_global<-c("Author"="Ning Liu",
                 "Email"="LN1267@gmail.com")
  if(! is.null(attrs)) attr_global<-c(attr_global,attrs)
  for (var in names(attr_global))  ncatt_put(ncout,0,var,attr_global[[var]])
  history <- paste("Ning Liu", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)
  nc_close(ncout)
  if(plot) {
    a<-raster(ncfname)
    print(plot(a[[1]]))
  }
}
