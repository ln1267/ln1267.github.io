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

# Theme for ggplot ----
theme_ning<-function(size.axis=8,size.title=10,base_family="sans",legend_position = "top"){
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
        plot.title = element_text(vjust = 2.5,hjust = 0.5,face="bold"),
		legend.position = legend_position
  )
}

theme_Publication <- function(base_size=14, base_family="sans",legend_position = "top") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5, margin = margin(0,0,20,0)),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line.x = element_line(colour="black"),
               axis.line.y = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.box = "vetical",
               legend.key.size= unit(0.5, "cm"),
               #legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold"),
				legend.position = legend_position
       ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
      
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
      
}


funs_nl<-list(

## Function for load multiple libraries----
#' This function allows you to check whether the required library has been installed, otherwise it will be installed and load.
#' @param libs A character vector of names of required libraries.
#' @keywords libraries
#' @export
#' @examples
#' libs<-c("ggplot2","caTools")
#' f_lib_check(libs)
f_lib_check=function(libs){
  for (lib in libs ){
    if(lib %in% rownames(installed.packages())){

    }else{
      install.packages(lib,repos='http://cran.us.r-project.org')
    }
  }

  a<-lapply(libs, require, character.only = TRUE)
},


f_digits=function(x,n=2,format=F) {
  if(format){
    format(round(x,n), big.mark=",")
  }else{
    round(x,n)
  }
  
},

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
f_crop_roi=function(daRaster,ROI,.mask=FALSE,.plot=FALSE){
  if (!compareCRS(ROI,daRaster))  ROI<-spTransform(ROI,crs(daRaster))
  daRaster<-crop(daRaster,ROI)
  if (.mask) daRaster<-mask(daRaster,ROI)
  if(.plot) {plot(daRaster[[1]]);plot(ROI,add=T)}
  return(daRaster)
},


# Function for validation

## R2
  f_R2=function(obs,sim){

	da<-data.frame(obs=obs,sim=sim)
	da<-na.omit(da)
    cor(da$obs,da$sim) ^ 2

  },
## RMSE
f_RMSD=function(obs,sim){
	da<-data.frame(obs=obs,sim=sim)
	da<-na.omit(da)
	sqrt(sum((da$obs-da$sim)^2)/(length(da$obs)-1))

},

f_RMSE=function(obs,sim){

  sqrt(mean((obs-sim)^2,na.rm=T))

},
## NSE
f_NSE=function(obs,sim){

	1 - sum( (obs - sim)^2 ,na.rm=T) / sum( (obs - mean(obs,na.rm=T))^2,na.rm=T )
},

f_KGE = function(Q, X, ...) {
	require(zoo)
  ok <- complete.cases(coredata(X), coredata(Q))
  1 - sqrt(
    (cor(X, Q, use = "complete") - 1)^2 +
      (mean(X[ok]) / mean(Q[ok]) - 1)^2 +
      (sd(X[ok]) / sd(Q[ok]) - 1)^2
  )
},

## Pbias
f_Pbias=function(obs,sim){

	100 * ( sum( sim - obs,na.rm=T ) / sum( obs,na.rm=T ) )
},

#
## Function for checking simulation accuracy----
#' @param obs
#' @keywords
#' @export
#' @examples
f_acc=function(obs,sim){
  #require("hydroGOF")
  
  acc_result<-c(funs_nl$f_R2(obs,sim),funs_nl$f_RMSE(obs,sim),funs_nl$f_Pbias(obs,sim),funs_nl$f_NSE(obs,sim),funs_nl$f_KGE(obs,sim))
  acc_result<-round(acc_result,3)
  names(acc_result)<-c("R2","RMSE","Pbias","NSE","KGE")
  #print(acc_result)
  acc_result
},


## Function for get season
f_Season = function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))
  },


#
## Function for fit NDVI phenology curve----
#' @param da dataframe with Year
#' @keywords
#' @export
#' @examples
f_fit_AG=function(da,Var="NDVI"){
  require(phenofit)
  require(lubridate)
  da_filled<-NULL
  for(yr in unique(da$Year)){

    t <- da$DOY[da$Year==yr]
    y <- unlist(da[da$Year==yr,Var])
	tout <- seq(1, 365, 1)
	if(leap_year(paste0(yr,"-01-01"))) tout <- seq(1, 366, 1)
    r <- FitDL.AG(y, t, tout)
    filled<-data.frame("Date"=as.Date(paste0(yr,tout),"%Y%j"),"Year"=yr,"DOY"=tout,"Filled"=r$zs$iter2)
  da_filled<-rbind(da_filled,filled)
  }
  return(da_filled)
},

#
## Function for calculating Hamon PET----
#' @param tavg Mean temperature.
#' @param mon The value of month.
#' @param lat The Latitude of the location.
#' @keywords PET, Hamon
#' @export
#' @examples
#' f_hamon_PET(tavg=10.5,mon=1,lat=34.334)
f_hamon_PET = function(tavg, mon, lat) {
  jdate<-c(1,32,61,92,122,153,183,214,245,275,306,336)[mon]
  ndays<-c(31,28,31,30,31,30,31,31,30,31,30,31)[mon]
  var_theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (jdate - 186)))
  var_pi <- asin(0.39795 * cos(var_theta))
  daylighthr <- 24 - 24/pi * acos((sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(var_pi))/(cos(lat *pi/180) * cos(var_pi)))

  esat <- 0.611 * exp(17.27 * tavg/(237.3 + tavg))

  return(29.8 * daylighthr * (esat/(tavg + 273.2))*ndays)

},

f_hamon_PET_daily = function(tavg, day, lat) {
  var_theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (day - 186)))
  var_pi <- asin(0.39795 * cos(var_theta))
  daylighthr <- 24 - 24/pi * acos((sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(var_pi))/(cos(lat *pi/180) * cos(var_pi)))

  esat <- 0.611 * exp(17.27 * tavg/(237.3 + tavg))

  return(29.8 * daylighthr * (esat/(tavg + 273.2)))

},

#
## Function for calculating PT PET----
#' @param da A dataframe, which has Rn and Ta
#' @export
#' @examples
#' f_PT(da)
f_PT=function(da,alpha=1.26){
  df<-da%>%
   mutate(esT=0.61121*exp(17.502*Ta/(Ta+240.97)),
           s=17.502*240.97*esT/((Ta+240.97)^2),
           ssy=s/(s+0.066),
          PT=alpha*ssy*Rn*0.0346)
  df$PT
},

#
## Function for calculating PT-JPL PET----
#' @param da A dataframe, which has a least Year, NDVI or LAI, Rn, Ta, RH, VPD(optional)
#' @export
#' @examples
#' f_PT_JPL(da)

f_PT_JPL=function(da,kPAR=0.5, kRn=0.6, TaOpt=25){
  require(dplyr)

  #kPAR is the light extinction coefficient (doi:10.1007/s11707-014-0446-7)
  #kRn is extinction coefficient constant (doi:10.1007/BF02243377.)

  #For mm/mo: x 0.0000004*60*60*12*30
  #W/m2

  # Check whether LAI is exist
  if( "LAI" %in% names(da)){

    df<-da%>%
    mutate(fiPAR=1-exp(-kPAR*LAI),
           NDVI= fiPAR +0.05,
           SAVI=0.45*NDVI+0.132,
           fAPAR=1.16*NDVI-0.14)

  # Otherwise NDVI is used
  }else{
     df<-da%>%
    mutate(SAVI=0.45*NDVI+0.132,
           fAPAR=1.16*NDVI-0.14,
           fiPAR=1.0*NDVI-0.05,
           LAI=-(1/kPAR)*log(1-fiPAR))

  }

  df<-df%>%
        mutate(Rns=Rn*exp(-kRn*LAI),
           Rnc=Rn-Rns,
           ea=RH*(0.61121*exp(17.502*Ta/(Ta+240.97))), # unit is Kpa
           esT=0.61121*exp(17.502*Ta/(Ta+240.97)),
           #VPD=esT-ea,
           s=17.502*240.97*esT/((Ta+240.97)^2),
           ssy=s/(s+0.066),
           phen=SAVI*Rn*Ta/VPD,
           phen=ifelse(phen<0,0,phen))

  # get the optimum condition
  df_max<-df%>%
    dplyr::select(Year,SAVI,phen,fAPAR)%>%
    group_by(Year)%>%
    summarise(phenmax=max(phen,na.rm = T),SAVImax=max(SAVI,na.rm = T),fAPARmax=max(fAPAR,na.rm = T))%>%
    mutate(Topt=TaOpt)

  # Calculate Topt from Phenmax
  if(0){
    for(i in 1:length(df_max$Year)){

      df_max$Topt[i]<-max(df$Ta[df$Year==df_max$Year[i] & df$phen==df_max$phenmax[i]],na.rm = T)

    }
  }

  PT_ET<-df%>%
	left_join(df_max,by="Year")%>%
    group_by(Year)%>%
    mutate(fT=exp(-((Ta-Topt)/Topt)^2),
           fT=ifelse(fT>1,1,fT),
           fM=fAPAR/fAPARmax,
           fM=ifelse(fM>1,1,fM),
           fg=fAPAR/fiPAR,
           fg=ifelse(fg>1,1,fg),
           fSM=RH^VPD,
           fwet=RH^4,
           Ei=fwet*1.26*ssy*Rnc*0.0346,
           Es=(fwet+fSM*(1-fwet))*1.26*ssy*Rns*0.0346,
           Ec=(1-fwet)*fg*fT*fM*1.26*ssy*Rnc*0.0346,
           ETpred=Ei+Es+Ec
        )
  PT_ET
},

#
## Function for calculating INTERCEPTION based on LAI and rainfall----
#' @param ts.prcp Rainfall time series
#' @param ts.lai LAI time series
#' @param lc_code Code of Land use,one of  c("Water","ENF","EBF","DNF","DBF","MF","CSH","OSH","WSA","SA","GRA","WET","CRO","Urban","CROPandNature","Snow","Barren","Unclass")
#' @export
#' @examples
#' da_mothly<-da_daily%>%
#' mutate(Ei_LT=f_Ei(ts.prcp = Rainfall,ts.lai = LAI_NC2))

f_Ei=function(ts.prcp,ts.lai,lc_code="ENF") {

    # Interception Precipitation Evaporation: prcp_real = prcp - Ei
    # @references
    # Van Dijk, A.I.J.M. and Warren, G., 2010. The Australian water resources assessment system. Version 0.5, 3(5). P39
    # TWO INTERCEPTION PARAMETERS
    # S_sls  : specific canopy rainfall storage capacity per unit leaf area (mm)
    # fER0   : Ratio of the mean evaporation rate and the mean rainfall intensity during storms (dimensionless)
    #   set:
    #   13 (Urban and Built-Up)           = 5  (mixed forest)
    #   16 (Barren or Sparsely Vegetated) = 10 (grassland)
   MODIS_LCs<-c("Water","ENF","EBF","DNF","DBF","MF","CSH","OSH","WSA","SA","GRA","WET","CRO","Urban","CROPandNature","Snow","Barren","Unclass")
   S_sls <- c(0.000, 0.123, 0.098, 0.123, 0.069, 0.131,
                                 0.014, 0.014, 0.174, 0.049, 0.114, 0.010,
                                 0.010, 0.131, 0.010, 0.000, 0.114, 0.000)
   fER0 <- c(0.000, 0.055, 0.085, 0.055, 0.010, 0.010,
                              0.010, 0.010, 0.109, 0.055, 0.023, 0.010,
                              0.158, 0.010, 0.158, 0.000, 0.023, 0.000)
   LAIref<-7
   LAI<-ts.lai;P<-ts.prcp
   lc_id<-which(MODIS_LCs==lc_code)

   fveg <- 1 - exp(-LAI/LAIref)
   Sveg <- S_sls[lc_id]*LAI

    fER <- fveg*fER0[lc_id]
    Pwet <- -log(1 - fER0[lc_id]) / fER0[lc_id] * Sveg / fveg
    Ei <- (P < Pwet) * fveg * P + (P >= Pwet) * ( fveg*Pwet + fER*(P - Pwet) )

    Ei
},

#
## Function for calculating INTERCEPTION based on the emperial equation from Throughfall and rainfall----
#' @param da_daily Daily data with PET, Rainfall, LAI
#' @param forest Code of forest types
#' @export
#' @examples
#' @cites  Helvey, J.D., Patric, J.H., 1965. Canopy and litter interception of rainfall by hardwoods of eastern United States. Water Resour. Res. 1, 193–206. https://doi.org/10.1029/WR001i002p00193
#' @cites  Biological effects in the hydrological cycle 1971
#'
#'

f_Ei_pot_USA=function(da_daily,forest="ENF") {

    # Interception Precipitation Evaporation: prcp_real = prcp - Ei
    # @references
    # Helvey, J.D., Patric, J.H., 1965. Canopy and litter interception of rainfall by hardwoods of eastern United States. Water Resour. Res. 1, 193–206. https://doi.org/10.1029/WR001i002p00193
    # 1971, Biological effects in the hydrological cycle
	# emperial Throughfall and Rainfall
	#Table 7 . Summary of Throughfall Plus Stemflow Equations and Computed Throughfall and
    # Slope  : specific canopy rainfall storage capacity per unit leaf area (mm)
    # Interception   :
    #   set:
    #   13 (Urban and Built-Up)           = 5  (mixed forest)
    #   16 (Barren or Sparsely Vegetated) = 10 (grassland)

   	require(dplyr)

	# Calculate the potential interception by forest type using the regression between throughfall and rainfall
	  if(forest=="DBF"){
	  	 da_Ei<-da_daily%>%
				mutate(Ei_pot=ifelse(GW=="GW",P_c*0.06+0.04*25.4,P_c*0.03+0.02*25.4))

	  }else{
	  	da_Ei<-da_daily%>%
				mutate(Ei_pot=P_c*0.12+0.03*25.4) # Loblolly pine
  	  }
	  return(da_Ei)
},

#
## Function for calculating Canopy evaporation based on the PET and Interception----
#' @param da_daily Daily data with PET, Rainfall, LAI
#' @param forest Code of forest types
#' @export
#' @examples
#' @cites  Helvey, J.D., Patric, J.H., 1965. Canopy and litter interception of rainfall by hardwoods of eastern United States. Water Resour. Res. 1, 193–206. https://doi.org/10.1029/WR001i002p00193
#' @cites  Biological effects in the hydrological cycle 1971
#'
#'

f_Evap=function(da_daily) {
	# Correct by rainfall

	da_Ei<-da_daily%>%
	  mutate(Ei_pot=ifelse(Ei_pot>P_c,P_c,Ei_pot))%>% # Interception should less than Rainfall
	  mutate(Ei_pot=ifelse(P_c==0,0,Ei_pot))%>% # If Rainfall ==0 , Interception ==0
	  rowwise() %>%
	  mutate(Ei=NA,P_Ei=P_c-Ei_pot)

	da_Ei[is.na(da_Ei)]<-0

	# Calculate Canopy evaporation with PET
	for(i in 1:nrow(da_Ei)){
	  if(i ==1){
		Ei_left=0
		ei_pot<-da_Ei$Ei_pot[i]
		Ep<-da_Ei$PT[i]*da_Ei$Fc[i]

		da_Ei$Ei[i]=min(Ep,ei_pot)
		Ei_left=max(0,ei_pot-da_Ei$Ei[i])

	  }else{
		ei_pot<-da_Ei$Ei_pot[i]+Ei_left
		Ep<-da_Ei$PT[i]*da_Ei$Fc[i]
		da_Ei$Ei[i]=min(Ep,ei_pot)
		Ei_left=max(0,ei_pot-da_Ei$Ei[i])

	  }

	}

	da_Ei
},


#
## Function for calculating PT PET based on PRISM climate data----
#' @param data climate time series
#' @param constants PT constants
#' @export
#' @examples
#'
#'
f_ET.PT=function (data, constants, ts = "daily", solar = "sunshine hours",
    alpha = 0.23, message = "yes", AdditionalStats = "yes",
    save.csv = "no", ...)
{
    if (is.null(data$Tmax) | is.null(data$Tmin)) {
        stop("Required data missing for 'Tmax' and 'Tmin', or 'Temp'")
    }
    if (is.null(data$va) | is.null(data$vs)) {
        if (is.null(data$RHmax) | is.null(data$RHmin)) {
            stop("Required data missing: need either 'va' and 'vs', or 'RHmax' and 'RHmin' (or 'RH')")
        }
    }
    if (solar == "data" & is.null(data$Rs)) {
        stop("Required data missing for 'Rs'")
    }
    else if (solar == "sunshine hours" & is.null(data$n)) {
        stop("Required data missing for 'n'")
    }
    else if (solar == "cloud" & is.null(data$Cd)) {
        stop("Required data missing for 'Cd'")
    }
    else if (solar == "monthly precipitation" & is.null(data$Precip)) {
        stop("Required data missing for 'Precip'")
    }
    if (is.na(as.numeric(alpha))) {
        stop("Please use a numeric value for the alpha (albedo of evaporative surface)")
    }
    if (!is.na(as.numeric(alpha))) {
        if (as.numeric(alpha) < 0 | as.numeric(alpha) > 1) {
            stop("Please use a value between 0 and 1 for the alpha (albedo of evaporative surface)")
        }
    }
    Ta <- (data$Tmax + data$Tmin)/2
    if (!is.null(data$va) & !is.null(data$vs)) {
        vabar <- data$va
        vas <- data$vs
    }
    else {
        vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax +
            237.3))
        vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin +
            237.3))
        vas <- (vs_Tmax + vs_Tmin)/2
        vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
    }
    P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
    delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta +
        237.3)^2)
    gamma <- 0.00163 * P/constants$lambda
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
    delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
    w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
    N <- 24/pi * w_s
    R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
        sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
        sin(w_s))
    R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
    if (solar == "data") {
        R_s <- data$Rs
    }
    else if (solar != "monthly precipitation" & solar !=
        "cloud") {
        R_s <- (constants$as + constants$bs * (data$n/N)) * R_a
    }
    else {
        R_s <- (0.85 - 0.047 * data$Cd) * R_a
    }
    R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * ((data$Tmax +
        273.2)^4 + (data$Tmin + 273.2)^4)/2 * (1.35 * R_s/R_so -
        0.35)
    R_nsg <- (1 - alpha) * R_s
    R_ng <- R_nsg - R_nl
    E_PT.Daily <- constants$alphaPT * (delta/(delta + gamma) *
        R_ng/constants$lambda - constants$G/constants$lambda)
    ET.Daily <- E_PT.Daily
    ET.Monthly <- aggregate(ET.Daily, list(as.yearmon(data$Date.daily,
        "%m/%y")), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, list(floor(as.numeric(as.yearmon(data$Date.daily,
        "%m/%y")))), FUN = sum)
    ET.MonthlyAve <- ET.AnnualAve <- NULL
    if (AdditionalStats == "yes") {
        for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
            i = mon - min(as.POSIXlt(data$Date.daily)$mon) +
                1
            ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                mon])
        }
        for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
            i = year - min(as.POSIXlt(data$Date.daily)$year) +
                1
            ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                year])
        }
    }
    ET_formulation <- "Priestley-Taylor"
    ET_type <- "Potential ET"
    if (alpha != 0.08) {
        Surface <- paste("user-defined, albedo =", alpha)
    }
    else if (alpha == 0.08) {
        Surface <- paste("water, albedo =", alpha)
    }
    if (solar == "data") {
        message1 <- "Solar radiation data have been used directly for calculating evapotranspiration"
    }
    else if (solar == "sunshine hours") {
        message1 <- "Sunshine hour data have been used for calculating incoming solar radiation"
    }
    else if (solar == "cloud") {
        message1 <- "Cloudiness data have been used for calculating sunshine hour and thus incoming solar radiation"
    }
    else {
        message1 <- "Monthly precipitation data have been used for calculating incoming solar radiation"
    }
    results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
        ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
        ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
        ET_type = ET_type, message1 = message1)
    if (ts == "daily") {
        res_ts <- ET.Daily
    }
    else if (ts == "monthly") {
        res_ts <- ET.Monthly
    }
    else if (ts == "annual") {
        res_ts <- ET.Annual
    }
    if (message == "yes") {
        message(ET_formulation, " ", ET_type)
        message("Evaporative surface: ", Surface)
        message(message1)
        message("Timestep: ", ts)
        message("Units: mm")
        message("Time duration: ", time(res_ts[1]), " to ",
            time(res_ts[length(res_ts)]))
        if (NA %in% res_ts) {
            message(length(res_ts), " ET estimates obtained; ",
                length(which(is.na(res_ts))), " NA output entries due to missing data")
            message("Basic stats (NA excluded)")
            message("Mean: ", round(mean(res_ts, na.rm = T),
                digits = 2))
            message("Max: ", round(max(res_ts, na.rm = T),
                digits = 2))
            message("Min: ", round(min(res_ts, na.rm = T),
                digits = 2))
        }
        else {
            message(length(res_ts), " ET estimates obtained")
            message("Basic stats")
            message("Mean: ", round(mean(res_ts), digits = 2))
            message("Max: ", round(max(res_ts), digits = 2))
            message("Min: ", round(min(res_ts), digits = 2))
        }
    }
    if (save.csv == "yes") {
        for (i in 1:length(results)) {
            namer <- names(results[i])
            write.table(as.character(namer), file = "ET_PriestleyTaylor.csv",
                dec = ".", quote = FALSE, col.names = FALSE,
                row.names = F, append = TRUE, sep = ",")
            write.table(data.frame(get(namer, results)), file = "ET_PriestleyTaylor.csv",
                col.names = F, append = T, sep = ",")
        }
        invisible(results)
    }
    else {
        return(results)
    }
},

#
## Function for calculating Hamon PET based on PRISM climate data----
#' @param data climate time series
#' @param constants PT constants
#' @export
#' @examples
#'
#'
f_ET.Hamon=function (data, ts = "daily", message = "yes",
    AdditionalStats = "yes", save.csv = "no", ...)
{
    if (is.null(data$Tmax) | is.null(data$Tmin)) {
        stop("Required data missing for 'Tmax' and 'Tmin', or 'Temp'")
    }
    if (is.null(data$n)) {
        stop("Required data missing for 'n'")
    }
    Ta <- (data$Tmax + data$Tmin)/2
    vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    ET_Hamon.Daily <- 0.55 * 25.4 * (data$n/12)^2 * (216.7 *
        vas * 10/(Ta + 273.3))/100
    ET.Daily <- ET_Hamon.Daily
    ET.Monthly <- aggregate(ET.Daily, list(as.yearmon(data$Date.daily,
        "%m/%y")), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, list(floor(as.numeric(as.yearmon(data$Date.daily,
        "%m/%y")))), FUN = sum)
    ET.MonthlyAve <- ET.AnnualAve <- NULL
    if (AdditionalStats == "yes") {
        for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
            i = mon - min(as.POSIXlt(data$Date.daily)$mon) +
                1
            ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                mon])
        }
        for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
            i = year - min(as.POSIXlt(data$Date.daily)$year) +
                1
            ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                year])
        }
    }
    ET_formulation <- "Hamon"
    ET_type <- "Potential ET"
    results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
        ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
        ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
        ET_type = ET_type)
    if (ts == "daily") {
        res_ts <- ET.Daily
    }
    else if (ts == "monthly") {
        res_ts <- ET.Monthly
    }
    else if (ts == "annual") {
        res_ts <- ET.Annual
    }
    if (message == "yes") {
        message(ET_formulation, " ", ET_type)
        message("Timestep: ", ts)
        message("Units: mm")
        message("Time duration: ", time(res_ts[1]), " to ",
            time(res_ts[length(res_ts)]))
        if (NA %in% res_ts) {
            message(length(res_ts), " ET estimates obtained; ",
                length(which(is.na(res_ts))), " NA output entries due to missing data")
            message("Basic stats (NA excluded)")
            message("Mean: ", round(mean(res_ts, na.rm = T),
                digits = 2))
            message("Max: ", round(max(res_ts, na.rm = T),
                digits = 2))
            message("Min: ", round(min(res_ts, na.rm = T),
                digits = 2))
        }
        else {
            message(length(res_ts), " ET estimates obtained")
            message("Basic stats")
            message("Mean: ", round(mean(res_ts), digits = 2))
            message("Max: ", round(max(res_ts), digits = 2))
            message("Min: ", round(min(res_ts), digits = 2))
        }
    }
    if (save.csv == "yes") {
        for (i in 1:length(results)) {
            namer <- names(results[i])
            write.table(as.character(namer), file = "ET_Hamon.csv",
                dec = ".", quote = FALSE, col.names = FALSE,
                row.names = F, append = TRUE, sep = ",")
            write.table(data.frame(get(namer, results)), file = "ET_Hamon.csv",
                col.names = F, append = T, sep = ",")
        }
        invisible(results)
    }
    else {
        return(results)
    }
},


## Calculate stream level ----
#' https://usgs-mrs.cr.usgs.gov/NHDHelp/WebHelp/NHD_Help/Introduction_to_the_NHD/Feature_Attribution/Stream_Levels.htm
#' stream level increase from outlet (1) to the top
#' @param FlowDir The file includes flow direction from a unique ID to the other ID.
#' @keywords stream_level
#' @export
#' @examples
#' f_stream_level('flowdir.txt')
f_stream_level=function(FlowDir=NA){

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
},

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
f_hrurouting=function(datain,byfield,varname,routpar,mc_cores=1){
  library(parallel)

  # get the input variables
  datain["flow"]<-datain[varname]
  datain["HUC"]<-datain[byfield]

# function for sum the upstream HUCs
	hru_accm=function(hru,water,routpar){
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
},


## Get the upstream HUCs of a HUC ----
#' return HUCIDs of this HUC
#' @param HUCID The unique ID of this HUC, which should be the same as the flow direction file.
#' @param routpar The stream level that caculated from f_stream_level.
#' @keywords upstream detection
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_upstreamHUCs(HUCID=HUCID,routpar=routpar)
f_upstreamHUCs=function(HUCID,routpar){

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

},

## Get the downstream HUCs of a HUC ----
#' return HUCIDs of this HUC
#' @param HUCID The unique ID of this HUC, which should be the same as the flow direction file.
#' @param routpar The stream level that caculated from f_stream_level.
#' @keywords upstream detection
#' @export
#' @examples
#' routpar<-f_stream_level('flowdir.txt')
#' f_downstreamHUCs(HUCID=HUCID,routpar=routpar)
f_downstreamHUCs=function(HUCID,routpar){

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

},


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
f_WaterDemand=function(datain,byfield,varname,routpar,mc_cores=1){
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
	},


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
numberOfDays = function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
},

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
hru_lc_zonal=function(classname,daname,shp,fun='mean',field=NULL,plot=T){
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
  f_zonal=function(i){
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
},

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

hru_lc_ratio=function(classname,shp,field=NULL,mcores=1){
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
},


## Trim the anomaly for a variable----
### treat +0.5% and -0.5% value as anomaly
cutAnomalies = function(x){
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
},

f_cut=function(x){

  low<-quantile(x,0.005,na.rm=T)
  high<-quantile(x,0.995,na.rm=T)
  x[x>high]<-NA
  x[x<low]<-NA
  x
},

## Setup parallel with multiple cors----
#' Setup up parallel using FORK
#' @param name A filename for storing parallel log.
#' @param ncores How many cores will be used for parallelization
#' @keywords Parallel
#' @export
#' @examples
#' f_Parallel_set(name="zeus",ncores=10)
#'
f_Parallel_set=function(name="zeus",ncores=NA){
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
},

## Plot theme is for ggplot----
theme_grid = function(base_size = 12, base_family = "Times"){
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
},

# function for MK trend analysis and change points detection ("trend" and "changepoint" packages)
## ts_in is the input time serie; name is the output pdf name; seasonal is wether for seasonal data; plot is whether plot result; main is the title for plot; Y_name is the title for y_axiel; sig is the sig threhold
f_MK_CP=function(ts_in,name="",seasonal=F,plot=F,main="",Y_name="Streamflow (mm)",sig=0.05){
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
},


## Calculate annual mean and anomaly (SAI or pecentage change) of a dataset (in array) in parallel (return a list with ("MEAN", "ANOM"))
f_SAI=function(data=data,method="SAI",mask=NA,plot=F,anom_ab=5){

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
},

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
f_cor=function(da,method="spearman") {
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
},

## Trend using lm (f_trend)----
#' Trend analysis using linear regression
#' @param da The input vector
#' @keywords trend
#' @export
#' @examples
#' x<-c(1:10)
#' f_trend(data=x)
f_trend=function(data){
  if(sum(is.na(data))>0 | sum(is.infinite(data))>0){
    c(NA,NA,NA)
  }else{
    .lm<-lm(data~c(1:length(data)))
    a<-summary(.lm)
    #a$coefficients
    c(a$r.squared,a$coefficients[2,4],a$coefficients[2,1])
  }
},


## Monthly Array to annual (f_mon2annual_array)----
#' Convert a monthly array to a annual array
#' @param da The input monthly array
#' @param fun The function for aggregation: "mean", "sum"
#' @keywords trend
#' @export
#' @examples
#' da<-array(c(1:288),c(4,3,24))
#' f_mon2annual_array(da=da,fun="mean")

f_mon2annual_array=function(da,fun="mean"){
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
},


##Transfer monthly frame data to annual data by fun="sum" ot "mean"
f_m2y=function(data, fun="mean"){

  .linshi<-melt(data,id=c(1,2,3))
  .out<-dcast(.linshi, ID+YEAR~variable, get(fun), na.rm=TRUE)
  return(.out)

},

##Transfer grid frame data to basin data by fun="mean"
f_grid2basin=function(data,type="annual",fun="mean"){
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
},

## summary funtion which can output summary information for all data frame objects in memory
f_summary=function(){
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

},

## Summary funtion for lists----
### which can output summary information for all list objects in memory
f_list_summary=function(){
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
},

####################################################################
## changepoint detection using "bfast" package and MK test using "trend" package
## in seasonal (default) and annual scale
## changepoint detection using "bfast" package
## http://www.sciencedirect.com/science/article/pii/S003442570900265X
######################################################################

f_dp=function(data,seasonal=TRUE,year_start,year_end){
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
},

####################################################################
## this function is used for plot Brick result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_grid_plot=function(data,info,annual=FALSE,monthly=FALSE,plot=FALSE){
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

},

####################################################################
## this function is used for plot scatter valid result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_scatter_plot=function(data,info,annual=FALSE,monthly=FALSE){
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
},

f_box_plot=function(name1){
  g_plot<-ggplot(data = ann_mean_main_veg, aes(x = VEG, y = ann_mean_main_veg[[a]])) +
    stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) +
    geom_boxplot() + xlab("Vegetation types") +
    ylab(name1) + theme_bw(base_size = 16, base_family = "Times")
  ggsave(g_plot,file =paste("box/",names(ann_mean_MJ),".pdf",sep="")[a],dpi = 300)
  #print(g_plot)
},

## Plot annual mean line----
#' Plot annual mean line
#' @param ... ggplot plots.
#' @param cols Number of columns
#' @examples
#' multiplot(p1,p2,p3,p4,cols=2)
#'
f_line_plot=function(name1){
  r<-coef(lm(mean_ann_MJ_Y[[a]] ~ YEAR, data = mean_ann_MJ_Y))
  print(r[2])
  l_plot<- ggplot(data = mean_ann_MJ_Y, aes(x = YEAR, y = mean_ann_MJ_Y[[a]])) + geom_point(size=4,shape=21, fill="white") +
    geom_line(size = 1) + scale_x_continuous(breaks=2002:2014)+
    xlab("YEAR") + ylab(name1) + theme_bw(base_size = 14, base_family = "Times") +
    geom_abline(intercept = r[1], slope = r[2])
  ggsave(l_plot,file =paste("line/",names(mean_ann_MJ_Y),".pdf",sep="")[a],dpi = 300)
  print(names(mean_ann_MJ_Y)[a])
},

## Function for ggplot multiple plot----
#' multiplot for ploting multi row and cols ggplot function
#' @param ... ggplot plots.
#' @param cols Number of columns
#' @examples
#' multiplot(p1,p2,p3,p4,cols=2)
#'

multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
},

## convert daily data to annual (zoo)----
#' Convert daily data to annual (zoo)----
#' @param x Input zoo variable.
#' @param FUN function for aggregation
#' @keywords aggregation
#' @export
#' @examples
#' daily2annual(x, FUN=mean)
#'
daily2annual=function (x, FUN, na.rm = TRUE, out.fmt = "%Y-%m-%d")
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
},

# Convert daily data to monthly (zoo)----
#' Convert daily data to monthly (zoo)----
#' @param x Input zoo variable.
#' @param FUN function for aggregation
#' @keywords aggregation
#' @export
#' @examples
#' daily2monthly(x, FUN=mean)
#'
daily2monthly=function (x, FUN, na.rm = TRUE, ...)
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
},


## Convert array to raster----
#' Transfering matrix/array to raster/brick
#' @param data A matrix or array object.
#' @param infonc A filename of a raster object for getting the raster extent info.
#' @keywords cats
#' @export
#' @examples
#' rc<-f_2raster(darray,infonc="/Dataset/backup/CABLE/ET_ann_82_14.nc")
f_2raster=function(data,infonc=NA){
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
},

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
f_sta_shp_nc=function(ncfilename=NULL,da=NULL,basin,fun="mean",varname,zonal_field,start,scale="month",weight=T,plot=T){
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
  }else if (fun=="sd" ){
    ex <- raster::extract(da, basin, fun=sd, na.rm=TRUE)
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
},
#' Extract zonal time-series from raster data using a shapefile
#'
#' @param data Raster data in terra format or a filename pointing to a terra raster data file
#' @param shp SpatialPolygonsDataFrame shapefile containing the polygons for which time-series are required
#' @param fun Function to be used for summarizing the values within each polygon. Default is "mean"
#' @param varname Name of the variable that represents the values extracted from the raster
#' @param zonal.field Name of the field in the shapefile to be used for grouping the time-series data
#' @param start.date Starting date of the time-series. If not provided, the function uses the layer names in the raster data
#' @param ts.by Temporal resolution of the time-series. Default is "1 day"
#' @param weight Logical value indicating whether weights should be used for summarizing values within polygons. Default is FALSE
#'
#' @return A data frame containing the zonal time-series data for each polygon in the shapefile
#'
#' @importFrom terra rast extract nlyr
#' @importFrom dplyr mutate starts_with filter setNames
#' @importFrom tidyr  pivot_longer
#'
#' @examples
#' # load sample data
#' data("global_pattern")
#' # create a shapefile from sample data
#' shp <- vect(global_pattern[1,], "tmp.shp")
#' # extract zonal time-series
#' f_zonal_TS(data = global_pattern, shp = shp, zonal.field = "CLASS", start.date = "2020-01-01")
#'
#' @export

f_zonal_TS<- function(data=NULL, shp=NULL, fun = "mean", varname="Value", zonal.field=NULL, start.date=NULL,
                            ts.by = "1 day", weight = FALSE) {
  library(terra)
  library(dplyr)
  # library(tidyr)
  if(!inherits(data,"SpatRaster") & ! inherits(data,"character")) stop("Either data filename or terra rast data beed be provided!")
  if (inherits(data,"character")) data<-rast(data)
  if(!is.null(start.date)){
    dates <- seq(as.Date(start.date), by = ts.by, length.out = nlyr(data))
    names(data)<-as.character(dates)
  }else{
    
    dates<-paste0("rast_",1: nlyr(data))
    names(data)<-as.character(dates)
  }
  # if(!is.null(shp)) data <- crop(data, shp)
  
  shp_att<-as.data.frame(shp)
  
  ex <- terra::extract(data, shp, fun = fun, na.rm = TRUE, weights = weight) 
  # names(ex)<-c("ID",dates)
  if(!is.null(start.date)) {
    
    da_all<-ex %>% 
      mutate(ID=shp_att[,zonal.field]) %>% 
      tidyr::pivot_longer(cols = starts_with("X"),names_prefix = "X",names_to = "Date",values_to = "Var") %>%
      mutate(Date=as.Date(Date,"%Y.%m.%d")) %>%
      filter(!is.na(Var)) %>% 
      setNames(c(zonal.field,"Date",varname))
    
  }else{

    da_all<-ex %>% 
      mutate(ID=shp_att[,zonal.field]) %>% 
      tidyr::pivot_longer(cols = starts_with("rast_"),names_prefix = "rast_",names_to = "Layer_ID",values_to = "Var") %>%
      filter(!is.na(Var)) %>% 
      setNames(c(zonal.field,"Layer_ID",varname))
 
  }

  return(da_all)
  
},
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
f_paste=function(x,y,sep=""){
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
},

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
f_zonal_shp_nc=function(ncfilename,shp,zonal_field,category=T,mcores=10){
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
},


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
f_plot_sp=function(da,filename,colstyle="RdYlGn",pretty=T,margin=list(),shpname=NA,varnames=NA,cuts=NA,ranges=NA,width=7,height=7,plot=T){
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
},

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

f_2nc=function(filename=NULL,da=NULL,ncfname,varname,start_date=NULL,scale="1 year",attrs=NULL,fillvalue=NULL,dlname=NULL,varunit=NULL,plot=T){
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
},


## Covert 8days brick to monthly brick----
#' @param year The calender year for this 8days brick file.
#' @param r The brick.
#' @keywords 8days2monthly
#' @export
#' @examples
#' da_brick<-brick("/Dataset/backup/CABLE/ET_ann_82_14.nc")
#' f_8days2month(2001,da_brick)

f_8days2month=function(year,r){

  # number of days in that year (leap year or not?)
  ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)

  # how many layers?
  n <- ceiling(ndays/8)
  # day of year for each layer
  nn <- rep(1:n, each=8)[1:ndays]

  # day of year for each month
  m <- as.integer(format(as.Date(1:ndays, origin=paste0(year-1, "-12-31")), "%m"))

  x <- cbind(layer=nn, month=m)
  weights <- table(x[,1], x[,2])

  #apply weight
  s <- list()
  for (i in 1:12) {
    w <- weights[,i]
    x <- r[[which(w > 0)]]
    ww <- w[w > 0] / 8
	for(n in 1:length(ww))  values(x[[n]])<-values(x[[n]])*ww[[n]]

    s[[i]] <- calc(x, mean,na.rm=T)
  }

  s <- stack(s)
  names(s) <- month.abb
  s
},

## Read the zonal climate from GEE ----
#' @param filename The csv file from GEE.
#' @param dataSource The data source ("Terra","Daymet","PRISM").
#' @param dataScale The timestep of for ouput data ("Daily","Monthly").
#' @keywords Climate
#' @export
#' @examples
#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
#' f_readGEEClimate(filename)
f_readGEEClimate=function(filename,dataSource="Terra",dataScale="Monthly"){
  
  require("dplyr")
  require("lubridate")
  
  da<-read.csv(filename)%>%
    mutate(Date=as.Date(as.character(date),"%Y%m%d"))%>%
    dplyr::select(-one_of(c("system.index","date",".geo")))
  
  if(dataSource=="Terra"){
    da<-da%>%
      mutate(Year=year(Date),Month=month(Date))%>%
      dplyr::rename(Ppt_mm=pr,Tmin_C=tmmn,Tmax_C=tmmx,swe_mm=swe,ET0=pet)%>%
      mutate(Tavg_C=(Tmin_C+Tmax_C)/20,ET0=ET0/10)%>%
      mutate(Tmax_C=Tmax_C/10,Tmin_C=Tmin_C/10)
    
  }else if(dataSource=="Daymet"){
    
    da<-da%>%
      mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
      mutate(Tavg_C=(tmax+tmin)/2)%>%
      dplyr::rename(Ppt_mm=prcp,Tmin_C=tmin,Tmax_C=tmax,swe_kgm2=swe,vp_Pa=vp,dayl_s=dayl,srad_Wm2=srad)
    
    if(dataScale=="Monthly"){
      da<-da%>%
        group_by(WS_ID,Year,Month)%>%
        summarise(Ppt_mm=sum(Ppt_mm),swe_kgm2=sum(swe_kgm2),dayl_s=sum(dayl_s),Tavg_C=mean(Tavg_C),Tmax_C=mean(Tmax_C),Tmin_C=mean(Tmin_C),vp_Pa=mean(vp_Pa),srad_Wm2=mean(srad_Wm2))
    }
    
  }else if(dataSource=="PRISM"){
    da<-da%>%
      mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
      dplyr::rename(Tavg_C=tmean,Ppt_mm=ppt,Tmin_C=tmin,Tmax_C=tmax,Tdavg_C=tdmean,vpdmin_hPa=vpdmin,vpdmax_hPa=vpdmax)
    
    if(dataScale=="Monthly"){
      da<-da%>%
        group_by(WS_ID,Year,Month)%>%
        summarise(Ppt_mm=sum(Ppt_mm),Tavg_C=mean(Tavg_C),Tmax_C=mean(Tmax_C),Tmin_C=mean(Tmin_C),Tdavg_C=mean(Tdavg_C),vpdmin_hPa=mean(vpdmin_hPa),vpdmax_hPa=mean(vpdmax_hPa))
    }
    
  }
  
  return(da)
  
},
## Read the zonal 8days LAI from GEE ----
#' @param filename The csv file from GEE.
#' @param dataScale The timestep of for ouput data ("Daily","8days,"Monthly").
#' @keywords LAI
#' @export
#' @examples
#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
#' read_GEE8DayLAI(filename)
read_GEE8DayLAI=function(filename,TimeScale="8days"){
  require(tidyverse)
  require(dplyr)
  require(lubridate)
  da_gee<-read.csv(filename)%>%
    dplyr::select(-.geo,-system.index)%>%
    pivot_longer(cols =starts_with("X20"),names_to="Info",names_prefix = "X",values_to ="LAI")%>%
    mutate(Date=as.Date(as.character(Info),"%Y%j"))%>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
    dplyr::select(-Info)
  
  da_gee

},
## Read the zonal data of S2 from GEE ----
# https://github.com/rfernand387/LEAF-Toolbox/wiki
#' @param filename The csv file from GEE.
#' @param VarName one of // 'Albedo', 'fAPAR','FCOVER','LAI','CWC','CCC'
#' @keywords LAI
#' @export
#' @examples
#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
#' read_GEE_S2(filename)
read_GEE_S2=function(filename,VarName="LAI"){
  require(tidyverse)
  require(dplyr)
  require(lubridate)
  da_gee<-read.csv(filename)%>%
    dplyr::select(-.geo,-system.index)%>%
    pivot_longer(cols =starts_with("X20"),names_to="Info",names_prefix = "X",values_to =VarName)%>%
    mutate(Date=as.Date(substr(as.character(Info),1,8),"%Y%m%d"))%>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
    dplyr::select(-Info)%>%
    filter(!is.na(get(VarName)))
  
  da_gee
  
},

## Extract soil values for each watershed
f_soilinfo=function(soilfname,Watersheds){
    require(raster)
	require(rgdal)
    SOIL<-brick(soilfname)
    Watersheds<-spTransform(Watersheds,crs(SOIL))
    SOIL_catchment<-raster::extract(SOIL,Watersheds,fun=mean,na.rm=T,weights=T)
    # fill NA values
    SOIL_catchment[is.infinite(SOIL_catchment)]<-NA
    SOIL_catchment[is.na(SOIL_catchment)]<-0
    SOIL_catchment<-round(SOIL_catchment,4)

    colnames(SOIL_catchment)<-c("uztwm", "uzfwm" , "uzk", "zperc" , "rexp" , "lztwm" , "lzfsm",
                                "lzfpm", "lzsk" , "lzpk" , "pfree")

    SOIL_catchment<-as.data.frame(cbind(WS_ID=Watersheds$WS_ID,SOIL_catchment))
    return(SOIL_catchment)
},

#' @title convert flow from m3/s to mm per month using date or per day without date
#' @param Q_m3s obeserved flow rate
#' @param area_m2 Dranage area
#' @param date date of the data
f_m3stomm=function(Q_m3s,area_m2,date=NULL){
  require(lubridate)

  if(is.null(date)){

    Q_m3s*1000*24*3600/area_m2

  }else{

    Days<-days_in_month(date)
    Q_m3s*1000*24*Days*3600/area_m2

  }

},


#' @title Sacremento Soil Moisture Accounting Model SAC-SMA
#' @description revised based on sacsmaR package
#' @param par model parameters (11 soil parameters)
#' @param ini.states initial parameters
#' @param prcp daily precipitation data
#' @param pet potential evapotranspiration, in mm
#' @param monthly TRUE/FALSE, whether monthly input time series
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' sacSma_mon(pet, prcp,par)
#' }
#' @rdname sacSim_mon
#' @export
f_SacSma = function(pet, prcp, par, SoilEvp=FALSE, DailyStep=FALSE,ini.states = c(0,0,500,500,500,0)) {

	names(par)<-toupper(names(par))
  if(sum(names(par) %in% c("UZTWM","UZFWM","UZK", "ZPERC",  "REXP", "LZTWM", "LZFSM", "LZFPM",  "LZSK",  "LZPK", "PFREE"))==11){
    uztwm  <-  par["UZTWM"]    # Upper zone tension water capacity [mm]
    uzfwm  <-  par["UZFWM"]    # Upper zone free water capacity [mm]
    lztwm  <-  par["LZTWM"]    # Lower zone tension water capacity [mm]
    lzfpm  <-  par["LZFPM"]    # Lower zone primary free water capacity [mm]
    lzfsm  <-  par["LZFSM"]    # Lower zone supplementary free water capacity [mm]
    uzk    <-  par["UZK"]    # Upper zone free water lateral depletion rate [1/day]
    lzpk   <-  par["LZPK"]    # Lower zone primary free water depletion rate [1/day]
    lzsk   <-  par["LZSK"]    # Lower zone supplementary free water depletion rate [1/day]
    zperc  <-  par["ZPERC"]    # Percolation demand scale parameter [-]
    rexp   <-  par["REXP"]   # Percolation demand shape parameter [-]
    pfree  <-  par["PFREE"]   # Percolating water split parameter (decimal fraction)
    pctim  <- 0 #   par[12]   # Impervious fraction of the watershed area (decimal fraction)
    adimp  <- 0 #  par[13]   # Additional impervious areas (decimal fraction)
    riva   <- 0 #  par[14]   # Riparian vegetation area (decimal fraction)
    side   <- 0 # par[15]   # The ratio of deep recharge to channel base flow [-]
    rserv  <- 0 #par[16]   # Fraction of lower zone free water not transferrable (decimal fraction)
  }else{
    print("Input soil parameter is missing")
  }

  # Initial Storage States (SAC-SMA)
  uztwc <- uztwm # Upper zone tension water storage
  uzfwc <- uzfwm # Upper zone free water storage
  lztwc <- lztwm # Lower zone tension water storage
  lzfsc <- lzfsm # Lower zone supplementary free water storage
  lzfpc <- lzfpm # Upper zone primary free water storage
  adimc <- 0 # Additional impervious area storage

  # RESERVOIR STATE ARRAY INITIALIZATION
  simaet  <- vector(mode = "numeric", length = length(prcp))
  simaet1  <- vector(mode = "numeric", length = length(prcp))
  simaet2  <- vector(mode = "numeric", length = length(prcp))
  simaet3  <- vector(mode = "numeric", length = length(prcp))
  simaet4  <- vector(mode = "numeric", length = length(prcp))
  simaet5  <- vector(mode = "numeric", length = length(prcp))
  simflow   <- vector(mode = "numeric", length = length(prcp))
  base_tot  <- vector(mode = "numeric", length = length(prcp))
  surf_tot  <- vector(mode = "numeric", length = length(prcp))
  interflow_tot  <- vector(mode = "numeric", length = length(prcp))
  uztwc_ts   <- vector(mode = "numeric", length = length(prcp))
  uzfwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lztwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfpc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfsc_ts  <- vector(mode = "numeric", length = length(prcp))

  thres_zero  <- 0.00001 # Threshold to be considered as zero
  parea       <- 1 - adimp - pctim

  for (i in 1:length(prcp)) {

    ### Set input precipitation and potential evapotranspiration
    pr = prcp[i] # This could be effective rainfall, a sum of rainfall and snowmelt
    edmnd = pet[i]

    # Initialize time interval sums
    sbf   <- 0  # Sum of total baseflow(from primary and supplemental storages)
    spbf   <- 0  # Sum of total baseflow(from primary storages)
    ssur  <- 0  # Sum of surface runoff
    sif   <- 0  # Sum of interflow
    sperc <- 0  # Time interval summation of percolation
    sdro  <- 0  # Sum of direct runoff from the additional impervious area
    tet       <-0     # Sum of total AET


    ## Compute for different compnents...
    # ET(1), ET from Upper zone tension water storage
    et1 <- edmnd * uztwc/uztwm
    red <- edmnd - et1  # residual ET demand
    uztwc <- uztwc - et1

    # ET(2), ET from upper zone free water storage
    et2 <- 0
    #print(paste0("I=",i," uztwm= ",uztwm," uztwc= ",uztwc," et1= ", et1, " pr= ",pr," pet= ",edmnd))
    # in case et1 > uztws, no water in the upper tension water storage
    if (uztwc <= 0) {
      et1 <- et1 + uztwc #et1 = uztwc
      uztwc <- 0
      red <- edmnd - et1

      # when upper zone free water content is less than residual ET
      if (uzfwc < red) {

        # all content at upper zone free water zone will be gone as ET
        et2 <- uzfwc
        uzfwc <- 0
        red <- red - et2
        if (uztwc < thres_zero) uztwc <- 0
        if (uzfwc < thres_zero) uzfwc <- 0

        # when upper zone free water content is more than residual ET
      } else {
        et2 <- red  # all residual ET will be gone as ET
        uzfwc <- uzfwc - et2
        red <- 0
      }

      # in case et1 <= uztws, all maximum et (et1) are consumed at uztwc,
      # so no et from uzfwc (et2=0)
    } else {

      # There's possibility that upper zone free water ratio exceeds
      #upper zone tension water ratio. If so, free water is transferred to
      #tension water storage

      if((uztwc / uztwm) < (uzfwc / uzfwm)) {
        uzrat = (uztwc + uzfwc) / (uztwm + uzfwm)
        uztwc = uztwm * uzrat
        uzfwc = uzfwm * uzrat
      }

      if(uztwc < thres_zero) uztwc = 0
      if(uzfwc < thres_zero) uzfwc = 0

    }

    # For soil evapration
    if(SoilEvp){
      et3<-0
      et5<-0
    }else{

    # ET(3), ET from Lower zone tension water storage when residual ET > 0
    et3 <- red * lztwc / (uztwm + lztwm) #residual ET is always bigger than ET(3)
    lztwc <- lztwc - et3

    # if lztwc is less than zero, et3 cannot exceed lztws
    if(lztwc < 0) {
      et3   <- et3 + lztwc  # et3 = lztwc
      lztwc <- 0
    }
    }

    # Water resupply from Lower free water storages to Lower tension water storage
    saved  <- rserv * (lzfpm + lzfsm)
    ratlzt <- lztwc / lztwm
    ratlz  <- (lztwc + lzfpc + lzfsc - saved) / (lztwm + lzfpm + lzfsm - saved)

    # water is first taken from supplementary water storage for resupply
    if (ratlzt < ratlz) {

      del <- (ratlz - ratlzt) * lztwm
      lztwc <- lztwc + del  # Transfer water from lzfss to lztws
      lzfsc <- lzfsc - del

      # if tranfer exceeds lzfsc then remainder comes from lzfps
      if(lzfsc < 0) {
        lzfpc <- lzfpc + lzfsc
        lzfsc <- 0
      }
    }

    if(lztwc < thres_zero) {lztwc <- 0}
    # Comment for additional imprevious ET
    # # ET(5), ET from additional impervious (ADIMP) area
    # # ????? no idea where this come from, I think there's a possibility that et5 can be negative values
    et5   <- et1 + (red + et2) * (adimc - et1 - uztwc) / (uztwm + lztwm)
    adimc <- adimc - et5
    if(adimc < 0) {
      #et5 cannot exceed adimc
      et5 <- et5 + adimc # et5 = adimc
      adimc <- 0
    }
    et5 <- et5 * adimp

    # Time interval available moisture in excess of uztw requirements
    twx <- pr + uztwc - uztwm

    # all moisture held in uztw- no excess
    if(twx < 0) {
      uztwc <- uztwc + pr
      twx <- 0
      # moisture available in excess of uztw storage
    } else {
      uztwc = uztwm
    }
    #
    # for now twx is excess rainfall after filling the uztwc
    #
    adimc <- adimc + pr - twx

    # Compute Impervious Area Runoff
    roimp <- pr * pctim

    # Determine computational time increments for the basic time interval
    ninc <- floor(1 + 0.2*(uzfwc+twx))  # Number of time increments that interval is divided into for further soil-moisture accountng

    dinc <- 1.0 / ninc                    # Length of each increment in days
    pinc <- twx / ninc                    # Amount of available moisture for each increment

    # Compute free water depletion fractions for the time increment
    #(basic depletions are for one day)
    duz   <- 1 - (1 - uzk)^dinc
    dlzp  <- 1 - (1 - lzpk)^dinc
    dlzs  <- 1 - (1 - lzsk)^dinc

    #print(paste0("ninc=", str(ninc)))

    # Start incremental for-loop for the time interval (smaller scale than the input data time series)
    for (n in 1:ninc){

      adsur <- 0 # Amount of surface runoff. This will be updated.
      excess<- 0  # the excess of LZ soil water capacity

      # Compute direct runoff from adimp area
      ratio <- (adimc - uztwc) / lztwm
      if(ratio < 0) ratio <- 0

      # Amount of direct runoff from the additional impervious area
      addro <- pinc*(ratio^2)

      # Compute baseflow and keep track of time interval sum
      # Baseflow from free water primary storage
      bf_p <- lzfpc * dlzp
      lzfpc <- lzfpc - bf_p
      if(lzfpc <= 0.0001) {
        bf_p  <- bf_p + lzfpc
        lzfpc <- 0
      }

      sbf <- sbf + bf_p
      spbf<- sbf + bf_p
      # Baseflow from free water supplemental storage
      bf_s  <- lzfsc * dlzs
      lzfsc <- lzfsc - bf_s
      if (lzfsc <= 0.0001) {
        bf_s <- bf_s + lzfsc
        lzfsc <- 0
      }

      # Total Baseflow from primary and supplemental storages
      sbf <- sbf + bf_s

      # Compute PERCOLATION- if no water available then skip.
      if((pinc + uzfwc) <= 0.01) {
        uzfwc <- uzfwc + pinc
      } else {

        # Limiting drainage rate from the combined saturated lower zone storages
        percm <- lzfpm * dlzp + lzfsm * dlzs
        perc <- percm * uzfwc / uzfwm

        # DEFR is the lower zone moisture deficiency ratio
        defr <- 1.0 - (lztwc + lzfpc + lzfsc)/(lztwm + lzfpm + lzfsm)

        if(defr < 0) {defr <- 0}

        perc <- perc * (1.0 + zperc * (defr^rexp))

        # Note. . . percolation occurs from uzfws before pav is added

        # Percolation rate exceeds uzfws
        if(perc >= uzfwc) {perc <- uzfwc}

        uzfwc <- uzfwc - perc    # Percolation rate is less than uzfws.

        # Check to see if percolation exceeds lower zone deficiency.
        check <- lztwc + lzfpc + lzfsc + perc - lztwm - lzfpm - lzfsm
        if(check > 0) {
          perc <- perc - check
          uzfwc <- uzfwc + check
        }

        # SPERC is the time interval summation of PERC
        sperc <- sperc + perc

        # Compute interflow and keep track of time interval sum. Note that PINC has not yet been added.
        del <- uzfwc * duz # The amount of interflow

        ## Check whether interflow is larger than uzfwc
        if (del > uzfwc) {
          del<-uzfwc
          uzfwc<-0.0
        }else{
          uzfwc <- uzfwc - del
        }

        sif <- sif + del

        # Distribute percolated water into the lower zones. Tension water
        # must be filled first except for the PFREE area. PERCT is
        # percolation to tension water and PERCF is percolation going to
        # free water.

        perct <- perc * (1.0 - pfree)  # Percolation going to the tension water storage
        if((perct + lztwc) <= lztwm) {

          lztwc <- lztwc + perct
          percf <- 0 # Pecolation going to th lower zone free water storages

        } else {

          percf <- lztwc + perct - lztwm
          lztwc <- lztwm

        }

        # Distribute percolation in excess of tension requirements among the free water storages.
        percf <- percf + (perc * pfree)

        if(percf != 0) {

          # Relative size of the primary storage as compared with total lower zone free water storages.
          hpl <- lzfpm / (lzfpm + lzfsm)

          # Relative fullness of each storage.
          ratlp <- lzfpc / lzfpm
          ratls <- lzfsc / lzfsm

          # The fraction going to primary
          fracp <- hpl * 2 * (1 - ratlp) / (2 - ratlp - ratls)

          if(fracp > 1.0) {fracp <- 1.0}

          percp <- percf * fracp # Amount of the excess percolation going to primary
          percs <- percf - percp # Amount of the excess percolation going to supplemental
          lzfsc <- lzfsc + percs

          if(lzfsc > lzfsm) {
            percs <- percs - lzfsc + lzfsm
            lzfsc <- lzfsm
          }

          lzfpc <- lzfpc + percf - percs

          # This is different to Peter's
          #
          # Check to make sure lzfps does not exceed lzfpm
          if(lzfpc >= lzfpm) {
            excess <- lzfpc - lzfpm
            lztwc <- lztwc + excess
            lzfpc <- lzfpm
            if(lztwc >= lztwm) {
              excess <- lztwc - lztwm
              lztwc <- lztwm
            }
          }

        }

        #

        # Distribute PINC between uzfws and surface runoff
        if((pinc+excess) != 0) {

          # check if pinc exceeds uzfwm
          if((pinc + uzfwc+excess) <= uzfwm) {

            uzfwc <- uzfwc + pinc+excess  # no surface runoff
          } else {
            sur <- pinc + uzfwc + excess - uzfwm # Surface runoff
            uzfwc <- uzfwm

            ssur = ssur + (sur * parea)

            # ADSUR is the amount of surface runoff which comes from
            # that portion of adimp which is not currently generating
            # direct runoff. ADDRO/PINC is the fraction of adimp
            # currently generating direct runoff.
            adsur = sur * (1.0 - addro / pinc)
            ssur = ssur + adsur * adimp

          }
        }
      }

      adimc <- adimc + pinc - addro - adsur
      if(adimc > (uztwm + lztwm)) {
        addro = addro + adimc - (uztwm + lztwm)
        adimc = uztwm + lztwm
      }

      # Direct runoff from the additional impervious area
      sdro  = sdro + (addro * adimp)

      if(adimc < thres_zero) {adimc <- 0}

    } # END of incremental for loop

    # Compute sums and adjust runoff amounts by the area over which they are generated.

    # EUSED is the ET from PAREA which is 1.0 - adimp - pctim
    eused <- et1 + et2 + et3
    sif <- sif * parea

    # Separate channel component of baseflow from the non-channel component
    tbf <- sbf * parea   # TBF is the total baseflow
    bfcc <- tbf / (1 + side)    # BFCC is baseflow, channel component

    bfp = (spbf * parea) / (1.0 + side)
    bfs = bfcc - bfp
    if (bfs < 0.) bfs = 0
    bfncc = tbf - bfcc # BFNCC IS BASEFLOW, NON-CHANNEL COMPONENT

    # Ground flow and Surface flow
    base <- bfcc                       # Baseflow and Interflow are considered as Ground inflow to the channel
    surf <- roimp + sdro + ssur + sif  # Surface flow consists of Direct runoff and Surface inflow to the channel

    # ET(4)- ET from riparian vegetation.
    et4 <- (edmnd - eused) * riva  # no effect if riva is set to zero

    # Compute total evapotransporation - TET
    eused <- eused * parea
    tet <- tet+eused + et4 + et5

    # Check that adimc >= uztws
    # This is not sure?
    #if(adimc > uztwc) adimc <- uztwc

    # Total inflow to channel for a timestep
    tot_outflow <- surf + base - et4;

    ### ------- Adjustments to prevent negative flows -------------------------#

    # If total outflow <0 surface and baseflow needs to be updated
    if (tot_outflow < 0) {

      tot_outflow = 0; surf = 0; base = 0;

    } else {

      surf_remainder = surf - et4
      surf <- max(0,surf_remainder)

      if (surf_remainder < 0) { # In this case, base is reduced

        base = base + surf_remainder
        if (base < 0) base = 0
      }
    }


    # Total inflow to channel for a timestep
    simaet[i]  <- tet
    simaet1[i]  <- et1
    simaet2[i]  <- et2
    simaet3[i]  <- et3
    simaet4[i]  <- et4
    simaet5[i]  <- et5
    simflow[i]  <- tot_outflow
    #surf_tot[i] <- surf
    surf_tot[i] <- ssur
    interflow_tot[i] <- sif
    base_tot[i] <- base
    uztwc_ts[i] <- uztwc
    uzfwc_ts[i]  <- uzfwc
    lztwc_ts[i]  <- lztwc
    lzfpc_ts[i]  <- lzfpc
    lzfsc_ts[i]  <- lzfsc
  } #close time-loop

  return(data.frame("aetTot" = simaet,"aetUZT" = simaet1,"aetUZF" = simaet2,"aetLZT" = simaet3,"aet4" = simaet4,"aet5" = simaet5,
                    "WaYldTot" = simflow, "WYSurface" = surf_tot,"WYInter" = interflow_tot, "WYBase" = base_tot,
                    "uztwc"=uztwc_ts,"uzfwc"=uzfwc_ts,
                    "lztwc"=lztwc_ts,"lzfpc"=lzfpc_ts,"lzfsc"=lzfsc_ts))
},

#' @title Mopnthly acremento Soil Moisture Accounting Model SAC-SMA
#' @description revised based on sacsmaR package
#' @param par model parameters (11 soil parameters)
#' @param ini.states initial parameters
#' @param prcp daily precipitation data
#' @param pet potential evapotranspiration, in mm
#' @param monthly TRUE/FALSE, whether monthly input time series
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' sacSma_monthly(pet, prcp,par)
#' }
#' @rdname sacSma_monthly
#' @export
sacSma_monthly =function(pet, prcp,par,inputScale="monthly",DailyStep=FALSE,ini.states = c(0,0,500,500,500,0)) {
  if(sum(names(par) %in% c("UZTWM","UZFWM","UZK", "ZPERC",  "REXP", "LZTWM", "LZFSM", "LZFPM",  "LZSK",  "LZPK", "PFREE"))==11){
    uztwm  <-  par["UZTWM"]    # Upper zone tension water capacity [mm]
    uzfwm  <-  par["UZFWM"]    # Upper zone free water capacity [mm]
    lztwm  <-  par["LZTWM"]    # Lower zone tension water capacity [mm]
    lzfpm  <-  par["LZFPM"]    # Lower zone primary free water capacity [mm]
    lzfsm  <-  par["LZFSM"]    # Lower zone supplementary free water capacity [mm]
    uzk    <-  par["UZK"]    # Upper zone free water lateral depletion rate [1/day]
    lzpk   <-  par["LZPK"]    # Lower zone primary free water depletion rate [1/day]
    lzsk   <-  par["LZSK"]    # Lower zone supplementary free water depletion rate [1/day]
    zperc  <-  par["ZPERC"]    # Percolation demand scale parameter [-]
    rexp   <-  par["REXP"]   # Percolation demand shape parameter [-]
    pfree  <-  par["PFREE"]   # Percolating water split parameter (decimal fraction)
    pctim  <- 0 #   par[12]   # Impervious fraction of the watershed area (decimal fraction)
    adimp  <- 0 #  par[13]   # Additional impervious areas (decimal fraction)
    riva   <- 0 #  par[14]   # Riparian vegetation area (decimal fraction)
    side   <- 0 # par[15]   # The ratio of deep recharge to channel base flow [-]
    rserv  <- 0 #par[16]   # Fraction of lower zone free water not transferrable (decimal fraction)
  }else{
    print("Input soil parameter is missing")
  }

  # Initial Storage States (SAC-SMA)
  uztwc <- uztwm # Upper zone tension water storage
  uzfwc <- uzfwm # Upper zone free water storage
  lztwc <- lztwm # Lower zone tension water storage
  lzfsc <- lzfsm # Lower zone supplementary free water storage
  lzfpc <- lzfpm # Upper zone primary free water storage
  adimc <- 0 # Additional impervious area storage

  #browser()
  # --  CONVERT FLOW RATES IN UNITS OF 1/D TO MONTHLY RATES
  if (inputScale=="monthly" & !DailyStep){
    # uzk<-min(uzk*30,1)
    # lzsk<-min(lzsk*30,1)
    # lzpk<-min(lzpk*30,1)
    #
    # uzk<-uzk*30
    # lzsk<-lzsk*30
    # lzpk<-lzpk*30
  }

  # RESERVOIR STATE ARRAY INITIALIZATION
  simaet  <- vector(mode = "numeric", length = length(prcp))
  simflow   <- vector(mode = "numeric", length = length(prcp))
  base_tot  <- vector(mode = "numeric", length = length(prcp))
  surf_tot  <- vector(mode = "numeric", length = length(prcp))
  interflow_tot  <- vector(mode = "numeric", length = length(prcp))
  uztwc_ts   <- vector(mode = "numeric", length = length(prcp))
  uzfwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lztwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfpc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfsc_ts  <- vector(mode = "numeric", length = length(prcp))

  thres_zero  <- 0.00001 # Threshold to be considered as zero
  parea       <- 1 - adimp - pctim
 #browser()
  for (i in 1:length(prcp)) {

    ### Set input precipitation and potential evapotranspiration
    pr = prcp[i] # This could be effective rainfall, a sum of rainfall and snowmelt
    edmnd = pet[i]

	# Initialize time interval sums
    sbf   <- 0  # Sum of total baseflow(from primary and supplemental storages)
    spbf   <- 0  # Sum of total baseflow(from primary storages)
    ssur  <- 0  # Sum of surface runoff
    sif   <- 0  # Sum of interflow
    sperc <- 0  # Time interval summation of percolation
    sdro  <- 0  # Sum of direct runoff from the additional impervious area
	  tet		<-0		# Sum of total AET

	# Check if it is monthly input and daily step simulation, then covert to daily using the average data
	if(inputScale=="monthly" & DailyStep) {
		day<-30
		pr<-prcp[i]/30
		edmnd<-pet[i]/30
		dt<-1
	}else{
		day<-1
		dt<-30

	}

	# run 30 times for monthly input and 1 time for daily input
	while (day>=1){

    ## Compute for different compnents...
    # ET(1), ET from Upper zone tension water storage
    et1 <- edmnd * uztwc/uztwm
    red <- edmnd - et1  # residual ET demand
    uztwc <- uztwc - et1

    # ET(2), ET from upper zone free water storage
    et2 <- 0
    #print(paste0("I=",i," uztwm= ",uztwm," uztwc= ",uztwc," et1= ", et1, " pr= ",pr," pet= ",edmnd))
    # in case et1 > uztws, no water in the upper tension water storage
    if (uztwc <= 0) {
      et1 <- et1 + uztwc #et1 = uztwc
      uztwc <- 0
      red <- edmnd - et1

      # when upper zone free water content is less than residual ET
      if (uzfwc < red) {

        # all content at upper zone free water zone will be gone as ET
        et2 <- uzfwc
        uzfwc <- 0
        red <- red - et2
        if (uztwc < thres_zero) uztwc <- 0
        if (uzfwc < thres_zero) uzfwc <- 0

        # when upper zone free water content is more than residual ET
      } else {
        et2 <- red  # all residual ET will be gone as ET
        uzfwc <- uzfwc - et2
        red <- 0
      }

      # in case et1 <= uztws, all maximum et (et1) are consumed at uztwc,
      # so no et from uzfwc (et2=0)
    } else {

      # There's possibility that upper zone free water ratio exceeds
      #upper zone tension water ratio. If so, free water is transferred to
      #tension water storage

      if((uztwc / uztwm) < (uzfwc / uzfwm)) {
        uzrat = (uztwc + uzfwc) / (uztwm + uzfwm)
        uztwc = uztwm * uzrat
        uzfwc = uzfwm * uzrat
      }

      if(uztwc < thres_zero) uztwc = 0
      if(uzfwc < thres_zero) uzfwc = 0

    }

    # ET(3), ET from Lower zone tension water storage when residual ET > 0
    et3 <- red * lztwc / (uztwm + lztwm) #residual ET is always bigger than ET(3)
    lztwc <- lztwc - et3

    # if lztwc is less than zero, et3 cannot exceed lztws
    if(lztwc < 0) {
      et3   <- et3 + lztwc  # et3 = lztwc
      lztwc <- 0
    }

    # Water resupply from Lower free water storages to Lower tension water storage
    saved  <- rserv * (lzfpm + lzfsm)
    ratlzt <- lztwc / lztwm
    ratlz  <- (lztwc + lzfpc + lzfsc - saved) / (lztwm + lzfpm + lzfsm - saved)

    # water is first taken from supplementary water storage for resupply
    if (ratlzt < ratlz) {

      del <- (ratlz - ratlzt) * lztwm
      lztwc <- lztwc + del  # Transfer water from lzfss to lztws
      lzfsc <- lzfsc - del

      # if tranfer exceeds lzfsc then remainder comes from lzfps
      if(lzfsc < 0) {
        lzfpc <- lzfpc + lzfsc
        lzfsc <- 0
      }
    }

    if(lztwc < thres_zero) {lztwc <- 0}
    # Comment for additional imprevious ET
    # # ET(5), ET from additional impervious (ADIMP) area
    # # ????? no idea where this come from, I think there's a possibility that et5 can be negative values
    et5   <- et1 + (red + et2) * (adimc - et1 - uztwc) / (uztwm + lztwm)
    adimc <- adimc - et5
    if(adimc < 0) {
      #et5 cannot exceed adimc
      et5 <- et5 + adimc # et5 = adimc
      adimc <- 0
    }
    et5 <- et5 * adimp

    # Time interval available moisture in excess of uztw requirements
    twx <- pr + uztwc - uztwm

    # all moisture held in uztw- no excess
    if(twx < 0) {
      uztwc <- uztwc + pr
      twx <- 0
      # moisture available in excess of uztw storage
    } else {
      uztwc = uztwm
    }
    #
    # for now twx is excess rainfall after filling the uztwc
    #
    adimc <- adimc + pr - twx

    # Compute Impervious Area Runoff
    roimp <- pr * pctim

    # Determine computational time increments for the basic time interval
    ninc <- floor(1.0 + 0.2*(uzfwc+twx)/1)  # Number of time increments that interval is divided into for further soil-moisture accountng

    dinc <- 1.0 / ninc*dt                    # Length of each increment in days
    pinc <- twx / ninc                    # Amount of available moisture for each increment

    # Compute free water depletion fractions for the time increment
    #(basic depletions are for one day)
     duz   <- 1 - (1 - uzk)^dinc
     dlzp  <- 1 - (1 - lzpk)^dinc
     dlzs  <- 1 - (1 - lzsk)^dinc

    #browser()
    # This is the version of Peter's
  #  duz   <- uzk*dinc*30
   # dlzp  <- lzpk*dinc*30
   # dlzs  <- lzsk*dinc*30

    #print(paste0("ninc=", str(ninc)))

    # Start incremental for-loop for the time interval (smaller scale than the input data time series)
    for (n in 1:ninc){

      adsur <- 0 # Amount of surface runoff. This will be updated.
      excess<- 0  # the excess of LZ soil water capacity

      # Compute direct runoff from adimp area
      ratio <- (adimc - uztwc) / lztwm
      if(ratio < 0) ratio <- 0

      # Amount of direct runoff from the additional impervious area
      addro <- pinc*(ratio^2)

      # Compute baseflow and keep track of time interval sum
      # Baseflow from free water primary storage
      bf_p <- lzfpc * dlzp
      lzfpc <- lzfpc - bf_p
      if(lzfpc <= 0.0001) {
        bf_p  <- bf_p + lzfpc
        lzfpc <- 0
      }

      sbf <- sbf + bf_p
      spbf<- sbf + bf_p
      # Baseflow from free water supplemental storage
      bf_s  <- lzfsc * dlzs
      lzfsc <- lzfsc - bf_s
      if (lzfsc <= 0.0001) {
        bf_s <- bf_s + lzfsc
        lzfsc <- 0
      }

      # Total Baseflow from primary and supplemental storages
      sbf <- sbf + bf_s

      # Compute PERCOLATION- if no water available then skip.
      if((pinc + uzfwc) <= 0.01) {
        uzfwc <- uzfwc + pinc
      } else {

        # Limiting drainage rate from the combined saturated lower zone storages
        percm <- lzfpm * dlzp + lzfsm * dlzs
        perc <- percm * uzfwc / uzfwm

        # DEFR is the lower zone moisture deficiency ratio
        defr <- 1.0 - (lztwc + lzfpc + lzfsc)/(lztwm + lzfpm + lzfsm)

        if(defr < 0) {defr <- 0}

        perc <- perc * (1.0 + zperc * (defr^rexp))

        # Note. . . percolation occurs from uzfws before pav is added

        # Percolation rate exceeds uzfws
        if(perc >= uzfwc) {perc <- uzfwc}

        uzfwc <- uzfwc - perc    # Percolation rate is less than uzfws.

        # Check to see if percolation exceeds lower zone deficiency.
        check <- lztwc + lzfpc + lzfsc + perc - lztwm - lzfpm - lzfsm
        if(check > 0) {
          perc <- perc - check
          uzfwc <- uzfwc + check
        }

        # SPERC is the time interval summation of PERC
        sperc <- sperc + perc

        # Compute interflow and keep track of time interval sum. Note that PINC has not yet been added.
        del <- uzfwc * duz # The amount of interflow

        ## Check whether interflow is larger than uzfwc
        if (del > uzfwc) {
          del<-uzfwc
          uzfwc<-0.0
        }else{
          uzfwc <- uzfwc - del
        }

        sif <- sif + del

        # Distribute percolated water into the lower zones. Tension water
        # must be filled first except for the PFREE area. PERCT is
        # percolation to tension water and PERCF is percolation going to
        # free water.

        perct <- perc * (1.0 - pfree)  # Percolation going to the tension water storage
        if((perct + lztwc) <= lztwm) {

          lztwc <- lztwc + perct
          percf <- 0 # Pecolation going to th lower zone free water storages

        } else {

          percf <- lztwc + perct - lztwm
          lztwc <- lztwm

        }

        # Distribute percolation in excess of tension requirements among the free water storages.
        percf <- percf + (perc * pfree)

        if(percf != 0) {

          # Relative size of the primary storage as compared with total lower zone free water storages.
          hpl <- lzfpm / (lzfpm + lzfsm)

          # Relative fullness of each storage.
          ratlp <- lzfpc / lzfpm
          ratls <- lzfsc / lzfsm

          # The fraction going to primary
          fracp <- hpl * 2 * (1 - ratlp) / (2 - ratlp - ratls)

          if(fracp > 1.0) {fracp <- 1.0}

          percp <- percf * fracp # Amount of the excess percolation going to primary
          percs <- percf - percp # Amount of the excess percolation going to supplemental
          lzfsc <- lzfsc + percs


          if(lzfsc > lzfsm) {
            percs <- percs - lzfsc + lzfsm
            lzfsc <- lzfsm
          }

          lzfpc <- lzfpc + percf - percs

          # This is different to Peter's
          #
          # Check to make sure lzfps does not exceed lzfpm
          if(lzfpc >= lzfpm) {
            excess <- lzfpc - lzfpm
            lztwc <- lztwc + excess
            lzfpc <- lzfpm
            if(lztwc >= lztwm) {
              excess <- lztwc - lztwm
              lztwc <- lztwm
            }
          }

        }

        #

        # Distribute PINC between uzfws and surface runoff
        if((pinc+excess) != 0) {

          # check if pinc exceeds uzfwm
          if((pinc + uzfwc+excess) <= uzfwm) {

            uzfwc <- uzfwc + pinc+excess  # no surface runoff
          } else {
            sur <- pinc + uzfwc + excess - uzfwm # Surface runoff
            uzfwc <- uzfwm

            ssur = ssur + (sur * parea)

            # ADSUR is the amount of surface runoff which comes from
            # that portion of adimp which is not currently generating
            # direct runoff. ADDRO/PINC is the fraction of adimp
            # currently generating direct runoff.
            adsur = sur * (1.0 - addro / pinc)
            ssur = ssur + adsur * adimp

          }
        }
      }

      adimc <- adimc + pinc - addro - adsur
      if(adimc > (uztwm + lztwm)) {
        addro = addro + adimc - (uztwm + lztwm)
        adimc = uztwm + lztwm
      }

      # Direct runoff from the additional impervious area
      sdro  = sdro + (addro * adimp)

      if(adimc < thres_zero) {adimc <- 0}

    } # END of incremental for loop

    # Compute sums and adjust runoff amounts by the area over which they are generated.

    # EUSED is the ET from PAREA which is 1.0 - adimp - pctim
    eused <- et1 + et2 + et3
    sif <- sif * parea

    # Separate channel component of baseflow from the non-channel component
    tbf <- sbf * parea   # TBF is the total baseflow
    bfcc <- tbf / (1 + side)    # BFCC is baseflow, channel component

    bfp = (spbf * parea) / (1.0 + side)
    bfs = bfcc - bfp
    if (bfs < 0.) bfs = 0
    bfncc = tbf - bfcc # BFNCC IS BASEFLOW, NON-CHANNEL COMPONENT

    # Ground flow and Surface flow
    base <- bfcc                       # Baseflow and Interflow are considered as Ground inflow to the channel
    surf <- roimp + sdro + ssur + sif  # Surface flow consists of Direct runoff and Surface inflow to the channel

    # ET(4)- ET from riparian vegetation.
    et4 <- (edmnd - eused) * riva  # no effect if riva is set to zero

    # Compute total evapotransporation - TET
    eused <- eused * parea
    tet <- tet+eused + et4 + et5

    # Check that adimc >= uztws
    # This is not sure?
    #if(adimc > uztwc) adimc <- uztwc

    # Total inflow to channel for a timestep
    tot_outflow <- surf + base - et4;

    ### ------- Adjustments to prevent negative flows -------------------------#

    # If total outflow <0 surface and baseflow needs to be updated
    if (tot_outflow < 0) {

      tot_outflow = 0; surf = 0; base = 0;

    } else {

      surf_remainder = surf - et4
      surf <- max(0,surf_remainder)

      if (surf_remainder < 0) { # In this case, base is reduced

        base = base + surf_remainder
        if (base < 0) base = 0
      }
    }

	day<-day-1
	} # end of month while loop

    # Total inflow to channel for a timestep
    simaet[i]  <- tet
    simflow[i]  <- tot_outflow
    #surf_tot[i] <- surf
    surf_tot[i] <- ssur
    interflow_tot[i] <- sif
    base_tot[i] <- base
    uztwc_ts[i] <- uztwc
    uzfwc_ts[i]  <- uzfwc
    lztwc_ts[i]  <- lztwc
    lzfpc_ts[i]  <- lzfpc
    lzfsc_ts[i]  <- lzfsc
  } #close time-loop

  return(data.frame("aetTot" = simaet,"WaYldTot" = simflow, "WYSurface" = surf_tot,"WYInter" = interflow_tot, "WYBase" = base_tot,
                    "uztwc"=uztwc_ts,"uzfwc"=uzfwc_ts,
                    "lztwc"=lztwc_ts,"lzfpc"=lzfpc_ts,"lzfsc"=lzfsc_ts))
},

Predict_monthly=function(fitModel,newdata,forestType="DBF"){
  
  HydroTestData <- as.zooreg(zoo(newdata[c("P","E","Q")], order.by = newdata$Date))

  Output_all<-predict(fitModel,newdata=HydroTestData,return_state =T)[,c("U","AET","ssur","sif","bfcc","uztwc","uzfwc" ,"lztwc" ,"lzfsc" ,"lzfpc")]
  Output_all$WYBase<-Output_all$bfcc # Baseflow
  Output_all$WYInter<-Output_all$sif	# Interflow
  Output_all$WYSurface<-Output_all$ssur # Surface flow

  names(Output_all)[1:2]<-c("Q_sim","ET")

  result_month<-cbind(newdata,Output_all)%>%
    mutate(Date=make_date(Year,Month,"01"))%>%
    filter(Year>=newdata$Year[1]+1)
  
  if(forestType=="DBF"){
    result_month<-result_month%>%
      mutate(GPP=ET*3.2,GPP_SD=ET*1.26) # DBF
  }else{
    result_month<-result_month%>%
      mutate(GPP=ET*2.46,GPP_SD=ET*0.96) # ENF
  }

	result_ann<-result_month%>%
	  mutate(Year=year(Date))%>%
	  group_by(Year)%>%
	  summarise(across(c("Rainfall","PT","PET_Hamon","PET","ET","Q","Q_sim","GPP","GPP_SD"),.fns = sum,na.rm=T))%>%
	  mutate(Pbias=(Q_sim-Q)/Q*100)

# Validation parameters
	val_par_monthly<-funs_nl$f_acc(result_month$Q,result_month$Q_sim)

	val_par_annual<-funs_nl$f_acc(result_ann$Q,result_ann$Q_sim)

	# Plots
	p2<-result_month%>%
	  ggplot(aes(x=Q,y=Q_sim))+geom_point()+geom_smooth(method = "lm")+coord_equal()+labs(x="Q Observed",y="Q Simulated")+theme_bw()

	p3<-result_month%>%
	  ggplot(aes(x=Date))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+scale_x_date(date_breaks ="1 year",date_labels = "%Y")+labs(x="Date",y="Flow (mm)")+theme_bw()

	p4<-result_ann%>%
	  ggplot(aes(x=Year))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+labs(x="Year",y="Flow (mm)")+scale_x_continuous(breaks = c(seq(1980,2022,1)))+theme_bw()

	Monthly_avg<-result_month%>%
	  ungroup()%>%
	  dplyr::select(-Date,-Year)%>%
	  group_by(Month)%>%
	  summarise(across(.fns = mean))
	
	Annual_avg<-result_ann%>%
	  select(-Year)%>%
	  summarise(across(.fns = mean))
	  	  
	return(list(monthly=result_month,annual=result_ann,
		  monthly_avg=Monthly_avg,annual_avg=Annual_avg,
		  Accuarcy=list(val_par_monthly=val_par_monthly,val_par_annual=val_par_annual),
		  Figs=list(monthly=p2,monthly_lines=p3,annual=p4)))
	  },

RunMonthlyWaSSI=function(data_in,soil_pars,inputScale="monthly",DailyStep=FALSE,forestType="DBF"){
  
  names(soil_pars)<-toupper(names(soil_pars))
  
  result_month<-cbind(data_in,funs_nl$sacSma_monthly(par = soil_pars,inputScale=inputScale,DailyStep=DailyStep,pet = data_in$PET, prcp = data_in$Rainfall))%>%
    mutate(Q_sim=WaYldTot,ET=aetTot)%>%
    mutate(Date=make_date(Year,Month,"01"))%>%
    filter(Year>=data_in$Year[1]+1)
  
  if(forestType=="DBF"){
    result_month<-result_month%>%
      mutate(GPP=ET*3.2,GPP_SD=ET*1.26) # DBF
  }else{
    result_month<-result_month%>%
      mutate(GPP=ET*2.46,GPP_SD=ET*0.96) # ENF
  }

	result_ann<-result_month%>%
	  mutate(Year=year(Date))%>%
	  group_by(Year)%>%
	  summarise(across(c("Rainfall","PT","PET_Hamon","PET","ET","Q","Q_sim","GPP","GPP_SD"),.fns = sum,na.rm=T))%>%
	  mutate(Pbias=(Q_sim-Q)/Q*100)

# Validation parameters
	val_par_monthly<-funs_nl$f_acc(result_month$Q,result_month$Q_sim)

	val_par_annual<-funs_nl$f_acc(result_ann$Q,result_ann$Q_sim)

	# Plots
	p2<-result_month%>%
	  ggplot(aes(x=Q,y=Q_sim))+geom_point()+geom_smooth(method = "lm")+coord_equal()+labs(x="Q Observed",y="Q Simulated")+theme_bw()

	p3<-result_month%>%
	  ggplot(aes(x=Date))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+scale_x_date(date_breaks ="1 year",date_labels = "%Y")+labs(x="Date",y="Flow (mm)")+theme_bw()

	p4<-result_ann%>%
	  ggplot(aes(x=Year))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+labs(x="Year",y="Flow (mm)")+scale_x_continuous(breaks = c(seq(1980,2022,1)))+theme_bw()

	Monthly_avg<-result_month%>%
	  ungroup()%>%
	  dplyr::select(-Date,-Year)%>%
	  group_by(Month)%>%
	  summarise(across(.fns = mean))
	
	Annual_avg<-result_ann%>%
	  select(-Year)%>%
	  summarise(across(.fns = mean))
	  	  
	return(list(monthly=result_month,annual=result_ann,
		  monthly_avg=Monthly_avg,annual_avg=Annual_avg,
		  Accuarcy=list(val_par_monthly=val_par_monthly,val_par_annual=val_par_annual),
		  Figs=list(monthly=p2,monthly_lines=p3,annual=p4)))
	  },

#' @title daily WaSSI based on SAC-SMA model
#' @description daily WaSSI model
#' @param da_daily daily input
#' @param soil_pars soil initial parameters
#' @param prcp daily precipitation data
#' @param pet potential evapotranspiration, in mm
#' @param monthly TRUE/FALSE, whether monthly input time series
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' sacSma_mon(pet, prcp,par)
#' }
#' @rdname sacSim_mon
#' @export
f_dailyWaSSI=function(da_daily,soil_pars,kc=0.6,GSjdays=c(128,280),forest="DBF",splitGrid=FALSE,...){
  
  require(dplyr)
  require(lubridate)
  
  if(splitGrid){
    da_daily<-da_daily%>%
      mutate(Rainfall=if_else(is.na(Rainfall),0,Rainfall))%>%
      mutate(j=yday(Date))%>%
      mutate(Fc=1-exp(-kc*LAI))%>%
      mutate(GW=ifelse(j>=GSjdays[1] & j<=GSjdays[2],"GW","NonGW"))%>%
      mutate(P_c=Rainfall*Fc,P_s=Rainfall*(1-Fc))
    
    # calculate potential Ei if it is not caculated before
    if(!"Ei_pot" %in% names(da_daily)) da_daily<-funs_nl$f_Ei_pot_USA(da_daily,forest)
    
    # Calculate the canopy Evaporation based on PET
    da_daily<-funs_nl$f_Evap(da_daily)
    
    # partition PET to canopy PET and soil surface PET
    da_sac<-da_daily%>%
      rowwise() %>%
      arrange(Date)%>%
      mutate(PET_Ec=PT*Fc-Ei,PET_Es=PT*(1-Fc))
    
    #print(summary(da_sac))
    
    da_sac[is.na(da_sac)]<-0
    
    out_Ec<-funs_nl$f_SacSma(pet =da_sac$PET_Ec,prcp = da_sac$P_Ei, par = soil_pars)
    
    out_Es<-funs_nl$f_SacSma(pet =da_sac$PET_Es,prcp = da_sac$P_s, par = soil_pars,SoilEvp = T)
    
	data_Ec<-cbind(da_sac,out_Ec)%>%
		dplyr::select(Date,Rainfall,VPD,PT,PET_Ec,Ei_pot,Ei,Fc,LAI,aetTot,aetUZT,aetUZF,uztwc,lztwc,WaYldTot)
	  
	  data_Es<-cbind(da_sac,out_Es)%>%
		dplyr::select(Date,aetTot,aetUZT,aetUZF,uztwc,lztwc,WaYldTot)
	  
	  result_SACSMA<-data_Ec%>%
		left_join(data_Es,by="Date",suffix=c(".c",".s"))%>%
		mutate(Year=year(Date),Month=month(Date))%>%
		mutate(Ec=aetTot.c,Es=aetTot.s)%>%
		mutate(AET=Ec+Es+Ei)%>%
		dplyr::select(Date,Rainfall,VPD,Fc,PT,PET_Ec,Ei_pot,Ei,Es,Ec,AET,WaYldTot.c,WaYldTot.s)%>%
		dplyr::rename(ET=AET)%>%
		mutate(WaYldTot=WaYldTot.s+WaYldTot.c,WaSSI_Tr=Ec/PET_Ec,WaSSI=ET/PT)%>%
		mutate(WaSSI_Tr=if_else(PET_Ec==0,1,WaSSI_Tr),WaSSI=if_else(PT==0,1,WaSSI))%>%
		mutate(Tr_ET=Ec/ET)%>%
		mutate(Tr_ET=if_else(ET==0 | is.na(ET) | is.nan(Tr_ET),1,Tr_ET))%>%
		mutate(Method="dWaSSI")
	
    
  }else{
    
    da_daily<-da_daily%>%
      mutate(Rainfall=if_else(is.na(Rainfall),0,Rainfall))%>%
      mutate(j=yday(Date))%>%
      mutate(Fc=1-exp(-kc*LAI))%>%
      mutate(GW=ifelse(j>=GSjdays[1] & j<=GSjdays[2],"GW","NonGW"))%>%
      mutate(P_c=Rainfall)
    
    # calculate potential Ei if it is not caculated before
    if(!"Ei_pot" %in% names(da_daily)) da_daily<-funs_nl$f_Ei_pot_USA(da_daily,forest)
    
    # Calculate the canopy Evaporation based on PET
    da_daily<-funs_nl$f_Evap(da_daily)
    
    # partition PET to canopy PET and soil surface PET
    da_sac<-da_daily%>%
      rowwise() %>%
      arrange(Date)%>%
      mutate(PET_Ec=(PT-Ei)*Fc,PET_Es=(PT-Ei)*(1-Fc))
    
    #print(summary(da_sac))
    
    da_sac[is.na(da_sac)]<-0
    
    out_Ec<-funs_nl$f_SacSma(pet =da_sac$PET_Ec,prcp = da_sac$P_Ei, par = soil_pars)
    
    out_Es<-funs_nl$f_SacSma(pet =da_sac$PET_Es,prcp = da_sac$P_Ei, par = soil_pars,SoilEvp = T)
    
	# Update flow from veg
	data_Ec<-cbind(da_sac,out_Ec)%>%
		mutate(WaYldTot=(WYSurface+WYInter)*Fc+WYBase)%>%
		dplyr::select(Date,Rainfall,VPD,PT,PET_Ec,Ei_pot,Ei,Fc,LAI,aetTot,aetUZT,aetUZF,uztwc,lztwc,WaYldTot)
		
	# Update flow from Soil surface
	data_Es<-cbind(da_sac,out_Es)%>%
		mutate(WaYldTot=(WYSurface+WYInter)*(1-Fc))%>%
		dplyr::select(Date,aetTot,aetUZT,aetUZF,uztwc,lztwc,WaYldTot)
  
	result_SACSMA<-data_Ec%>%
		left_join(data_Es,by="Date",suffix=c(".c",".s"))%>%
		mutate(Year=year(Date),Month=month(Date))%>%
		mutate(Ec=aetTot.c,Es=aetTot.s)%>%
		mutate(AET=Ec+Es+Ei)%>%
		dplyr::select(Date,Rainfall,VPD,Fc,PT,PET_Ec,Ei_pot,Ei,Es,Ec,AET,WaYldTot.c,WaYldTot.s)%>%
		dplyr::rename(ET=AET)%>%
		mutate(WaYldTot=WaYldTot.s+WaYldTot.c,WaSSI_Tr=Ec/PET_Ec,WaSSI=ET/PT)%>%
		mutate(WaSSI_Tr=if_else(PET_Ec==0,1,WaSSI_Tr),WaSSI=if_else(PT==0,1,WaSSI))%>%
		mutate(Tr_ET=Ec/ET)%>%
		mutate(Tr_ET=if_else(ET==0 | is.na(ET) | is.nan(Tr_ET),1,Tr_ET))%>%
		mutate(Method="dWaSSI")
	
  }
  
  
  # UWUE from  Zhou 2015; WUE from Zhang 
  uWUEp<-data.frame("IGBP"=c("CRO","DBF","GRA","ENF","WSA","MF","CSH","Average"),"uWUEp"=c(11.24,9.55,7.88,9.96,9.39,9.07,6.84,9.52),"uWUEp_sd"=c(2.9,1.6,1.78,2.81,1.35,2,1.44,2.53))
  # Calculte GPP from Tr
  if("VPD" %in% names(result_SACSMA))
    result_SACSMA<-result_SACSMA%>%
    mutate(VPD=VPD*10)%>% # kPa to hPa
    mutate(GPP=Ec*uWUEp$uWUEp[uWUEp$IGBP==forest] /sqrt(VPD))%>%
    mutate(GPP=if_else(VPD==0 ,0,GPP))%>%
    mutate(GPP_SD=Ec*uWUEp$uWUEp_sd[uWUEp$IGBP==forest]/sqrt(VPD))%>%
    mutate(GPP_SD=if_else(VPD==0 ,0,GPP_SD))%>%
    mutate(Method="dWaSSI")
  
  return(result_SACSMA)
}

)

## This is a list for FLUXNET data process
FLX_fns<-list(

## Function for convert lat and long to shapefile----
#' @param da dataframe has lat and long.
#' @keywords lat and long
#' @export
#' @examples
#'	Longitude<-c( -94.99729,-94.99726,-94.99457,-94.99458,-94.99729)
#'	Latitude<-c( 29.17112, 29.17107, 29.17273, 29.17278, 29.17112)
#'  da<-data.frame(Longitude=Longitude,Latitude=Latitude)
#'	ha<-f_LongLat2Point(da)

LongLat2Points=function(da,LongField="Longitude",LatField="Latitude",outfile=NULL){
  require(sp)
  require(rgdal)
  
  xy<-data.frame(X=da[LongField],Y=da[LatField])
  
  points<-SpatialPointsDataFrame(coords = xy, data = da,
                                 proj4string = CRS("+proj=longlat +datum=WGS84") )
								 
  if(!is.null(outfile)) writeOGR(points,outfile,"XY",driver = "ESRI Shapefile")
  
  return(points)
},

## Function for Read FLUXNET data----
#' @param dirfolder where those csvs are
#' @param Sites Filter site IDs
#' @param Scale "HH", "DD", "WW","MM","YY"
#' @param Var "FULLSET" OR "SUBSET"
#' @keywords libraries
#' @export
#' @examples
#' da<-f_read_FLUXNET("E:/Fluxdata")

Read_FLUXNET=function(dirfolder,Sites=NULL,Scale="MM",Var="FULLSET"){
	#require(plyr)
##get names of all flux data
  allFiles<-dir(path=dirfolder,include.dirs=TRUE,all.files =TRUE,pattern = ".csv",full.names = T,recursive = T)
  names<-dir(path=dirfolder,all.files =TRUE,pattern = ".csv",recursive = T)
  Sitefiles<-data.frame(Name=names,Files=allFiles)%>%
    filter(str_detect(Name,paste0(Var,"_",Scale,"_")))%>%
    mutate(Site_ID=substr(Name, 5, 10))
  
  if(!is.null(Sites)) Sitefiles <-Sitefiles%>% filter(Site_ID %in% Sites)
  
  da_original<-data.frame()
  for (i in c(1:nrow(Sitefiles))){
  
  		.csv<-read.csv(file=Sitefiles$Files[i],head=T)
  		.csv<-cbind(Site_ID=Sitefiles$Site_ID[i],.csv)
  		print(Sitefiles$Site_ID[i])
  		da_original<-plyr::rbind.fill(da_original,.csv)
  }
da_original
},


## Function for Select FLUXNET variables and calssify QC----
#' @param flux_original data read by f_read_FLUXNET
#' @keywords libraries
#' @export
#' @examples
#' da<-f_sel_vars(flux_original)
SelectVars=function(flux_original,QC_flag=0.8,HighQA=FALSE,Vars=NULL){
  ##--------------------------------------------------------------
  # https://fluxnet.org/data/fluxnet2015-dataset/subset-data-product/
#	**Atomospheric**  
#		TA	deg C	Temperature  
#		SW	W m-2	Shortwave radiation  
#		LW	W m-2	Longwave radiation  
#		VPD	hPa	  Vapor Pressure Deficit  
#		PA	kpa	  Atmospheric pressure  
#		P	  mm	  Precipitation  
#		WS	m s-1	Wind speed  
#		CO2	umolCO2 mol-1	CO2 mole fraction  
#		TS	deg C	Soil temperature  
#		SWC	%	    Soil water content  
#	**Energy**  
#		LE	  W m-2	Latent heat flux  
#		H	  W m-2	Sensible heat flux  
#		EBC		energy closure balance  
#	**NET ECOSYSTEM EXCHANGE**  	  
#		CUT		            Constant Ustar Threshold  
#		VUT		            Variable Ustar Threshold  
#		NEE	  gC m-2 d-1	Net Ecosystem Exchange  
#		RECO  gC m-2 d-1	Ecosystem Respiration  
#		GPP	  gC m-2 d-1	Gross Primary Production 
  ## ----------------------------------------------------------
    
  allvars<-names(flux_original)
  # this if for testing select variables
  allvars<-allvars[-c(grep("_QC",allvars),grep("_SD",allvars),grep("_SE",allvars),grep("_SR",allvars),grep("_JOINTUNC",allvars),grep("_RANDUNC",allvars),grep("_REF",allvars))]
  allvars[!grepl("[0-9]",allvars)]
  
  #select useful vaiables 
  allvars<-names(flux_original)
  if(is.null(Vars)) {
	keeps <- c("Site_ID","TIMESTAMP","TA_F","P_F","SW_IN_F","LW_IN_F","VPD_F","PA_F","USTAR","NETRAD","WS_F","LE_F_MDS","LE_CORR","H_F_MDS","H_CORR","G_F_MDS","EBC_CF_N","SWC_F_MDS_1","NEE_CUT_MEAN","NEE_VUT_MEAN","RECO_NT_VUT_MEAN","RECO_NT_CUT_MEAN","GPP_NT_CUT_MEAN","GPP_NT_VUT_MEAN","RECO_DT_VUT_MEAN","RECO_DT_CUT_MEAN","GPP_DT_CUT_MEAN","GPP_DT_VUT_MEAN","NEE_CUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF","RECO_NT_CUT_REF","GPP_NT_CUT_REF","GPP_NT_VUT_REF","RECO_DT_VUT_REF","RECO_DT_CUT_REF","GPP_DT_CUT_REF","GPP_DT_VUT_REF")
  
  }else{
  
	keeps <- c("Site_ID","TIMESTAMP",Vars)
  
  }
  
  keeps_QC <- c("Site_ID","TIMESTAMP",paste(keeps[-c(1:2)],"_QC",sep=""))
  
  keeps<-keeps[-grep("_CUT",keeps)]
  keeps_QC<-keeps_QC[-grep("_CUT",keeps_QC)]
  
  flux_data <- flux_original[,(names(flux_original) %in% keeps)]
  flux_data_QC <- flux_original[,(names(flux_original) %in% keeps_QC)]
  flux_QC<-flux_data_QC
  
  # set data quality based on the QC
  ncols<-3:ncol(flux_QC)
  flux_QC[ncols][flux_data_QC[ncols]>=0.8]<-"A"
  flux_QC[ncols][flux_data_QC[ncols]>=0.5 & flux_data_QC[ncols]<0.8]<-"B"
  flux_QC[ncols][flux_data_QC[ncols]<0.5]<-"C"
  flux_QC[ncols][is.na(flux_data_QC[ncols]<0.5)]<-"D"
   
  
  if(HighQA){
  # delete low quality data
	  for (i in c(3:length(flux_data))){
		print(names(flux_data)[i])
		# set all -9999 as NA
		flux_data[[i]][flux_data[[i]]< -299]<-NA
		
		# set data below the quality code as NA
		index_QC<-grep(names(flux_data)[i],names(flux_data_QC))
		if(length(index_QC)>0){
		  print(names(flux_data_QC)[index_QC])
		  flux_data[[i]][flux_data_QC[index_QC]<QC_flag]<-NA 
		}
		
	  }
  }
  
  # set carbon to NA based on NEE quality
  # for (var in c("RECO_DT_VUT_MEAN","GPP_DT_VUT_MEAN","RECO_NT_VUT_MEAN","GPP_NT_VUT_MEAN")){
  #   flux_data[[var]][flux_data_QC$NEE_VUT_MEAN_QC<QC_flag]<-NA
  # }
 merge(flux_data,flux_QC,by=c("Site_ID","TIMESTAMP"))
},

## Function for conver LE unit for FLUXNET data----
#' @param flux_data data read by f_read_FLUXNET
#' @param Scale daily or monthly
#' @keywords libraries
#' @export
#' @examples
#' da<-f_convert_unit(flux_data)
ConvertUnit=function(flux_data,LEField="LE_F_MDS",Scale="daily",useTair=T){
  # convert units
  require(lubridate)
  if(useTair){
     ##Calculate ET from LE and Tair from the water density and latent heat of vaporation, see Moffat manuscript on WUE
#  λ (J kg-1) = 1000*(2500 – 2.37T), T = Air Temperature in °C (Celsius) (Pereira et al., 2013):
    flux_data$ET<-flux_data[,LEField]/(1000*(2500 - 2.37 * flux_data$TA_F)) # (ET, mm s-1)

  }else{

    flux_data$ET<-flux_data[,LEField]/2454000 # (ET, mm s-1)

  }

  if(Scale=="daily"){
      flux_data<-flux_data%>%
        mutate(Date=as.Date(paste0(flux_data$TIMESTAMP),"%Y%m%d"))%>%
        mutate(ET=ET*60*60*24)                #(ET, mm/day)
    
  }else{
    
      flux_data<-flux_data%>%
      mutate(Date=as.Date(paste0(flux_data$TIMESTAMP,"01"),"%Y%m%d"))%>%
      mutate(ndays=days_in_month(Date))%>%
      mutate(P_F=P_F*ndays)%>% # Preciptation to mm/month
      mutate(ET=ET*60*60*24*ndays)   #(ET, mm/month)
  }
}
)


dWaSSI=list(

addWarmup=function(da,nyr=2){

	### This function is used to add number of years data to the begining for warming up
	# The data shoud have at either Date or Year field

  require(lubridate)
  
  if("Year" %in% names(da) & "Date" %in% names(da) ){
    
    da_warm<-da%>%
      filter(Year<=min(da$Year)+(nyr-1))%>%
      mutate(Year=Year-nyr,Date=Date-years(nyr))    
    
  }else if("Year" %in% names(da)){
    da_warm<-da%>%
      filter(Year<=min(da$Year)+(nyr-1))%>%
      mutate(Year=Year-nyr)
    
  }else if("Date" %in% names(da)){
    
    da_warm<-da%>%
      mutate(Year=year(Date))%>%
      filter(Year<=min(da$Year)+(nyr-1))%>%
      mutate(Year=Year-nyr,Date=Date-years(nyr))%>%
      dplyr::select(-Year)
  }
  
  da$Warmup<-FALSE
  
  da_warm<-da_warm%>%
    mutate(Warmup=TRUE)%>%
    rbind(da)

  return(da_warm)
},

WaSSI=function(da_daily,soil_pars,kc=0.6,GSjdays=c(128,280),forest="DBF",...){

  require(dplyr)
  require(lubridate)

    da_daily<-da_daily%>%
      mutate(Rainfall=if_else(is.na(Rainfall),0,Rainfall))%>%
      mutate(j=yday(Date))%>%
      mutate(Fc=1-exp(-kc*LAI))%>%
      mutate(GW=ifelse(j>=GSjdays[1] & j<=GSjdays[2],"GW","NonGW"))%>%
      mutate(P_c=Rainfall)

    # calculate potential Ei if it is not caculated before
    if(!"Ei_pot" %in% names(da_daily)) da_daily<-funs_nl$f_Ei_pot_USA(da_daily,forest)

    # Calculate the canopy Evaporation based on PET
    da_daily<-funs_nl$f_Evap(da_daily)

    # partition PET to canopy PET and soil surface PET
    da_sac<-da_daily%>%
      rowwise() %>%
      arrange(Date)%>%
      mutate(PET_Ec=(PT-Ei)*Fc,PET_Es=(PT-Ei)*(1-Fc))

    #print(summary(da_sac))

    da_sac[is.na(da_sac)]<-0

    out_Ec<-dWaSSI$SMA(prcp = da_sac$P_Ei,pet = da_sac$PET_Ec,pet_Soil = da_sac$PET_Es,SoilEvp = T, par = soil_pars)

	result<-cbind(da_sac,out_Ec[c("ESoilTot","aetTot","aetUZT","aetUZF","WYSurface","WYInter","WYBase","uztwc","lztwc","WaYldTot")])%>%
		mutate(Year=year(Date),Month=month(Date))%>%
		mutate(Ec=aetTot,Es=ESoilTot)%>%
		mutate(AET=Ec+Es+Ei)%>%
		dplyr::select(Date,Rainfall,VPD,Fc,PT,PET_Ec,PET_Es,Ei_pot,Ei,Es,Ec,AET,WaYldTot,aetUZT,aetUZF,WYSurface,WYInter,WYBase,uztwc,lztwc)%>%
		dplyr::rename(ET=AET)%>%
		mutate(WaSSI_Tr=Ec/PET_Ec,WaSSI=ET/PT)%>%
		mutate(WaSSI_Tr=if_else(PET_Ec==0,1,WaSSI_Tr),WaSSI=if_else(PT==0,1,WaSSI))%>%
		mutate(Tr_ET=Ec/ET)%>%
		mutate(Tr_ET=if_else(ET==0 | is.na(ET) | is.nan(Tr_ET),1,Tr_ET))%>%
		mutate(Method="dWaSSI")

  # UWUE from  Zhou 2015; WUE from Zhang
  uWUEp<-data.frame("IGBP"=c("CRO","DBF","GRA","ENF","WSA","MF","CSH","Average"),"uWUEp"=c(11.24,9.55,7.88,9.96,9.39,9.07,6.84,9.52),"uWUEp_sd"=c(2.9,1.6,1.78,2.81,1.35,2,1.44,2.53))
  # Calculte GPP from Tr
  if("VPD" %in% names(result))
    result<-result%>%
		mutate(VPD=VPD*10)%>% # kPa to hPa
		mutate(GPP=Ec*uWUEp$uWUEp[uWUEp$IGBP==forest] /sqrt(VPD))%>%
		mutate(GPP=if_else(VPD==0 ,0,GPP))%>%
		mutate(GPP_SD=Ec*uWUEp$uWUEp_sd[uWUEp$IGBP==forest]/sqrt(VPD))%>%
		mutate(GPP_SD=if_else(VPD==0 ,0,GPP_SD))%>%
		mutate(Method="dWaSSI")

  return(result)
  
},

#' @title daily SMA model
#' @description Soil moisture accounting for daily WaSSI
#' @param prcp daily precipitation data
#' @param pet potential evapotranspiration, in mm
#' @param pet_Soil daily input
#' @param soil_pars soil initial parameters
#' @param SoilEvp TRUE/FALSE, whether calculate Soil Evaporation
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' sacSma_mon(pet, prcp,par)
#' }
#' @rdname sacSim_mon
#' @export
SMA = function(prcp,pet,par,pet_Soil=NULL,SoilEvp=FALSE, ini.states = c(0,0,500,500,500,0)) {

	names(par)<-toupper(names(par))
  if(sum(names(par) %in% c("UZTWM","UZFWM","UZK", "ZPERC",  "REXP", "LZTWM", "LZFSM", "LZFPM",  "LZSK",  "LZPK", "PFREE"))==11){
    uztwm  <-  par["UZTWM"]    # Upper zone tension water capacity [mm]
    uzfwm  <-  par["UZFWM"]    # Upper zone free water capacity [mm]
    lztwm  <-  par["LZTWM"]    # Lower zone tension water capacity [mm]
    lzfpm  <-  par["LZFPM"]    # Lower zone primary free water capacity [mm]
    lzfsm  <-  par["LZFSM"]    # Lower zone supplementary free water capacity [mm]
    uzk    <-  par["UZK"]    # Upper zone free water lateral depletion rate [1/day]
    lzpk   <-  par["LZPK"]    # Lower zone primary free water depletion rate [1/day]
    lzsk   <-  par["LZSK"]    # Lower zone supplementary free water depletion rate [1/day]
    zperc  <-  par["ZPERC"]    # Percolation demand scale parameter [-]
    rexp   <-  par["REXP"]   # Percolation demand shape parameter [-]
    pfree  <-  par["PFREE"]   # Percolating water split parameter (decimal fraction)
    pctim  <- 0 #   par[12]   # Impervious fraction of the watershed area (decimal fraction)
    adimp  <- 0 #  par[13]   # Additional impervious areas (decimal fraction)
    riva   <- 0 #  par[14]   # Riparian vegetation area (decimal fraction)
    side   <- 0 # par[15]   # The ratio of deep recharge to channel base flow [-]
    rserv  <- 0 #par[16]   # Fraction of lower zone free water not transferrable (decimal fraction)
  }else{
    print("Input soil parameter is missing")
  }

  # Initial Storage States (SAC-SMA)
  uztwc <- uztwm # Upper zone tension water storage
  uzfwc <- uzfwm # Upper zone free water storage
  lztwc <- lztwm # Lower zone tension water storage
  lzfsc <- lzfsm # Lower zone supplementary free water storage
  lzfpc <- lzfpm # Upper zone primary free water storage
  adimc <- 0 # Additional impervious area storage

  # RESERVOIR STATE ARRAY INITIALIZATION
  simaet  <- vector(mode = "numeric", length = length(prcp)) # total ET
  simaetSoil  <- vector(mode = "numeric", length = length(prcp)) # ET for Soil total
  simaetSoil_1  <- vector(mode = "numeric", length = length(prcp)) # ET for Soil surface for soil tention
  simaetSoil_2  <- vector(mode = "numeric", length = length(prcp)) # ET for Soil surface for soil free water
  simaet1  <- vector(mode = "numeric", length = length(prcp)) # ET for Veg from upper for soil tention
  simaet2  <- vector(mode = "numeric", length = length(prcp)) # ET for Vegfrom upper for soil free water
  simaet3  <- vector(mode = "numeric", length = length(prcp)) # ET for Vegfrom lower soil for soil tention
  simaet4  <- vector(mode = "numeric", length = length(prcp)) # ET for Vegfrom lower soil for soil free water
  simaet5  <- vector(mode = "numeric", length = length(prcp))
  simflow   <- vector(mode = "numeric", length = length(prcp))
  base_tot  <- vector(mode = "numeric", length = length(prcp))
  surf_tot  <- vector(mode = "numeric", length = length(prcp))
  interflow_tot  <- vector(mode = "numeric", length = length(prcp))
  uztwc_ts   <- vector(mode = "numeric", length = length(prcp))
  uzfwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lztwc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfpc_ts  <- vector(mode = "numeric", length = length(prcp))
  lzfsc_ts  <- vector(mode = "numeric", length = length(prcp))

  thres_zero  <- 0.00001 # Threshold to be considered as zero
  parea       <- 1 - adimp - pctim

  for (i in 1:length(prcp)) {

    ### Set input precipitation and potential evapotranspiration
    pr = prcp[i] # This could be effective rainfall, a sum of rainfall and snowmelt
	edmnd = pet[i]
	
    # Initialize time interval sums
    sbf   <- 0  # Sum of total baseflow(from primary and supplemental storages)
    spbf   <- 0  # Sum of total baseflow(from primary storages)
    ssur  <- 0  # Sum of surface runoff
    sif   <- 0  # Sum of interflow
    sperc <- 0  # Time interval summation of percolation
    sdro  <- 0  # Sum of direct runoff from the additional impervious area
    tet   <-0   # Sum of total AET

	# Calculate the soil water evapration first for the upper layer
	if(SoilEvp & !is.null(pet_Soil)){
		edmnd_soil = pet_Soil[i]
		# For soil evapration
		
		# ET(1), ET soil from Upper zone tension water storage
		etSoil_1 <- edmnd_soil * uztwc/uztwm
		red <- edmnd_soil - etSoil_1  # residual ET demand
		uztwc <- uztwc - etSoil_1

		# ET(2), ET from upper zone free water storage
		etSoil_2 <- 0
		# in case et1 > uztws, no water in the upper tension water storage
		if (uztwc <= 0) {
		  etSoil_1 <- etSoil_1 + uztwc #et1 = uztwc
		  uztwc <- 0
		  red <- edmnd_soil - etSoil_1

		  # when upper zone free water content is less than residual ET
		  if (uzfwc < red) {

			# all content at upper zone free water zone will be gone as ET
			etSoil_2 <- uzfwc
			uzfwc <- 0
			red <- red - etSoil_2
			if (uztwc < thres_zero) uztwc <- 0
			if (uzfwc < thres_zero) uzfwc <- 0

			# when upper zone free water content is more than residual ET
		  } else {
			etSoil_2 <- red  # all residual ET will be gone as ET
			uzfwc <- uzfwc - etSoil_2
			red <- 0
		  }

		  # in case et1 <= uztws, all maximum et (et1) are consumed at uztwc,
		  # so no et from uzfwc (et2=0)
		} else {

		  # There's possibility that upper zone free water ratio exceeds
		  #upper zone tension water ratio. If so, free water is transferred to
		  #tension water storage

		  if((uztwc / uztwm) < (uzfwc / uzfwm)) {
			uzrat = (uztwc + uzfwc) / (uztwm + uzfwm)
			uztwc = uztwm * uzrat
			uzfwc = uzfwm * uzrat
		  }

		  if(uztwc < thres_zero) uztwc = 0
		  if(uzfwc < thres_zero) uzfwc = 0

		}
	}

    # For veg transpiration
	## Compute for different compnents...
    # ET(1), ET from Upper zone tension water storage
    et1 <- edmnd * uztwc/uztwm
    red <- edmnd - et1  # residual ET demand
    uztwc <- uztwc - et1

    # ET(2), ET from upper zone free water storage
    et2 <- 0
    #print(paste0("I=",i," uztwm= ",uztwm," uztwc= ",uztwc," et1= ", et1, " pr= ",pr," pet= ",edmnd))
    # in case et1 > uztws, no water in the upper tension water storage
    if (uztwc <= 0) {
      et1 <- et1 + uztwc #et1 = uztwc
      uztwc <- 0
      red <- edmnd - et1

      # when upper zone free water content is less than residual ET
      if (uzfwc < red) {

        # all content at upper zone free water zone will be gone as ET
        et2 <- uzfwc
        uzfwc <- 0
        red <- red - et2
        if (uztwc < thres_zero) uztwc <- 0
        if (uzfwc < thres_zero) uzfwc <- 0

        # when upper zone free water content is more than residual ET
      } else {
        et2 <- red  # all residual ET will be gone as ET
        uzfwc <- uzfwc - et2
        red <- 0
      }

      # in case et1 <= uztws, all maximum et (et1) are consumed at uztwc,
      # so no et from uzfwc (et2=0)
    } else {

      # There's possibility that upper zone free water ratio exceeds
      #upper zone tension water ratio. If so, free water is transferred to
      #tension water storage

      if((uztwc / uztwm) < (uzfwc / uzfwm)) {
        uzrat = (uztwc + uzfwc) / (uztwm + uzfwm)
        uztwc = uztwm * uzrat
        uzfwc = uzfwm * uzrat
      }

      if(uztwc < thres_zero) uztwc = 0
      if(uzfwc < thres_zero) uzfwc = 0

    }

    # ET(3), ET from Lower zone tension water storage when residual ET > 0
    et3 <- red * lztwc / (uztwm + lztwm) #residual ET is always bigger than ET(3)
    lztwc <- lztwc - et3

    # if lztwc is less than zero, et3 cannot exceed lztws
    if(lztwc < 0) {
      et3   <- et3 + lztwc  # et3 = lztwc
      lztwc <- 0
    }

    # Water resupply from Lower free water storages to Lower tension water storage
    saved  <- rserv * (lzfpm + lzfsm)
    ratlzt <- lztwc / lztwm
    ratlz  <- (lztwc + lzfpc + lzfsc - saved) / (lztwm + lzfpm + lzfsm - saved)

    # water is first taken from supplementary water storage for resupply
    if (ratlzt < ratlz) {

      del <- (ratlz - ratlzt) * lztwm
      lztwc <- lztwc + del  # Transfer water from lzfss to lztws
      lzfsc <- lzfsc - del

      # if tranfer exceeds lzfsc then remainder comes from lzfps
      if(lzfsc < 0) {
        lzfpc <- lzfpc + lzfsc
        lzfsc <- 0
      }
    }

    if(lztwc < thres_zero) {lztwc <- 0}
    # Comment for additional imprevious ET
    # # ET(5), ET from additional impervious (ADIMP) area
    # # ????? no idea where this come from, I think there's a possibility that et5 can be negative values
    et5   <- et1 + (red + et2) * (adimc - et1 - uztwc) / (uztwm + lztwm)
    adimc <- adimc - et5
    if(adimc < 0) {
      #et5 cannot exceed adimc
      et5 <- et5 + adimc # et5 = adimc
      adimc <- 0
    }
    et5 <- et5 * adimp


	# Add Precip in to update the soil water content

    # Time interval available moisture in excess of uztw requirements
    twx <- pr + uztwc - uztwm

    # all moisture held in uztw- no excess
    if(twx < 0) {
      uztwc <- uztwc + pr
      twx <- 0
      # moisture available in excess of uztw storage
    } else {
      uztwc = uztwm
    }
    #
    # for now twx is excess rainfall after filling the uztwc
    #
    adimc <- adimc + pr - twx

    # Compute Impervious Area Runoff
    roimp <- pr * pctim

    # Determine computational time increments for the basic time interval
    ninc <- floor(1 + 0.2*(uzfwc+twx))  # Number of time increments that interval is divided into for further soil-moisture accountng

    dinc <- 1.0 / ninc                    # Length of each increment in days
    pinc <- twx / ninc                    # Amount of available moisture for each increment

    # Compute free water depletion fractions for the time increment
    #(basic depletions are for one day)
    duz   <- 1 - (1 - uzk)^dinc
    dlzp  <- 1 - (1 - lzpk)^dinc
    dlzs  <- 1 - (1 - lzsk)^dinc

    #print(paste0("ninc=", str(ninc)))

    # Start incremental for-loop for the time interval (smaller scale than the input data time series)
    for (n in 1:ninc){

      adsur <- 0 # Amount of surface runoff. This will be updated.
      excess<- 0  # the excess of LZ soil water capacity

      # Compute direct runoff from adimp area
      ratio <- (adimc - uztwc) / lztwm
      if(ratio < 0) ratio <- 0

      # Amount of direct runoff from the additional impervious area
      addro <- pinc*(ratio^2)

      # Compute baseflow and keep track of time interval sum
      # Baseflow from free water primary storage
      bf_p <- lzfpc * dlzp
      lzfpc <- lzfpc - bf_p
      if(lzfpc <= 0.0001) {
        bf_p  <- bf_p + lzfpc
        lzfpc <- 0
      }

      sbf <- sbf + bf_p
      spbf<- sbf + bf_p
      # Baseflow from free water supplemental storage
      bf_s  <- lzfsc * dlzs
      lzfsc <- lzfsc - bf_s
      if (lzfsc <= 0.0001) {
        bf_s <- bf_s + lzfsc
        lzfsc <- 0
      }

      # Total Baseflow from primary and supplemental storages
      sbf <- sbf + bf_s

      # Compute PERCOLATION- if no water available then skip.
      if((pinc + uzfwc) <= 0.01) {
        uzfwc <- uzfwc + pinc
      } else {

        # Limiting drainage rate from the combined saturated lower zone storages
        percm <- lzfpm * dlzp + lzfsm * dlzs
        perc <- percm * uzfwc / uzfwm

        # DEFR is the lower zone moisture deficiency ratio
        defr <- 1.0 - (lztwc + lzfpc + lzfsc)/(lztwm + lzfpm + lzfsm)

        if(defr < 0) {defr <- 0}

        perc <- perc * (1.0 + zperc * (defr^rexp))

        # Note. . . percolation occurs from uzfws before pav is added

        # Percolation rate exceeds uzfws
        if(perc >= uzfwc) {perc <- uzfwc}

        uzfwc <- uzfwc - perc    # Percolation rate is less than uzfws.

        # Check to see if percolation exceeds lower zone deficiency.
        check <- lztwc + lzfpc + lzfsc + perc - lztwm - lzfpm - lzfsm
        if(check > 0) {
          perc <- perc - check
          uzfwc <- uzfwc + check
        }

        # SPERC is the time interval summation of PERC
        sperc <- sperc + perc

        # Compute interflow and keep track of time interval sum. Note that PINC has not yet been added.
        del <- uzfwc * duz # The amount of interflow

        ## Check whether interflow is larger than uzfwc
        if (del > uzfwc) {
          del<-uzfwc
          uzfwc<-0.0
        }else{
          uzfwc <- uzfwc - del
        }

        sif <- sif + del

        # Distribute percolated water into the lower zones. Tension water
        # must be filled first except for the PFREE area. PERCT is
        # percolation to tension water and PERCF is percolation going to
        # free water.

        perct <- perc * (1.0 - pfree)  # Percolation going to the tension water storage
        if((perct + lztwc) <= lztwm) {

          lztwc <- lztwc + perct
          percf <- 0 # Pecolation going to th lower zone free water storages

        } else {

          percf <- lztwc + perct - lztwm
          lztwc <- lztwm

        }

        # Distribute percolation in excess of tension requirements among the free water storages.
        percf <- percf + (perc * pfree)

        if(percf != 0) {

          # Relative size of the primary storage as compared with total lower zone free water storages.
          hpl <- lzfpm / (lzfpm + lzfsm)

          # Relative fullness of each storage.
          ratlp <- lzfpc / lzfpm
          ratls <- lzfsc / lzfsm

          # The fraction going to primary
          fracp <- hpl * 2 * (1 - ratlp) / (2 - ratlp - ratls)

          if(fracp > 1.0) {fracp <- 1.0}

          percp <- percf * fracp # Amount of the excess percolation going to primary
          percs <- percf - percp # Amount of the excess percolation going to supplemental
          lzfsc <- lzfsc + percs

          if(lzfsc > lzfsm) {
            percs <- percs - lzfsc + lzfsm
            lzfsc <- lzfsm
          }

          lzfpc <- lzfpc + percf - percs

          # This is different to Peter's
          #
          # Check to make sure lzfps does not exceed lzfpm
          if(lzfpc >= lzfpm) {
            excess <- lzfpc - lzfpm
            lztwc <- lztwc + excess
            lzfpc <- lzfpm
            if(lztwc >= lztwm) {
              excess <- lztwc - lztwm
              lztwc <- lztwm
            }
          }

        }

        #

        # Distribute PINC between uzfws and surface runoff
        if((pinc+excess) != 0) {

          # check if pinc exceeds uzfwm
          if((pinc + uzfwc+excess) <= uzfwm) {

            uzfwc <- uzfwc + pinc+excess  # no surface runoff
          } else {
            sur <- pinc + uzfwc + excess - uzfwm # Surface runoff
            uzfwc <- uzfwm

            ssur = ssur + (sur * parea)

            # ADSUR is the amount of surface runoff which comes from
            # that portion of adimp which is not currently generating
            # direct runoff. ADDRO/PINC is the fraction of adimp
            # currently generating direct runoff.
            adsur = sur * (1.0 - addro / pinc)
            ssur = ssur + adsur * adimp

          }
        }
      }

      adimc <- adimc + pinc - addro - adsur
      if(adimc > (uztwm + lztwm)) {
        addro = addro + adimc - (uztwm + lztwm)
        adimc = uztwm + lztwm
      }

      # Direct runoff from the additional impervious area
      sdro  = sdro + (addro * adimp)

      if(adimc < thres_zero) {adimc <- 0}

    } # END of incremental for loop

    # Compute sums and adjust runoff amounts by the area over which they are generated.

    # EUSED is the ET from PAREA which is 1.0 - adimp - pctim
    eused <- et1 + et2 + et3
    sif <- sif * parea

    # Separate channel component of baseflow from the non-channel component
    tbf <- sbf * parea   # TBF is the total baseflow
    bfcc <- tbf / (1 + side)    # BFCC is baseflow, channel component

    bfp = (spbf * parea) / (1.0 + side)
    bfs = bfcc - bfp
    if (bfs < 0.) bfs = 0
    bfncc = tbf - bfcc # BFNCC IS BASEFLOW, NON-CHANNEL COMPONENT

    # Ground flow and Surface flow
    base <- bfcc                       # Baseflow and Interflow are considered as Ground inflow to the channel
    surf <- roimp + sdro + ssur + sif  # Surface flow consists of Direct runoff and Surface inflow to the channel

    # ET(4)- ET from riparian vegetation.
    et4 <- (edmnd - eused) * riva  # no effect if riva is set to zero

    # Compute total evapotransporation - TET
    eused <- eused * parea
    tet <- tet+eused + et4 + et5

    # Check that adimc >= uztws
    # This is not sure?
    #if(adimc > uztwc) adimc <- uztwc

    # Total inflow to channel for a timestep
    tot_outflow <- surf + base - et4;

    ### ------- Adjustments to prevent negative flows -------------------------#

    # If total outflow <0 surface and baseflow needs to be updated
    if (tot_outflow < 0) {

      tot_outflow = 0; surf = 0; base = 0;

    } else {

      surf_remainder = surf - et4
      surf <- max(0,surf_remainder)

      if (surf_remainder < 0) { # In this case, base is reduced

        base = base + surf_remainder
        if (base < 0) base = 0
      }
    }


    # Total inflow to channel for a timestep
    simaet[i]  <- tet
    simaet1[i]  <- et1
    simaet2[i]  <- et2
    simaet3[i]  <- et3
    simaet4[i]  <- et4
    simaet5[i]  <- et5
    simflow[i]  <- tot_outflow
    #surf_tot[i] <- surf
    surf_tot[i] <- ssur
    interflow_tot[i] <- sif
    base_tot[i] <- base
    uztwc_ts[i] <- uztwc
    uzfwc_ts[i]  <- uzfwc
    lztwc_ts[i]  <- lztwc
    lzfpc_ts[i]  <- lzfpc
    lzfsc_ts[i]  <- lzfsc
	
	if(SoilEvp & !is.null(pet_Soil)){
		simaetSoil_1[i]	<-etSoil_1
		simaetSoil_2[i]	<-etSoil_2
		simaetSoil[i]	<-etSoil_2+etSoil_1
	}
	
  } #close time-loop

	if(SoilEvp & !is.null(pet_Soil)){
	  return(data.frame("ESoilTot"=simaetSoil,"ESoil_1"=simaetSoil_1,"ESoil_2"=simaetSoil_2,
						"aetTot" = simaet,"aetUZT" = simaet1,"aetUZF" = simaet2,"aetLZT" = simaet3,"aet4" = simaet4,"aet5" = simaet5,
						"WaYldTot" = simflow, "WYSurface" = surf_tot,"WYInter" = interflow_tot, "WYBase" = base_tot,
						"uztwc"=uztwc_ts,"uzfwc"=uzfwc_ts,
						"lztwc"=lztwc_ts,"lzfpc"=lzfpc_ts,"lzfsc"=lzfsc_ts))
	}else{
	return(data.frame("aetTot" = simaet,"aetUZT" = simaet1,"aetUZF" = simaet2,"aetLZT" = simaet3,"aet4" = simaet4,"aet5" = simaet5,
						"WaYldTot" = simflow, "WYSurface" = surf_tot,"WYInter" = interflow_tot, "WYBase" = base_tot,
						"uztwc"=uztwc_ts,"uzfwc"=uzfwc_ts,
						"lztwc"=lztwc_ts,"lzfpc"=lzfpc_ts,"lzfsc"=lzfsc_ts))
	}
},

# Calibration Soil parameters based on Q ----
#' @title Calibration Soil parameters of WaSSI model
#' @description FUNCTION_DESCRIPTION
#' @param data_in  dataframe with Date, P, E and Q cariables
#' @param warmup months or days for warming up WaSSI-C model
#' @param Sim_year The priod for calibration and validation
#' @return outputs calibration and validation result
#' @examples
#'  warmup=12
#'  stationname=""
#'  Sim_year<-list("Calibration"=c("2003-01-01","2010-12-01"),"Validation"=c("2011-01-01","2016-12-01"))
#'  WaSSI_calibration(data_in,Sim_year,stationname,warmup)
#' @export
# SNOW Model based on Tavg----
#' @title SNOW Model
#' @description Peter's Snow model
#' @param ts.prcp numeric vector of precipitation time-series (mm)
#' @param ts.temp numeric vector of average temperatuer time-series (Deg C)
#' @param snowrange (-3,1) the temperature range between snow and rain
#' @param meltmax  maximium ratio of snow melt (default: 0.5)
#' @param snowpack  initial snowpack (default: 0)
#' @return a dataframe of c("prcp","snowpack","snowmelt"), effective rainfall, snowpack and snowmelt
#' @rdname snowmelt
#' @details This is based on Peter's Fortran code
#' @examples
#' \dontrun{
#' mellt<-snow_melt(ts.prcp,ts.temp,meltmax=0.5,snowrange=c(-3,1),snowpack=0)
#' }
#' @export
snow_melt=function(ts.prcp,ts.temp,meltmax=1,snowrange=c(-5,-1),snowpack=0) {

  # Define outputs
  ts.snowpack <- vector(mode = "numeric", length = length(ts.prcp))
  ts.snowmelt <- vector(mode = "numeric", length = length(ts.prcp))
  ts.prcp.eff<- vector(mode = "numeric", length = length(ts.prcp))
  # loop each time step
  for (i in c(1:length(ts.prcp))){
    tavg<-ts.temp[i];prcp<-ts.prcp[i]
    # ---- FOR TEMPERATURE LESS THAN SNOW TEMP, CALCULATE SNOWPPT
    if (tavg <= snowrange[1]) {
      snow<-prcp
      rain<-0
      # ---- FOR TEMPERATURE GREATER THAN RAIN TEMP, CALCULATE RAINPPT
    }else if (tavg >= snowrange[2]){
      rain<-prcp
      snow<-0
      # ---- FOR TEMPERATURE BETWEEN RAINTEMP AND SNOWTEMP, CALCULATE SNOWPPT AND RAINPPT
    }else{
      snow<- prcp*((snowrange[2] - tavg)/(snowrange[2] - snowrange[1]))
      rain<-prcp-snow
    }
    # Calculate the snow pack
    snowpack<-snowpack+snow
    # ---- CALCULATE SNOW MELT FRACTION BASED ON MAXIMUM MELT RATE (MELTMAX) AND MONTHLY TEMPERATURE

    snowmfrac = ((tavg-snowrange[1])/(snowrange[2] - snowrange[1]))*meltmax

    if (snowmfrac >= meltmax) snowmfrac <- meltmax
    if (snowmfrac <0) snowmfrac <- 0

    # ---- CALCULATE AMOUNT OF SNOW MELTED (MM) TO CONTRIBUTE TO INFILTRATION & RUNOFF

    # ---- IF SNOWPACK IS LESS THAN 10.0 MM, ASSUME IT WILL ALL MELT (MCCABE AND WOLOCK, 1999)
    # ---- (GENERAL-CIRCULATION-MODEL SIMULATIONS OF FUTURE SNOWPACK IN THE WESTERN UNITED STATES)

    if (snowpack <= 10.0) {
      snowm<-snowpack
      snowpack<- 0

    }else{
      snowm = snowpack * snowmfrac
      snowpack = snowpack - snowm
    }

    # -- COMPUTE THE INFILTRATION FOR A GIVEN MONTH FOR EACH LAND USE
    ts.snowpack[i]<-snowpack
    ts.snowmelt[i]<-snowm
    ts.prcp.eff[i]<-rain+snowm
  }
  return(data.frame(prcp=ts.prcp.eff,snowpack=ts.snowpack,snowmelt=ts.snowmelt))
},

# Solar from EcoHydRology package----
#' @title SNOW Model
#' @description snow melt model from EcoHydRology package
#' @param Date "%Y-%m-%d" (mm)
#' @param ts.prcp numeric vector of precipitation time-series (mm)
#' @param ts.temp numeric vector of average temperatuer time-series (Deg C)
#' @param snowrange (-3,1) the temperature range between snow and rain
#' @param meltmax  maximium ratio of snow melt (default: 0.5)
#' @param snowpack  initial snowpack (default: 0)
#' @return a dataframe of c("prcp","snowpack","snowmelt"), effective rainfall, snowpack and snowmelt
#' @rdname snowmelt
#' @details This is based on Peter's Fortran code
#' @examples
#' \dontrun{
#' mellt<-snow_melt(ts.prcp,ts.temp,meltmax=0.5,snowrange=c(-3,1),snowpack=0)
#' }
#' @export
Solar = function (lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="kJm2d", latUnits = "unknown", printWarn=TRUE) {
	
	if ((abs(lat) > pi/2 & latUnits == "unknown") | latUnits == "degrees" ){
		if (printWarn==TRUE) 
		lat <- lat*pi/180
	} else if (latUnits == "unknown"){
		if (printWarn==TRUE) warning("In Solar(): Input latitude units are not specified and assumed to be radians")
	}
	
	if (units == "kJm2d") convert <- 1 else convert <- 86.4  # can convert to W/m2
    return( signif((1 - albedo) * (1 - forest) * transmissivity(Tx, Tn) * 
        PotentialSolar(lat, Jday) * slopefactor(lat, Jday, slope, aspect) / convert , 5 ))
},

# SNOWmelt Model from EcoHydRology package----
#' @title SNOW Model
#' @description snow melt model from EcoHydRology package
#' @param Date "%Y-%m-%d" (mm)
#' @param ts.prcp numeric vector of precipitation time-series (mm)
#' @param ts.temp numeric vector of average temperatuer time-series (Deg C)
#' @param snowrange (-3,1) the temperature range between snow and rain
#' @param meltmax  maximium ratio of snow melt (default: 0.5)
#' @param snowpack  initial snowpack (default: 0)
#' @return a dataframe of c("prcp","snowpack","snowmelt"), effective rainfall, snowpack and snowmelt
#' @rdname snowmelt
#' @details This is based on Peter's Fortran code
#' @examples
#' \dontrun{
#' mellt<-snow_melt(ts.prcp,ts.temp,meltmax=0.5,snowrange=c(-3,1),snowpack=0)
#' }
#' @export
SnowMelt=function(JDay, precip_mm, Tmax_C, Tmin_C, lat_deg, slope=0, aspect=0, tempHt=1, windHt=2, groundAlbedo=0.25, 		SurfEmissiv=0.95, windSp=2, forest=0, startingSnowDepth_m=0, startingSnowDensity_kg_m3=450){	
## Constants :
	WaterDens <- 1000			# kg/m3
	lambda <- 3.35*10^5			# latent heat of fusion (kJ/m3)
	lambdaV <- 2500				# (kJ/kg) latent heat of vaporization
	SnowHeatCap <- 2.1			# kJ/kg/C
	LatHeatFreez <- 333.3		# kJ/kg
	Cw <- 4.2*10^3				# Heat Capacity of Water (kJ/m3/C)
	
##	Converted Inputs :
	Tav <- (Tmax_C+Tmin_C)/2		# degrees C
	precip_m <- precip_mm*0.001	 	# precip in m 
	R_m <- precip_m					# (m) depth of rain
	R_m[which(Tav < 0)] <- 0		# ASSUMES ALL SNOW at < 0C
	NewSnowDensity <- 50+3.4*(Tav+15)		# kg/m3
	NewSnowDensity[which(NewSnowDensity < 50)] <- 50
	NewSnowWatEq <- precip_m				# m
	NewSnowWatEq[which(Tav >= 0)] <- 0			# No new snow if average temp above or equals 0 C
	NewSnow <- NewSnowWatEq*WaterDens/NewSnowDensity		# m
	#JDay <- strptime(Date, format="%Y-%m-%d")$yday+1
	lat <- lat_deg*pi/180		#	latitude in radians
	rh 	<- log((windHt+0.001)/0.001)*log((tempHt+0.0002)/0.0002)/(0.41*0.41*windSp*86400)	# (day/m) Thermal Resistance	 
	if (length(windSp)==1) rh <- rep(rh,length(precip_mm))									##	creates a vector of rh values
	cloudiness 		<- EstCloudiness(Tmax_C,Tmin_C)
	AE 				<- AtmosphericEmissivity(Tav, cloudiness)	# (-) Atmospheric Emissivity

#  New Variables	:
	SnowTemp 		<- rep(0,length(precip_m)) 		# Degrees C
	rhos 			<- SatVaporDensity(SnowTemp)	# 	vapor density at surface (kg/m3)
	rhoa 			<- SatVaporDensity(Tmin_C)		#	vapor density of atmoshpere (kg/m3) 
	SnowWaterEq 	<- vector(length=length(precip_mm))		#  (m) Equiv depth of water
	TE 				<- rep(SurfEmissiv,length(precip_mm))	#	(-) Terrestrial Emissivity
	DCoef 			<- rep(0,length(precip_mm))				#   Density Coefficient (-) (Simplified version)
	SnowDensity 	<- rep(450,length(precip_mm))			#  (kg/m3)  Max density is 450
	SnowDepth 		<- vector(length=length(precip_mm))		#  (m)
	SnowMelt 		<- rep(0,length(precip_mm))				#  (m)
	Albedo 			<- rep(groundAlbedo,length(precip_mm)) 	#  (-) This will change for days with snow
	
##	Energy Terms
	H 		<- vector(length=length(precip_mm))	#	Sensible Heat exchanged (kJ/m2/d)
	E 		<- vector(length=length(precip_mm))	#	Vapor Energy	(kJ/m2/d)
	S 		<- vector(length=length(precip_mm))	#	Solar Radiation (kJ/m2/d)
	La 		<- Longwave(AE, Tav)					#	Atmospheric Longwave Radiation (kJ/m2/d)
	Lt 		<- vector(length=length(precip_mm))	#	Terrestrial Longwave Radiation (kJ/m2/d)
	G 		<- 173								#	Ground Condution (kJ/m2/d) 
	P 		<- Cw * R_m * Tav					# 	Precipitation Heat (kJ/m2/d)
	Energy 	<- vector(length=length(precip_mm))	# Net Energy (kJ/m2/d)

##  Initial Values.  
	SnowWaterEq[1] 	<- startingSnowDepth_m * startingSnowDensity_kg_m3 / WaterDens		
	SnowDepth[1] 	<- startingSnowDepth_m			
	Albedo[1] <- ifelse(NewSnow[1] > 0, 0.98-(0.98-0.50)*exp(-4*NewSnow[1]*10),ifelse(startingSnowDepth_m == 0, groundAlbedo, max(groundAlbedo, 0.5+(groundAlbedo-0.85)/10)))  # If snow on the ground or new snow, assume Albedo yesterday was 0.5
	S[1] <- Solar(lat=lat,Jday=JDay[1], Tx=Tmax_C[1], Tn=Tmin_C[1], albedo=Albedo[1], forest=forest, aspect=aspect, slope=slope)
	H[1] <- 1.29*(Tav[1]-SnowTemp[1])/rh[1] 
	E[1] <- lambdaV*(rhoa[1]-rhos[1])/rh[1]
	if(startingSnowDepth_m>0) TE[1] <- 0.97 
	Lt[1] <- Longwave(TE[1],SnowTemp[1])
	Energy[1] <- S[1] + La[1] - Lt[1] + H[1] + E[1] + G + P[1]
	SnowDensity[1] <- ifelse((startingSnowDepth_m+NewSnow[1])>0, min(450, (startingSnowDensity_kg_m3*startingSnowDepth_m + NewSnowDensity[1]*NewSnow[1])/(startingSnowDepth_m+NewSnow[1])), 450)
	SnowMelt[1] <- max(0,	min((startingSnowDepth_m/10+NewSnowWatEq[1]),  # yesterday on ground + today new  
				      (Energy[1]-SnowHeatCap*(startingSnowDepth_m/10+NewSnowWatEq[1])*WaterDens*(0-SnowTemp[1]))/(LatHeatFreez*WaterDens) ) )
	SnowDepth[1] <- max(0,(startingSnowDepth_m/10 + NewSnowWatEq[1]-SnowMelt[1])*WaterDens/SnowDensity[1])
	SnowWaterEq[1] <- max(0,startingSnowDepth_m/10-SnowMelt[1]+NewSnowWatEq[1])	
	
	

##  Snow Melt Loop	
	for (i in 2:length(precip_m)){
		if (NewSnow[i] > 0){ 
			Albedo[i] <- 0.98-(0.98-Albedo[i-1])*exp(-4*NewSnow[i]*10)
		} else if (SnowDepth[i-1] < 0.1){ 
			Albedo[i] <- max(groundAlbedo, Albedo[i-1]+(groundAlbedo-0.85)/10)
		} else Albedo[i] <- 0.35-(0.35-0.98)*exp(-1*(0.177+(log((-0.3+0.98)/(Albedo[i-1]-0.3)))^2.16)^0.46)

		S[i] <- Solar(lat=lat,Jday=JDay[i], Tx=Tmax_C[i], Tn=Tmin_C[i], albedo=Albedo[i-1], forest=forest, aspect=aspect, slope=slope, printWarn=FALSE)

		if(SnowDepth[i-1] > 0) TE[i] <- 0.97 	#	(-) Terrestrial Emissivity
		if(SnowWaterEq[i-1] > 0 | NewSnowWatEq[i] > 0) {
			DCoef[i] <- 6.2
			if(SnowMelt[i-1] == 0){ 
				SnowTemp[i] <- max(min(0,Tmin_C[i]),min(0,(SnowTemp[i-1]+min(-SnowTemp[i-1],Energy[i-1]/((SnowDensity[i-1]*
					SnowDepth[i-1]+NewSnow[i]*NewSnowDensity[i])*SnowHeatCap*1000)))))
			}
		}

		rhos[i] <- SatVaporDensity(SnowTemp[i])
		H[i] <- 1.29*(Tav[i]-SnowTemp[i])/rh[i] 
		E[i] <- lambdaV*(rhoa[i]-rhos[i])/rh[i]
		Lt[i] <- Longwave(TE[i],SnowTemp[i])
		Energy[i] <- S[i] + La[i] - Lt[i] + H[i] + E[i] + G + P[i]

		if (Energy[i]>0) k <- 2 else k <- 1
		
		SnowDensity[i] <- ifelse((SnowDepth[i-1]+NewSnow[i])>0, min(450, 
			((SnowDensity[i-1]+k*30*(450-SnowDensity[i-1])*exp(-DCoef[i]))*SnowDepth[i-1] + NewSnowDensity[i]*NewSnow[i])/(SnowDepth[i-1]+NewSnow[i])), 450)

		SnowMelt[i] <- max(0,	min( (SnowWaterEq[i-1]+NewSnowWatEq[i]),  # yesterday on ground + today new
				      (Energy[i]-SnowHeatCap*(SnowWaterEq[i-1]+NewSnowWatEq[i])*WaterDens*(0-SnowTemp[i]))/(LatHeatFreez*WaterDens) )  )

		SnowDepth[i] <- max(0,(SnowWaterEq[i-1]+NewSnowWatEq[i]-SnowMelt[i])*WaterDens/SnowDensity[i])
		SnowWaterEq[i] <- max(0,SnowWaterEq[i-1]-SnowMelt[i]+NewSnowWatEq[i])	# (m) Equiv depth of water
	}
	
	Results<-data.frame(Date, Tmax_C, Tmin_C, precip_mm, R_m*1000, NewSnowWatEq*1000,SnowMelt*1000, NewSnow, SnowDepth, SnowWaterEq*1000)
	colnames(Results)<-c("Date", "MaxT_C", "MinT_C", "Precip_mm", "Rain_mm", "SnowfallWatEq_mm", "SnowMelt_mm", "NewSnow_m", "SnowDepth_m", "SnowWaterEq_mm")
	return(Results)
},

SoilParCal=function(data_in,Sim_year,stationname="",dailyScale=T,validation=TRUE,return_state=F,export=F,output_dir="./",init_ratio=1){
  
  warmup<-365
  dt<-1
  exportVars<-c("U","AET")
  if(return_state) exportVars<-c("U","AET","uztwc","uzfwc" ,"lztwc" ,"lzfsc" ,"lzfpc")
  #browser()
  if(!dailyScale ) {
    
    warmup<-12
    dt<-30
  }
  
  # Define values
  Accu_cal<-NULL;Accu_val<-NULL;out_Val<-NULL
  
  HydroTestData <- as.zooreg(zoo(data_in[c("P","E","Q")], order.by = data_in$Date))
  
  # select particular time period for calibration
  da_cal<-window(HydroTestData,start = as.Date(Sim_year$Calibration[1])-years(1),end = Sim_year$Calibration[2])
  
  if(dailyScale & leap_year(as.Date(Sim_year$Calibration[1])-years(1))) warmup<-366
  
  ## an unfitted model, with ranges of possible parameter values
  modx <- hydromad(da_cal, sma = "sacramento",warmup=warmup,adimp = 0,pctim = 0,dt=dt,
	uztwc_0=init_ratio,uzfwc_0=init_ratio,lztwc_0=init_ratio,lzfsc_0=init_ratio,lzfpc_0=init_ratio,return_state=return_state)
	
   ## now try to fit it
  
  hydromad.stats("viney" = function(Q, X, ...) {
    hmadstat("r.squared")(Q, X, ...) -
      5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)
  })
  
  set.seed(0)
  fitx <- fitByOptim(modx,objective=hmadstat("viney")) #,bjective=f_KGE
  
  Output_calibrated<-window(cbind(fitx$data,fitx$U),start = Sim_year$Calibration[1],end = Sim_year$Calibration[2])[,c("P","E","Q",exportVars)]
  
  names(Output_calibrated)[1:5]<-c("Rainfall","PET","Q","Q_Sim","ET")
  
  summary(fitx)
  # Run validation period with optimized soil parameters

   
  Output_all<-predict(fitx,newdata=HydroTestData,return_state =return_state)[,exportVars]

  names(Output_all)[1:2]<-c("Q_Sim","ET")
  

  # get the validation data
  if (validation ){
    
    out_Val<-window(cbind(HydroTestData,Output_all),start = Sim_year$Validation[1],end = Sim_year$Validation[2])
    names(out_Val)[1:3]<-c("Rainfall","PET","Q")
  }
  
  # Print information
  #print(soil_pars)
  Accu_cal<-funs_nl$f_acc(Output_calibrated$Q,Output_calibrated$Q_Sim)
  print(Accu_cal)
  
  if(validation){
    Accu_val<-funs_nl$f_acc(out_Val$Q,out_Val$Q_Sim)
    print(Accu_val)
  }
  
  # Getting the new numeric goodness of fit
  for(pred in c("Calibration","Validation")){

    if(pred=="Calibration"){
      Simulated<-Output_calibrated$Q_Sim
      Observed<-Output_calibrated$Q  
      da_dates<-index(Output_calibrated)
    }else if(pred== "Validation" & validation){
      Simulated<-out_Val$Q_Sim
      Observed<-out_Val$Q
      da_dates<-index(out_Val)
      if(length(Observed)<24) next()
    }

    if(pred== "Validation" & !validation) next()

    if(dailyScale){
      pdf(paste0(output_dir,"Q_daily_",stationname,"_",pred,".pdf"),width = 9,height = 9)
      ggof(sim=Simulated, obs=Observed,dates=da_dates,ylab = "Flow (mm)",main =stationname,ftype = "dma",FUN = "sum",gofs=c( "RMSE", "PBIAS", "NSE","KGE", "R2"))
      dev.off()

    }else{
      pdf(paste0(output_dir,"Q_",stationname,"_",pred,".pdf"),width = 9,height = 6)
      ggof(sim=Simulated, obs=Observed,dates=da_dates,ylab = "Flow (mm)",main =stationname,ftype = "ma",FUN = "sum",gofs=c( "RMSE", "PBIAS", "NSE", "KGE","R2"))
      dev.off()

    }
    
  }
  if(export){
    return(list("FitResult"=fitx,"SimulateResult"=Output_all,"Accu_cal"=Accu_cal,"Accu_val"=Accu_val,Output_calibrated=Output_calibrated))
  }else{
    return(list("Accu_cal"=Accu_cal,"Accu_val"=Accu_val))
  }
},
  
  RunWaSSI=function(data_in,soil_pars,forestType="DBF"){

	result_SACSMA<-dWaSSI$WaSSI(data_in,soil_pars,forest = forestType)

	result_daily<-result_SACSMA%>%
	  right_join(data_in[,c("Date","Q")],by="Date")%>%
	  mutate(Q_sim=WaYldTot)%>%
	  filter(Date>= min(result_SACSMA$Date)+years(1))

	result_month<-result_daily%>%
	  mutate(Year=year(Date),Month=month(Date))%>%
	  group_by(Year,Month)%>%
	  summarise(across(c("Rainfall","Ei","Es","Ec","ET","WaYldTot","Q","Q_sim","GPP","GPP_SD"),.fns = sum,na.rm=T))%>%
	  mutate(Date=make_date(Year,Month,"01"))

	result_ann<-result_daily%>%
	  mutate(Year=year(Date))%>%
	  group_by(Year)%>%
	  summarise(across(c("Rainfall","PT","Ei","Es","Ec","ET","WaYldTot","Q","Q_sim","GPP","GPP_SD"),.fns = sum,na.rm=T))%>%
	  mutate(Tr_ET=Ec/ET)%>%
	  mutate(Pbias=(Q_sim-Q)/Q*100)

# Validation parameters
	val_par_daily<-funs_nl$f_acc(result_daily$Q,result_daily$Q_sim)

	val_par_monthly<-funs_nl$f_acc(result_month$Q,result_month$Q_sim)

	val_par_annual<-funs_nl$f_acc(result_ann$Q,result_ann$Q_sim)

	# Plots
	p1<-result_daily%>%
	  ggplot(aes(x=Q,y=Q_sim))+geom_point()+geom_smooth(method = "lm")+coord_equal()+labs(x="Q Observed",y="Q Simulated")+theme_bw()

	p2<-result_month%>%
	  ggplot(aes(x=Q,y=Q_sim))+geom_point()+geom_smooth(method = "lm")+coord_equal()+labs(x="Q Observed",y="Q Simulated")+theme_bw()

	p3<-result_month%>%
	  ggplot(aes(x=Date))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+scale_x_date(date_breaks ="1 year",date_labels = "%Y")+labs(x="Date",y="Flow (mm)")+theme_bw()

	p4<-result_ann%>%
	  ggplot(aes(x=Year))+geom_line(aes(y=Q,color="Observed"))+
	  geom_line(aes(y=Q_sim,color="Simulated"))+scale_color_manual(name="Legend",values = c("black","red"),breaks=c("Observed","Simulated"))+labs(x="Year",y="Flow (mm)")+scale_x_continuous(breaks = c(seq(1980,2022,1)))+theme_bw()

	Monthly_avg<-result_month%>%
	  ungroup()%>%
	  dplyr::select(-Date,-Year)%>%
	  group_by(Month)%>%
	  summarise(across(.fns = mean))
	
	Annual_avg<-result_ann%>%
	  select(-Year)%>%
	  summarise(across(.fns = mean))
	  
	  
	return(list(daily=result_daily,monthly=result_month,annual=result_ann,monthly_avg=Monthly_avg,
		  annual_avg=Annual_avg,
		  Accuarcy=list(val_par_daily=val_par_daily,val_par_monthly=val_par_monthly,val_par_annual=val_par_annual),
		  Figs=list(daily=p1,monthly=p2,monthly_lines=p3,annual=p4)))
	  }

)


fn_NFS<-list(

# Function for accpop of intakes
AccPopulation=function(da_in,Pct_field="Pct",IBT=FALSE){
  
    da_sta<-data.frame("Group"=c(">0%",">10%",">20%",">30%",">40%",">50%",">60%",">70%",">80%",">90%"),"Population"=NA)
    
	intervlas<-c(0,10,20,30,40,50,60,70,80,90)
	
	da_in$Pct<-Pct[Pct_field]
	
  	if(IBT) da_sta["PopuIBT"]<-NA
  
  	for (j in c(1:length(intervlas))) {
  	  da_sta$Population[j]<-sum(da_in$Population[da_in$Pct>intervlas[j]],na.rm=T)
  	  
  	  if(IBT) da_sta$PopuIBT[j]<-sum(da_in$Population[da_in$Pct_post>intervlas[j]],na.rm=T)
  	} 

  return(da_sta)
  
},

# Cut to 0-10
PctCut=function(pct){
  as.character(cut(pct,c(0,10,25,50,75,100),labels = c(">0 - 10","11 - 25","26 - 50","51 - 75","76 - 100"),include.lowest = F, right = T))
}



)

fn_GEE<-list(

	## Read the zonal climate from GEE ----
	#' @param filename The csv file from GEE.
	#' @param dataSource The data source ("Terra","Daymet","PRISM").
	#' @param dataScale The timestep of for ouput data ("Daily","Monthly").
	#' @keywords Climate
	#' @export
	#' @examples
	#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
	#' f_readGEEClimate(filename)
	readClimate=function(filename,dataSource="Terra",dataScale="Monthly"){
	  
	  require("dplyr")
	  require("lubridate")
	  
	  da<-read.csv(filename)%>%
		mutate(Date=as.Date(as.character(date),"%Y%m%d"))%>%
		dplyr::select(-one_of(c("system.index","date",".geo")))
	  
	  if(dataSource=="Terra"){
		da<-da%>%
		  mutate(Year=year(Date),Month=month(Date))%>%
		  dplyr::rename(Ppt_mm=pr,Tmin_C=tmmn,Tmax_C=tmmx,swe_mm=swe,ET0=pet)%>%
		  mutate(Tavg_C=(Tmin_C+Tmax_C)/20,ET0=ET0/10)%>%
		  mutate(Tmax_C=Tmax_C/10,Tmin_C=Tmin_C/10)
		
	  }else if(dataSource=="Daymet"){
		
		da<-da%>%
		  mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
		  mutate(Tavg_C=(tmax+tmin)/2)%>%
		  dplyr::rename(Ppt_mm=prcp,Tmin_C=tmin,Tmax_C=tmax,swe_kgm2=swe,vp_Pa=vp,dayl_s=dayl,srad_Wm2=srad)
		
		if(dataScale=="Monthly"){
		  da<-da%>%
			group_by(WS_ID,Year,Month)%>%
			summarise(Ppt_mm=sum(Ppt_mm),swe_kgm2=sum(swe_kgm2),dayl_s=sum(dayl_s),Tavg_C=mean(Tavg_C),Tmax_C=mean(Tmax_C),Tmin_C=mean(Tmin_C),vp_Pa=mean(vp_Pa),srad_Wm2=mean(srad_Wm2))
		}
		
	  }else if(dataSource=="PRISM"){
		da<-da%>%
		  mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
		  dplyr::rename(Tavg_C=tmean,Ppt_mm=ppt,Tmin_C=tmin,Tmax_C=tmax,Tdavg_C=tdmean,vpdmin_hPa=vpdmin,vpdmax_hPa=vpdmax)
		
		if(dataScale=="Monthly"){
		  da<-da%>%
			group_by(WS_ID,Year,Month)%>%
			summarise(Ppt_mm=sum(Ppt_mm),Tavg_C=mean(Tavg_C),Tmax_C=mean(Tmax_C),Tmin_C=mean(Tmin_C),Tdavg_C=mean(Tdavg_C),vpdmin_hPa=mean(vpdmin_hPa),vpdmax_hPa=mean(vpdmax_hPa))
		}
		
	  }
	  
	  return(da)
	  
	},
	## Read the zonal 8days LAI from GEE ----
	#' @param filename The csv file from GEE.
	#' @param dataScale The timestep of for ouput data ("Daily","8days,"Monthly").
	#' @keywords LAI
	#' @export
	#' @examples
	#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
	#' read_GEE8DayLAI(filename)
	read8DayLAI=function(filename,TimeScale="8days"){
	  require(tidyverse)
	  require(dplyr)
	  require(lubridate)
	  da_gee<-read.csv(filename)%>%
		dplyr::select(-.geo,-system.index)%>%
		pivot_longer(cols =starts_with("X20"),names_to="Info",names_prefix = "X",values_to ="LAI")%>%
		mutate(Date=as.Date(as.character(Info),"%Y%j"))%>%
		mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
		dplyr::select(-Info)
	  
	  da_gee

	},
	## Read the zonal data of S2 from GEE ----
	# https://github.com/rfernand387/LEAF-Toolbox/wiki
	#' @param filename The csv file from GEE.
	#' @param VarName one of // 'Albedo', 'fAPAR','FCOVER','LAI','CWC','CCC'
	#' @keywords LAI
	#' @export
	#' @examples
	#' filename<-"E:/Research/WaSSI/Turkey/TerraClimate_CB.csv"
	#' read_S2(filename)
	read_S2=function(filename,VarName="LAI"){
	  require(tidyverse)
	  require(dplyr)
	  require(lubridate)
	  da_gee<-read.csv(filename)%>%
		dplyr::select(-.geo,-system.index)%>%
		pivot_longer(cols =starts_with("X20"),names_to="Info",names_prefix = "X",values_to =VarName)%>%
		mutate(Date=as.Date(substr(as.character(Info),1,8),"%Y%m%d"))%>%
		mutate(Year=year(Date),Month=month(Date),Day=day(Date))%>%
		dplyr::select(-Info)%>%
		filter(!is.na(get(VarName)))
	  
	  da_gee
	  
	},

	read_all=function(siteName,WS_Field=NULL,dir="./"){
	  require(readr)
	  
	  PRISM_name<-paste0(dir,"Climate_",siteName,".csv")
	  Daymet_name<-paste0(dir,"Climate_Daymet_daily_",siteName,".csv")
	  LAI_8days_name<-paste0(dir,"LAI_8days_2000_2020_",siteName,".csv")
	  SoiPar_name<-paste0(dir,"Soil_",siteName,".csv")
	  Imp_name<-paste0(dir,"Imp_",siteName,".csv")
	  LAI_s2_name<-paste0(dir,"LAI_S2_",siteName,".csv")
	  
	  da_PRISM<-da_Daymet<-da_LAI_8days<-da_Soil<-da_Imp<-da_LAI_S2<-da_Albedo_S2<-NULL
	  
	  Albedo_s2_name<-paste0(dir,"Albedo_S2_",siteName,".csv")
	  if(file.exists(PRISM_name)) da_PRISM<-read_csv(PRISM_name) %>% dplyr::select(-"system:index",-".geo") %>% mutate(Date=as.Date(as.character(date),"%Y%m%d"))
	  
	  if(file.exists(Daymet_name)){
		  da_Daymet<-read_csv(Daymet_name) %>% 
			dplyr::select(-"system:index",-".geo") %>%   
			mutate(Date=as.Date(as.character(date),"%Y%m%d")) %>%
			mutate(Rs=srad* dayl/1000000,n=dayl/60/60)%>% # MJ/m2/day
			mutate(Tmax=tmax,Tmin=tmin)%>%
			mutate(J=yday(Date),Year=year(Date),Month=month(Date),Day=day(Date))%>%
			mutate(Date.daily=Date)%>%
			mutate(va=vp/1000, #kpa
				  vs_Tmax=0.6108 * exp(17.27 * Tmax/(Tmax +237.3)),
				  vs_Tmin =0.6108 * exp(17.27 * Tmin/(Tmin +237.3)))%>%
			mutate(vs=(vs_Tmax + vs_Tmin)/2,VPD=vs-va)
		}
		
	   if(file.exists(LAI_8days_name)) da_LAI_8days<-funs_nl$read_GEE8DayLAI(LAI_8days_name)

	  if(file.exists(SoiPar_name)) da_Soil<-read_csv(SoiPar_name) %>% dplyr::select(-"system:index",-".geo")%>% mutate(adimp=0,pctim=0) %>% as.list()
	  
	  if(file.exists(Imp_name)) da_Imp<-read_csv(Imp_name) %>% dplyr::select(-"system:index",-".geo") 
	  
  	 if(file.exists(LAI_s2_name)) da_LAI_S2<-funs_nl$read_GEE_S2(LAI_s2_name) 
	  
	  if(file.exists(Albedo_s2_name)) da_Albedo_S2<-funs_nl$read_GEE_S2(Albedo_s2_name,VarName = "Albedo") 
	  
	  return(list(Site=siteName,Climate_PRISM=da_PRISM,Climate_Daymet=da_Daymet,LAI_8days=da_LAI_8days,Impervious=da_Imp,SoilPar=da_Soil,LAI_S2=da_LAI_S2,Albedo_S2=da_Albedo_S2))
	  
	  },
	  
	  s2_DailyLAI=function(da){
	   da%>% 
		filter(LAI>0)%>%
		mutate(DOY=yday(Date),Year=year(Date))%>%
		funs_nl$f_fit_AG(Var="LAI")%>%
		mutate(Date=as.Date(paste0(Year,DOY),"%Y%j"))%>%
		dplyr::select(-DOY)%>%
		dplyr::rename(LAI=Filled)%>%
		dplyr::select(Date,LAI)
	  
	},

	s2_DailyAlbedo=function(da){
	   da%>% 
		filter(Albedo>0)%>%
		mutate(DOY=yday(Date),Year=year(Date))%>%
		funs_nl$f_fit_AG(Var="Albedo")%>%
		mutate(Date=as.Date(paste0(Year,DOY),"%Y%j"))%>%
		dplyr::select(-DOY)%>%
		dplyr::rename(Albedo=Filled)%>%
		dplyr::select(Date,Albedo)
	  
	},
	DaymetPT=function(daymetClimate,lat,elevation,.alpha=0.23,Albedo=F){
		data("constants",package="Evapotranspiration")
		constants$lat<-lat
		constants$lat_rad<-lat*pi/180
		constants$Elev<-elevation
		if(Albedo){
		  .alpha<-daymetClimate$Albedo
		  funs_nl$f_ET.PT(data = daymetClimate,constants = constants, ts = "daily", solar = "data", alpha = .alpha,message=F)$ET.Daily
		  
		}else{
		  funs_nl$f_ET.PT(data = daymetClimate,constants = constants, ts = "daily", solar = "data", alpha = .alpha,message=T)$ET.Daily
		}
		


},
	Rn_Daymet=function(data,constants,lat,elevation,.alpha=0.23){
		
		data("constants",package="Evapotranspiration")
		constants$lat<-lat
		constants$lat_rad<-lat*pi/180
		constants$Elev<-elevation
		
		data$Ta <- (data$Tmax + data$Tmin)/2
		P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
		delta <- 4098 * (0.6108 * exp((17.27 * data$Ta)/(data$Ta + 237.3)))/((data$Ta +
			237.3)^2)
		gamma <- 0.00163 * P/constants$lambda
		d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
		delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
		w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
		N <- 24/pi * w_s
		R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
			sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
			sin(w_s))
		R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
		R_s <- data$Rs
		R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(data$va)) * ((data$Tmax +
			273.2)^4 + (data$Tmin + 273.2)^4)/2 * (1.35 * R_s/R_so -
			0.35)
		R_nsg <- (1 - .alpha) * R_s
		R_ng <- R_nsg - R_nl
		
		return(list(Rn=R_ng,Rns=R_nsg,Rnl=R_nl))
	}
	  
)


fn_ECOSTRESS<-list(

	#Function for downloading data for a request from https://appeears.earthdatacloud.nasa.gov
	AppEEARX_download=function(task_id,task_name,user="",password=""){
	  ###-----------------------------
	  # This function is used to download all data from a request on "https://appeears.earthdatacloud.nasa.gov"
	  ###-----------------------------

	  require(httr)
	  require(jsonlite)

	  ## function for downloading each file
	  download_withAPI<-function(id,dest_dir){
		file_id<-filelist$file_id[id]
		filename<-filelist$file_name[id]
		token <- paste("Bearer", fromJSON(token_response)$token)

		# create a destination directory to store the file in
		if(!dir.exists(dest_dir)) dir.create(dest_dir)
		filepath <- paste(dest_dir, filename, sep = '/')
		suppressWarnings(dir.create(dirname(filepath)))

		# write the file to disk using the destination directory and file name
		response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id, sep = ""),
						write_disk(filepath, overwrite = TRUE), progress(), add_headers(Authorization = token))
	  }


	  # Connect to the servier
	  if(user==""| password=="") return("Username and Password are required for https://appeears.earthdatacloud.nasa.gov")
	  secret <- base64_enc(paste(user, password, sep = ":"))
	  response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login",
					   add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
								   "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
					   body = "grant_type=client_credentials")
	  token_response <- prettify(toJSON(content(response), auto_unbox = TRUE))

	  token <- paste("Bearer", fromJSON(token_response)$token)

	  response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), add_headers(Authorization = token))
	  bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))

	  # Get the file list
	  filelist <- fromJSON(bundle_response)$files

	  # download each file
	  lapply(1:nrow(filelist), download_withAPI,dest_dir=task_name)

	# Log out
		token <- paste("Bearer", fromJSON(token_response)$token)
		response <- POST("https://appeears.earthdatacloud.nasa.gov/api/logout",
						 add_headers(Authorization = token,
									 "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
						 body = "grant_type=client_credentials")
		
	},

# Function for masking cloud of ECOSTRESS data
## Function for get the image list
Datalist=function(dir_Cloud=NULL,dir_LST=NULL,dir_ET=NULL){

	library(stringr)
	require(lubridate)
	require(stringr)
	require(dplyr)
	require(tidyverse)
	
	Datalist<-NULL
	
	if(!is.null(dir_Cloud)) {
		files_Cloud<-dir(dir_Cloud,".tif")
		Cloud_list<-data.frame("filename"=files_Cloud)%>%
		  mutate(Var=str_match(filename, "SDS_(.*?)_doy")[,2])%>%
		  mutate(Name=str_match(filename, "_doy(.*?)_aid0001")[,2])%>%
		  as.data.frame()
		  
		Datalist<-rbind(Datalist,Cloud_list)
	}
	
	if(!is.null(dir_LST)) {

		files_LST<-dir(dir_LST,".tif")
		LST_list<-data.frame("filename"=files_LST)%>%
		  mutate(Var=str_match(filename, "SDS_(.*?)_doy")[,2])%>%
		  mutate(Name=str_match(filename, "_doy(.*?)_aid0001")[,2])%>%
		  as.data.frame()
		  
		Datalist<-rbind(Datalist,LST_list)
	}
	if(!is.null(dir_ET)){
		files_ET<-dir(dir_ET,".tif")
		ET_list<-data.frame("filename"=files_ET)%>%
		 mutate(Var=str_match(filename, "PT_JPL_(.*?)_doy")[,2])%>%
		 mutate(Name=str_match(filename, "_doy(.*?)_aid0001")[,2])%>%
		 as.data.frame()
		 
		Datalist<-rbind(Datalist,ET_list)
	}	

	Datalist<-Datalist%>%
	  pivot_wider(id_cols = Name,names_from = Var,values_from = filename,values_fill = NA)%>%
	  mutate(UTC=parse_date_time(Name,"%Y%j%H%M%S",tz="UTC"))%>%
	  mutate(Timestamp=with_tz(UTC,tz="EST"))
	  
	return(Datalist)
	
	},

## Read cloud info
	readEcosCloud=function(ID,Dir_cloud="ECOSTRESS/data/Cloud/",plot=T,shp=NULL){
	  
	  filename<-paste0(Dir_cloud,"ECO2CLD.001_SDS_CloudMask_doy",ID,"_aid0001.tif")
	  #print(filename)
	  if(!file.exists(filename)) return()

	  # try to open the raster
	  a<-try(raster(filename),silent=T)
	  if(inherits(a, "try-error")) stop(paste0("Bad raster - ",ID))
	  da<-raster(filename)
	  
	  # mask data by it QC
	  values(da)[values(da)>1 | values(da)<0]<- 0 # Clear sky - value==1
	  values(da)[is.na(values(da))]<- 0
	  values(da)<-as.integer(values(da))
	  
	  # plot if there is valid pixels
	  if(sum(values(da)==1)>0) {
		print(ID)
		arg <- list(at=c(0.5,1.3), labels=c("Cloudy","Clear"))
		if(plot){
		plot(da,main=paste0("Cloud - ",ID,"\n",Datalist$Timestamp[Datalist$Name==ID]),breaks=c(-0.1,0.9,1.8), col=c("gray","skyblue"), axis.args=arg,zlim=c(-0.1,1.5))
	   if(!is.null(shp)) plot(shp[,1],color=0,add=T)
		}
		
		names(da)<-paste0("CloudMask",ID)
		return(da)
	  }
	  
	},

# Function for reading Ecostress LST variables
	readEcosLST=function(ID,var,LST_QC_lookup,da_dir,mask=F,plot=T,shp=NULL){
	  #LST_QC_lookup "ECOSTRESS/data/Lookups/ECO2LSTE-001-SDS-QC-lookup.csv"
	  filename<-paste0(da_dir,"ECO2LSTE.001_SDS_",var,"_doy",ID,"_aid0001.tif")
	  LST_QC_lookup<-read.csv(LST_QC_lookup)%>%
		filter(Mandatory.QA.flags=="Pixel produced, best quality")%>%
		filter(LST.accuracy!=">2 K (Poor performance)")
	  #print(filename)
	  if(!file.exists(filename)) return()
	  #options(show.error.messages = FALSE)
	  # try to ooen the raster
	  a<-try(raster(filename),silent=T)
	  if(inherits(a, "try-error")) stop(paste0("Bad raster - ",ID))
	  if(is.character(a)) next()
	  da<-raster(filename)
	  
	  # mask data by it QC
	  if(var=="LST"){
		values(da)[values(da)>65535 | values(da)<7500]<- NA
		values(da)<-values(da)*0.02-273.15 
	  }else if (var=="QC"){
		values(da)[!values(da) %in% LST_QC_lookup$Value]<- NA
		
	  }else if(var=="LST_err"){
		values(da)[values(da)==0]<- NA
		values(da)<-values(da)*0.04 
	  }

	  # Mask the variable by QC
	  if(mask & var!="QC"){
		da_qc<-raster(paste0(da_dir,"ECO2LSTE.001_SDS_QC_doy",ID,"_aid0001.tif"))
		values(da_qc)[!values(da_qc) %in% LST_QC_lookup$Value]<- NA
		da<-mask(da,da_qc)
	  }

	  # plot if there is valid pixels
	  if(!is.null(shp)) {
		da<-crop(da,shp)
		values_no<-sum(!is.na(values(mask(da,shp))))
	  }else{
		values_no<-sum(!is.na(values(da)))
	  }
		
	  if(values_no>1) {
		print(ID)
		
		if(plot){
			plot(da,main=paste0(var," - ",ID,"\n",Datalist$Timestamp[Datalist$Name==ID]))
		   if(!is.null(shp)) {
			 plot(shp[,1],color=0,add=T)
		   }
		}
		
		names(da)<-paste0(var,ID)
		return(da)
		}
	  },
	  
	  readEcosET=function(ID,var,LST_QC_lookup,da_dir,mask=F,da_LST_dir=NULL,plot=T,shp=NULL){
	  
	  filename<-paste0(da_dir,"ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_",var,"_doy",ID,"_aid0001.tif")
	  LST_QC_lookup<-read.csv(LST_QC_lookup)%>%
		filter(Mandatory.QA.flags=="Pixel produced, best quality")%>%
		filter(LST.accuracy!=">2 K (Poor performance)")
	  # 
	  # LST_QC_lookup<-read.csv("ECOSTRESS/data/Lookups/ECO2LSTE-001-SDS-QC-lookup.csv")%>%
	  #   filter(Mandatory.QA.flags=="Pixel produced, best quality")
	  #print(filename)
	  if(!file.exists(filename)) return()
	  #options(show.error.messages = FALSE)
	  # try to ooen the raster
	  a<-try(raster(filename),silent=T)
	  if(inherits(a, "try-error")) stop(paste0("Bad raster - ",ID))
	  if(is.character(a)) next()
	  da<-raster(filename)
	  
	  # mask data by it QC
	  if(var=="ETinst" | var=="ETdaily" | var=="ETinstUncertainty"){
		values(da)[values(da)>2000 | values(da)<0]<- NA
	  }else if (var=="ETcanopy"| var=="ETsoil" | var=="ETinterception"){
		
		values(da)[values(da)>100 | values(da)<0]<- NA
		
	  }

	  # Mask the variable by QC
	  if(mask & var!="QC" & !is.null(da_LST_dir)){
		da_qc<-raster(paste0(da_LST_dir,"ECO2LSTE.001_SDS_QC_doy",ID,"_aid0001.tif"))
		values(da_qc)[!values(da_qc) %in% LST_QC_lookup$Value]<- NA
		da<-mask(da,da_qc)
	  }

	  # plot if there is valid pixels
	  if(!is.null(shp)) {
		da<-crop(da,shp)
		values_no<-sum(!is.na(values(mask(da,shp))))
	  }else{
		values_no<-sum(!is.na(values(da)))
	  }
	  
	  if(values_no>1) {
		print(ID)
		
		if(plot){
		plot(da,main=paste0(var," - ",ID,"\n",Datalist$Timestamp[Datalist$Name==ID]))
	   if(!is.null(shp)) plot(shp[,1],color=0,add=T)
		}
		
		names(da)<-paste0(var,ID)
		return(da)
	  }
	  
	},
	
	# delete no intersection images
	rmNULLImages=function(dalist,shp){
		f_intect<-function(x){
		if(is.null(x)) return(NULL) 
		x<-mask(x,shp)
		if(sum(!is.na(values(x)))==0) return(NULL) 
		crop(x,shp)
		}
	  aa<-lapply(dalist,f_intect) 
	  aa[sapply(aa, is.null)] <- NULL
	  a1<-lapply(aa,function(x) names(x))
	  names(aa)<-a1
	  return(aa)
	}
	
)
