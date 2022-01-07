/*
Author: Ning Liu (ln1267@gmail.com)

This code is used fro processing input data for WaSSI model using GEE and some default dataset. Currently, it works best for the USA.

USE:

	var WaSSI = require('users/ln1267/default:WaSSI_input_common')

INPUTS:
        - LAI_USA_8days_*: <image>
				this is the LAI data downloaded from BNU 8-day global LAI product fro 2001 to 2020
				
        - Imp_MX: <image>
                      This is the impervious data fro Mexico

        - Imp_USA: <image>
                    This is the impervious for USA from NLCD_2016_Impervious

        - soil_WaSSI_stacks_USA: <image>
                    This is the soil parameters for SAC-SMA 
					
        - US_lai_monthly_avg_2000_2019: <image>
			This is the long term monthly LAI

OUTPUTS:
        - RasterizeWS
			This is for rasterizing watershed bounday to a raster
			
         - zonalTS
			Function for zonal image collection by watersheds
			
		- zonalImg
			Function for zonal image bands by watersheds
		
		  images with bands:
          - landsat original bands: all from SR excpet the TIR bands (from TOA) 
          - cloud masked
          - 'NDVI': normalized vegetation index
          - 'FVC': fraction of vegetation cover [0-1]
          - 'TPW': total precipitable water [mm]
          - 'EM': surface emissvity for TIR band
          - 'LST': land surface temperature
*/

// Process original data to the target---------------------------------
function namerange(yr){
  
  var days=['001','009','017','025','033','041','049','057','065','073','081','089','097','105','113','121','129','137','145','153','161','169','177','185','193','201','209','217','225','233','241','249','257','265','273','281','289','297','305','313','321','329','337','345','353','361']
  var ans=[]
  for(var i=0;i<=45;i++) { ans.push(yr+days[i])}
  //for(var i=0;i<=45;i++) { ans.push('B'+yr+days[i])}
  return ans
}  


var  LAIbands=['b1','b2','b3','b4','b5','b6','b7','b8','b9','b10','b11','b12','b13','b14','b15','b16','b17','b18','b19','b20','b21','b22','b23','b24','b25','b26','b27','b28','b29','b30','b31','b32','b33','b34','b35','b36','b37','b38','b39','b40','b41','b42','b43','b44','b45','b46']

// Merge 8days LAI all Time series 
exports.LAI_8days_2000_2020=ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2000')
        .select(LAIbands,namerange(2000))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2001')
        .select(LAIbands,namerange(2001)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2002')
        .select(LAIbands,namerange(2002)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2003')
        .select(LAIbands,namerange(2003)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2004')
        .select(LAIbands,namerange(2004)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2005')
        .select(LAIbands,namerange(2005)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2006')
        .select(LAIbands,namerange(2006)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2007')
        .select(LAIbands,namerange(2007)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2008')
        .select(LAIbands,namerange(2008)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2009')
        .select(LAIbands,namerange(2009)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2010')
        .select(LAIbands,namerange(2010)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2011')
        .select(LAIbands,namerange(2011)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2012')
        .select(LAIbands,namerange(2012)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2013')
        .select(LAIbands,namerange(2013)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2014')
        .select(LAIbands,namerange(2014)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2015')
        .select(LAIbands,namerange(2015)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2016')
        .select(LAIbands,namerange(2016)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2017')
        .select(LAIbands,namerange(2017)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2018')
        .select(LAIbands,namerange(2018)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2019')
        .select(LAIbands,namerange(2019)))
        .addBands(ee.Image('projects/ee-ln1267/assets/LAI_USA_8days_2020')
        .select(LAIbands,namerange(2020)))

// ------------------dataset-------------------

// Mosia impervious for USA and MX
var Imp_USA = ee.Image('projects/ee-ln1267/assets/NLCD_2016_Impervious')
var Imp_MX = ee.Image('projects/ee-ln1267/assets/MEX_GMIS_impervious')

var Imp_USA=Imp_USA.updateMask(Imp_USA.lte(100))
var Imp_MX=Imp_MX.updateMask(Imp_MX.lte(100))

exports.Impervious =Imp_USA.addBands(Imp_MX).reduce(ee.Reducer.first());

// Rename Soil
exports.SOIL_SAC=ee.Image("users/ln1267/USA_WaSSI/soil_WaSSI_stacks_USA")
        .select(['b1','b2','b3','b4','b5','b6','b7','b8','b9','b10','b11'],
        ['uztwm','uzfwm','uzk','zperc','rexp','lztwm','lzfsm','lzfpm','lzsk','lzpk','pfree'])

// Rename LAI avg
exports.LAI_avg=ee.Image("users/ln1267/USA_WaSSI/US_lai_monthly_avg_2000_2019")
        .select(['b1','b2','b3','b4','b5','b6','b7','b8','b9','b10','b11','b12'],
        ['01','02','03','04','05','06','07','08','09','10','11','12'])





// ------------------Functions for Zonal Img-------------------

// Make an image from shp id.
exports.RasterizeWS=function(shp,field){
var StateImg = shp
  .reduceToImage({
    properties: [field],
    reducer: ee.Reducer.first()
});
return StateImg;
}

// Zonal image by Bands
exports.zonalImg=function(img,Watersheds,scale){
  
  var table=img.reduceRegions(Watersheds, ee.Reducer.mean(), scale, "EPSG:4326");

  table = table
        .map(function(feature) {
           return ee.Feature(feature.select([".*"], null, false))
        });
		
	return table;
}	

// Zonal image collections
exports.zonalTS=function(imgcol,Watersheds,startdate,enddate){
  
  var table = imgcol
      .filterDate(startdate,enddate)
      .map(function(img) {
        // Add the date as a number.
        var dateStr = img.date();
        var dateNum = ee.Number.parse(dateStr.format("YYYYMMdd"));
        img = img.addBands(ee.Image(dateNum).rename('date'));
        return img.reduceRegions(Watersheds, ee.Reducer.mean(), 100, "EPSG:4326")
      }).flatten();

  table = table
        .map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});

  return table
}

// ---------------------------Functions for zonal by group---------------------------

// Convert bands to img collection 
exports.bandToCollection = function(collection){
  // get band names
  var bands =  collection.bandNames()
  var dayCounter = ee.List.sequence(1, bands.size())

  // build new collection with 1 image per band
  var newCollection = dayCounter.map(function(b){
  
  var img = ee.Image(collection.select(ee.String(bands.get(ee.Number(b).subtract(1)))))
  
  return(img.set('system:index',bands.get(ee.Number(b).subtract(1))))

  })

  return newCollection
}


// Zonal data by HUC and LC
exports.zonalGroupLAI=function(img){
  
  // huc_lc (image) and Region (feature) are required
  
  var nlDiff = img.addBands(huc_lc);

  // Grouped a mean reducer: change of nightlights by land cover category.
  var means = nlDiff.reduceRegion({
    reducer: ee.Reducer.mean().group({
      groupField: 1,
      groupName: 'code',
    }),
    geometry: Region,
    scale: 100,
    maxPixels: 1e13
  });
  
  return(ee.Feature(null,means));
}

/* example

var lai_avg_zonal=ee.ImageCollection(bandToCollection(LAI_avg))
            .map(zonalGroupLAI);

*/

