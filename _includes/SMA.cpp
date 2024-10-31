// SMA.cpp
// C++ Implementation of the (dWaSSI model)
// Author: Ning Liu
// Date: 2024-10-30

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;
// Revised SAC-SMA model
// [[Rcpp::export]] 
DataFrame SMA(
    NumericVector prcp,
    NumericVector pet,
    NumericVector par,
    Nullable<NumericVector> pet_Soil = R_NilValue,
    bool SoilEvp = false,
    NumericVector ini_states = NumericVector::create(0, 0, 500, 500, 500, 0)
) {
  // Check if all required parameters are present
  std::vector<std::string> required_params = {"UZTWM", "UZFWM", "UZK", "ZPERC", "REXP",
                                              "LZTWM", "LZFSM", "LZFPM", "LZSK", "LZPK", "PFREE"};
  for (auto &param : required_params) {
    if (!par.containsElementNamed(param.c_str())) { // Use c_str() if necessary
      stop(std::string("Input soil parameter '") + param + "' is missing");
    }
  }
  
  // Extract parameters
  double uztwm  = par["UZTWM"];    // Upper zone tension water capacity [mm]
  double uzfwm  = par["UZFWM"];    // Upper zone free water capacity [mm]
  double lztwm  = par["LZTWM"];    // Lower zone tension water capacity [mm]
  double lzfpm  = par["LZFPM"];    // Lower zone primary free water capacity [mm]
  double lzfsm  = par["LZFSM"];    // Lower zone supplementary free water capacity [mm]
  double uzk    = par["UZK"];      // Upper zone free water lateral depletion rate [1/day]
  double lzpk   = par["LZPK"];     // Lower zone primary free water depletion rate [1/day]
  double lzsk   = par["LZSK"];     // Lower zone supplementary free water depletion rate [1/day]
  double zperc  = par["ZPERC"];    // Percolation demand scale parameter [-]
  double rexp   = par["REXP"];     // Percolation demand shape parameter [-]
  double pfree  = par["PFREE"];    // Percolating water split parameter (decimal fraction)
  
  // Initialize additional parameters (set to zero as in R)
  double pctim  = 0.0; // Impervious fraction of the watershed area
  double adimp  = 0.0; // Additional impervious areas
  double riva   = 0.0; // Riparian vegetation area
  double side   = 0.0; // The ratio of deep recharge to channel base flow
  double rserv  = 0.0; // Fraction of lower zone free water not transferrable
  
  // Initial Storage States (SAC-SMA)
  double uztwc = uztwm;   // Upper zone tension water storage
  double uzfwc = uzfwm;   // Upper zone free water storage
  double lztwc = lztwm;   // Lower zone tension water storage
  double lzfsc = lzfsm;   // Lower zone supplementary free water storage
  double lzfpc = lzfpm;   // Lower zone primary free water storage
  double adimc = 0.0;     // Additional impervious area storage
  
  // Number of time steps
  int n = prcp.size();
  
  // Initialize output vectors
  NumericVector simaet(n, 0.0);          // Total ET
  NumericVector simaetSoil(n, 0.0);      // ET for Soil total
  NumericVector simaetSoil_1(n, 0.0);    // ET for Soil surface for soil tension
  NumericVector simaetSoil_2(n, 0.0);    // ET for Soil surface for soil free water
  NumericVector simaet1(n, 0.0);         // ET for Veg from upper for soil tension
  NumericVector simaet2(n, 0.0);         // ET for Veg from upper for soil free water
  NumericVector simaet3(n, 0.0);         // ET for Veg from lower soil for soil tension
  NumericVector simaet4(n, 0.0);         // ET for Veg from lower soil for soil free water
  NumericVector simaet5(n, 0.0);         // ET from additional impervious area
  NumericVector simflow(n, 0.0);         // Total outflow
  NumericVector base_tot(n, 0.0);        // Baseflow
  NumericVector surf_tot(n, 0.0);        // Surface runoff
  NumericVector interflow_tot(n, 0.0);   // Interflow
  NumericVector uztwc_ts(n, 0.0);         // Upper zone tension water storage time series
  NumericVector uzfwc_ts(n, 0.0);         // Upper zone free water storage time series
  NumericVector lztwc_ts(n, 0.0);         // Lower zone tension water storage time series
  NumericVector lzfpc_ts(n, 0.0);         // Lower zone primary free water storage time series
  NumericVector lzfsc_ts(n, 0.0);         // Lower zone supplementary free water storage time series
  
  double thres_zero = 0.00001;            // Threshold to be considered as zero
  double parea = 1.0 - adimp - pctim;     // Area parameter
  
    // Handle pet_Soil if SoilEvp is TRUE
  NumericVector pet_Soil_vec;
  bool has_pet_Soil = false;
  if(SoilEvp && pet_Soil.isNotNull()) {
    pet_Soil_vec = as<NumericVector>(pet_Soil);
    has_pet_Soil = true;
    if(pet_Soil_vec.size() != n){
      stop("Length of pet_Soil must match length of prcp and pet.");
    }
  }
  
  // Loop over each time step
  for(int i = 0; i < n; i++) {
    // Set input precipitation and potential evapotranspiration
    double pr = prcp[i];    // Precipitation [mm]
    double edmnd = pet[i];  // Potential ET [mm]
    
    // Initialize time interval sums
    double sbf   = 0.0; // Sum of total baseflow
    double spbf  = 0.0; // Sum of total baseflow (duplicate in R code)
    double ssur  = 0.0; // Sum of surface runoff
    double sif   = 0.0; // Sum of interflow
    double sperc = 0.0; // Sum of percolation
    double sdro  = 0.0; // Sum of direct runoff from impervious area
    double tet   = 0.0; // Sum of total ET
    
    // Calculate soil water evaporation for the upper layer if applicable
    double etSoil_1 = 0.0, etSoil_2 = 0.0;
    if(SoilEvp && has_pet_Soil) {
      double edmnd_soil = pet_Soil_vec[i];
      
      // ET from Upper zone tension water storage
      etSoil_1 = edmnd_soil * uztwc / uztwm;
      double red = edmnd_soil - etSoil_1; // Residual ET demand
      uztwc -= etSoil_1;
      
      if(uztwc <= 0.0) {
        etSoil_1 += uztwc; // Adjust ET(1)
        uztwc = 0.0;
        red = edmnd_soil - etSoil_1;
        
        if(uzfwc < red) {
          etSoil_2 = uzfwc;
          uzfwc = 0.0;
          red -= etSoil_2;
        } else {
          etSoil_2 = red;
          uzfwc -= etSoil_2;
          red = 0.0;
        }
        
        // Ensure no negative storages
        if(uztwc < thres_zero) uztwc = 0.0;
        if(uzfwc < thres_zero) uzfwc = 0.0;
      } else {
        // Transfer free water to tension water storage if ratio exceeds
        if( (uztwc / uztwm) < (uzfwc / uzfwm) ) {
          double uzrat = (uztwc + uzfwc) / (uztwm + uzfwm);
          uztwc = uztwm * uzrat;
          uzfwc = uzfwm * uzrat;
        }
        
        if(uztwc < thres_zero) uztwc = 0.0;
        if(uzfwc < thres_zero) uzfwc = 0.0;
      }
    }
    
    // ET(1): ET from Upper zone tension water storage
    double et1 = edmnd * uztwc / uztwm;
    double red = edmnd - et1; // Residual ET demand
    uztwc -= et1;
    
    // ET(2): ET from Upper zone free water storage
    double et2 = 0.0;
    if(uztwc <= 0.0) {
      et1 += uztwc; // Adjust ET(1)
      uztwc = 0.0;
      red = edmnd - et1;
      
      if(uzfwc < red) {
        et2 = uzfwc;
        uzfwc = 0.0;
        red -= et2;
      } else {
        et2 = red;
        uzfwc -= et2;
        red = 0.0;
      }
      
      // Ensure no negative storages
      if(uztwc < thres_zero) uztwc = 0.0;
      if(uzfwc < thres_zero) uzfwc = 0.0;
    } else {
      // Transfer free water to tension water storage if ratio exceeds
      if( (uztwc / uztwm) < (uzfwc / uzfwm) ) {
        double uzrat = (uztwc + uzfwc) / (uztwm + uzfwm);
        uztwc = uztwm * uzrat;
        uzfwc = uzfwm * uzrat;
      }
      
      if(uztwc < thres_zero) uztwc = 0.0;
      if(uzfwc < thres_zero) uzfwc = 0.0;
    }
    
    // ET(3): ET from Lower zone tension water storage
    double et3 = red * lztwc / (uztwm + lztwm);
    lztwc -= et3;
    if(lztwc < 0.0) {
      et3 += lztwc;
      lztwc = 0.0;
    }
    
    // Water resupply from Lower free water storages to Lower tension water storage
    double saved = rserv * (lzfpm + lzfsm);
    double ratlzt = lztwc / lztwm;
    double ratlz  = (lztwc + lzfpc + lzfsc - saved) / (lztwm + lzfpm + lzfsm - saved);
    
    if(ratlzt < ratlz) {
      double del = (ratlz - ratlzt) * lztwm;
      lztwc += del;
      lzfsc -= del;
      
      // Ensure no negative storages
      if(lzfsc < 0.0) {
        lzfpc += lzfsc;
        lzfsc = 0.0;
      }
    }
    
    if(lztwc < thres_zero) lztwc = 0.0;
    
    // ET(5): ET from additional impervious area
    double et5 = 0.0;
    et5 = et1 + (red + et2) * (adimc - et1 - uztwc) / (uztwm + lztwm);
    adimc -= et5;
    if(adimc < 0.0) {
      et5 += adimc; // Adjust ET(5)
      adimc = 0.0;
    }
    et5 *= adimp;
    
    // Add Precipitation to update soil water content
    double twx = pr + uztwc - uztwm;
    
    if(twx < 0.0) {
      uztwc += pr;
      twx = 0.0;
    } else {
      uztwc = uztwm;
    }
    
    // Moisture available in excess of uztw storage
    double excess_pr = twx;
    
    // Update impervious area storage
    adimc += pr - excess_pr;
    
    // Compute Impervious Area Runoff
    double roimp = pr * pctim;
    
    // Determine computational time increments
    int ninc = floor(1.0 + 0.2 * (uzfwc + excess_pr));
    double dinc = 1.0 / ninc;
    double pinc = excess_pr / ninc;
    
    // Compute free water depletion fractions
    double duz = 1.0 - pow(1.0 - uzk, dinc);
    double dlzp = 1.0 - pow(1.0 - lzpk, dinc);
    double dlzs = 1.0 - pow(1.0 - lzsk, dinc);
    
    // Start incremental for-loop
    for(int n = 0; n < ninc; n++) {
      double adsur = 0.0;   // Amount of surface runoff
      double excess = 0.0;  // Excess of lower zone soil water capacity
      
      // Compute direct runoff from impervious area
      double ratio = (adimc - uztwc) / lztwm;
      if(ratio < 0.0) ratio = 0.0;
      
      double addro = pinc * pow(ratio, 2.0);
      
      // Compute baseflow from primary storage
      double bf_p = lzfpc * dlzp;
      lzfpc -= bf_p;
      if(lzfpc <= 0.0001) {
        bf_p += lzfpc;
        lzfpc = 0.0;
      }
      sbf += bf_p;
      spbf += bf_p;
      
      // Compute baseflow from supplementary storage
      double bf_s = lzfsc * dlzs;
      lzfsc -= bf_s;
      if(lzfsc <= 0.0001) {
        bf_s += lzfsc;
        lzfsc = 0.0;
      }
      sbf += bf_s;
      
      // Compute percolation
      double perc = 0.0;
      if((pinc + uzfwc) > 0.01) {
        double percm = lzfpm * dlzp + lzfsm * dlzs;
        perc = percm * uzfwc / uzfwm;
        
        // DEFR is the lower zone moisture deficiency ratio
        double defr = 1.0 - (lztwc + lzfpc + lzfsc) / (lztwm + lzfpm + lzfsm);
        if(defr < 0.0) defr = 0.0;
        
        perc *= (1.0 + zperc * pow(defr, rexp));
        
        // Percolation rate exceeds uzfwc
        if(perc >= uzfwc) perc = uzfwc;
        
        uzfwc -= perc;
        
        // Check if percolation exceeds lower zone deficiency
        double check = lztwc + lzfpc + lzfsc + perc - lztwm - lzfpm - lzfsm;
        if(check > 0.0) {
          perc -= check;
          uzfwc += check;
        }
        
        // Sum of percolation
        sperc += perc;
        
        // Compute interflow
        double del = uzfwc * duz;
        if(del > uzfwc) {
          del = uzfwc;
          uzfwc = 0.0;
        } else {
          uzfwc -= del;
        }
        sif += del;
        
        // Distribute percolated water into lower zones
        double perct = perc * (1.0 - pfree);
        double percf = 0.0;
        if((perct + lztwc) <= lztwm) {
          lztwc += perct;
        } else {
          percf = lztwc + perct - lztwm;
          lztwc = lztwm;
        }
        percf += perc * pfree;
        
        if(percf != 0.0) {
          double hpl = lzfpm / (lzfpm + lzfsm);
          double ratlp = lzfpc / lzfpm;
          double ratls = lzfsc / lzfsm;
          
          double fracp = hpl * 2.0 * (1.0 - ratlp) / (2.0 - ratlp - ratls);
          if(fracp > 1.0) fracp = 1.0;
          
          double percp = percf * fracp;
          double percs = percf - percp;
          lzfsc += percs;
          
          if(lzfsc > lzfsm) {
            percs -= (lzfsc - lzfsm);
            lzfsc = lzfsm;
          }
          
          lzfpc += percf - percs;
          
          // Ensure lzfpc does not exceed lzfpm
          if(lzfpc >= lzfpm) {
            excess = lzfpc - lzfpm;
            lztwc += excess;
            lzfpc = lzfpm;
            if(lztwc >= lztwm) {
              excess = lztwc - lztwm;
              lztwc = lztwm;
            }
          }
        }
        
        // Distribute pinc between uzfwc and surface runoff
        if((pinc + excess) != 0.0) {
          if((pinc + uzfwc + excess) <= uzfwm) {
            uzfwc += pinc + excess; // No surface runoff
          } else {
            double sur = pinc + uzfwc + excess - uzfwm; // Surface runoff
            uzfwc = uzfwm;
            
            ssur += (sur * parea);
            
            // Surface runoff from impervious area not currently generating direct runoff
            adsur = sur * (1.0 - addro / pinc);
            ssur += adsur * adimp;
          }
        }
      } else {
        // If no moisture available for percolation, add to uzfwc
        uzfwc += pinc;
      }
      
      // Update impervious area storage
      adimc += pinc - addro - adsur;
      if(adimc > (uztwm + lztwm)) {
        addro += adimc - (uztwm + lztwm);
        adimc = uztwm + lztwm;
      }
      
      // Direct runoff from additional impervious area
      sdro += (addro * adimp);
      
      // Ensure no negative impervious area storage
      if(adimc < thres_zero) adimc = 0.0;
    } // End of incremental for-loop
    
    // Compute sums and adjust runoff amounts
    double eused = (et1 + et2 + et3) * parea;
    sif *= parea;
    
    // Separate channel component of baseflow from non-channel component
    double tbf = sbf * parea;   // Total baseflow
    double bfcc = tbf / (1.0 + side); // Channel component baseflow
    double bfp = (spbf * parea) / (1.0 + side); // Primary component baseflow
    double bfs = bfcc - bfp; // Supplemental component baseflow
    if(bfs < 0.0) bfs = 0.0;
    double bfncc = tbf - bfcc; // Non-channel component baseflow
    
    // Ground flow and Surface flow
    double base = bfcc;
    double surf = roimp + sdro + ssur + sif;
    
    // ET(4): ET from riparian vegetation (no effect if riva = 0)
    double et4 = (edmnd - (et1 + et2 + et3)) * riva;
    
    // Total evapotranspiration
    tet += eused + et4 + et5;
    
    // Adjust total outflow to prevent negative flows
    double tot_outflow = surf + base - et4;
    if(tot_outflow < 0.0) {
      tot_outflow = 0.0;
      surf = 0.0;
      base = 0.0;
    } else {
      double surf_remainder = surf - et4;
      surf = std::max(0.0, surf_remainder);
      if(surf_remainder < 0.0) {
        base += surf_remainder;
        if(base < 0.0) base = 0.0;
      }
    }
    
    // Store results
    simaet[i]      = tet;
    simaet1[i]     = et1;
    simaet2[i]     = et2;
    simaet3[i]     = et3;
    simaet4[i]     = et4;
    simaet5[i]     = et5;
    simflow[i]     = tot_outflow;
    surf_tot[i]    = ssur;
    interflow_tot[i] = sif;
    base_tot[i]    = base;
    uztwc_ts[i]    = uztwc;
    uzfwc_ts[i]    = uzfwc;
    lztwc_ts[i]    = lztwc;
    lzfpc_ts[i]    = lzfpc;
    lzfsc_ts[i]    = lzfsc;
    
    if(SoilEvp && has_pet_Soil) {
      simaetSoil_1[i] = etSoil_1;
      simaetSoil_2[i] = etSoil_2;
      simaetSoil[i]   = etSoil_1 + etSoil_2;
    }
  } // End of time-loop
  
  // Prepare output data frame
  if(SoilEvp && has_pet_Soil) {
    return DataFrame::create(
      Named("ESoilTot")    = simaetSoil,
      Named("ESoil_1")     = simaetSoil_1,
      Named("ESoil_2")     = simaetSoil_2,
      Named("aetTot")      = simaet,
      Named("aetUZT")      = simaet1,
      Named("aetUZF")      = simaet2,
      Named("aetLZT")      = simaet3,
      Named("aet4")        = simaet4,
      Named("aet5")        = simaet5,
      Named("WaYldTot")    = simflow,
      Named("WYSurface")   = surf_tot,
      Named("WYInter")     = interflow_tot,
      Named("WYBase")      = base_tot,
      Named("uztwc")       = uztwc_ts,
      Named("uzfwc")       = uzfwc_ts,
      Named("lztwc")       = lztwc_ts,
      Named("lzfpc")       = lzfpc_ts,
      Named("lzfsc")       = lzfsc_ts
    );
  } else {
    return DataFrame::create(
      Named("aetTot")      = simaet,
      Named("aetUZT")      = simaet1,
      Named("aetUZF")      = simaet2,
      Named("aetLZT")      = simaet3,
      Named("aet4")        = simaet4,
      Named("aet5")        = simaet5,
      Named("WaYldTot")    = simflow,
      Named("WYSurface")   = surf_tot,
      Named("WYInter")     = interflow_tot,
      Named("WYBase")      = base_tot,
      Named("uztwc")       = uztwc_ts,
      Named("uzfwc")       = uzfwc_ts,
      Named("lztwc")       = lztwc_ts,
      Named("lzfpc")       = lzfpc_ts,
      Named("lzfsc")       = lzfsc_ts
    );
  }
}

