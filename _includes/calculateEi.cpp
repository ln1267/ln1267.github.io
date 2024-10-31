// calculateEi.cpp
// C++ Implementation of the (dWaSSI model)
// Author: Ning Liu
// Date: 2024-10-30

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Wet Canopy Evaporation function

// [[Rcpp::export]]
DataFrame calculateEi(DataFrame da_daily) {
  // Extract necessary columns from the input DataFrame
  NumericVector Ei_pot = da_daily["Ei_pot"];
  NumericVector P_c = da_daily["P_c"];
  NumericVector PT = da_daily["PT"];
  NumericVector Fc = da_daily["Fc"];
  
  int n = da_daily.nrows();
  
  // Step 1: Correct Ei_pot by Rainfall
  for(int i = 0; i < n; i++) {
    // Replace NA with 0 for Ei_pot and P_c
    double Ei_pot_val = Ei_pot[i];
    double P_c_val = P_c[i];
    
    if(R_IsNA(Ei_pot_val)) Ei_pot_val = 0.0;
    if(R_IsNA(P_c_val)) P_c_val = 0.0;
    
    // Ensure Ei_pot does not exceed P_c
    Ei_pot_val = std::min(Ei_pot_val, P_c_val);
    
    // If P_c is 0, set Ei_pot to 0
    if(P_c_val == 0.0) {
      Ei_pot_val = 0.0;
    }
    
    Ei_pot[i] = Ei_pot_val;
  }
  
  // Step 2: Initialize Ei and P_Ei
  NumericVector Ei(n, NA_REAL); // Initialize Ei with NA
  NumericVector P_Ei(n, NA_REAL); // Initialize P_Ei with NA
  
  for(int i = 0; i < n; i++) {
    double P_c_val = P_c[i];
    double Ei_pot_val = Ei_pot[i];
    
    if(R_IsNA(P_c_val)) P_c_val = 0.0;
    if(R_IsNA(Ei_pot_val)) Ei_pot_val = 0.0;
    
    P_Ei[i] = P_c_val - Ei_pot_val;
  }
  
  // Step 3: Replace NA values in Ei and P_Ei with 0
  for(int i = 0; i < n; i++) {
    if(R_IsNA(Ei[i])) {
      Ei[i] = 0.0;
    }
    if(R_IsNA(P_Ei[i])) {
      P_Ei[i] = 0.0;
    }
  }
  
  // Step 4: Calculate Canopy Evaporation (Ei)
  double Ei_left = 0.0;
  for(int i = 0; i < n; i++) {
    double ei_pot;
    double Ep;
    
    if(i == 0) {
      Ei_left = 0.0;
      ei_pot = Ei_pot[i];
      Ep = PT[i] * Fc[i];
    } else {
      ei_pot = Ei_pot[i] + Ei_left;
      Ep = PT[i] * Fc[i];
    }
    
    // Replace NA in Ep with 0
    if(R_IsNA(Ep)) {
      Ep = 0.0;
    }
    
    // Replace NA in ei_pot with 0
    if(R_IsNA(ei_pot)) {
      ei_pot = 0.0;
    }
    
    // Calculate Ei as the minimum of Ep and ei_pot
    double Ei_val = std::min(Ep, ei_pot);
    Ei[i] = Ei_val;
    
    // Update Ei_left for the next iteration
    Ei_left = std::max(0.0, ei_pot - Ei_val);
  }
  
  // Step 5: Assign Ei and P_Ei back to the DataFrame
  List da_Ei = da_daily;
  da_Ei["Ei"] = Ei;
  da_Ei["P_Ei"] = P_Ei;
  
  // Replace any remaining NA values with 0
  // (This is precautionary; already handled above)
  for(int i = 0; i < da_Ei.size(); i++) {
    if(Rf_isNull(da_Ei[i])) {
      da_Ei[i] = 0.0;
    }
  }
  
  // Convert List back to DataFrame
  DataFrame output = as<DataFrame>(da_Ei);
  
  return output;
}
