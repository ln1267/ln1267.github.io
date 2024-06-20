// Define the value labels from the provided UID and object labels
var valueLabels = {
  1: 'F1.1 Permanent upland streams',
  2: 'F1.2 Permanent lowland rivers',
  3: 'F1.3 Freeze-thaw rivers and streams',
  4: 'F1.4 Seasonal upland streams',
  5: 'F1.5 Seasonal lowland rivers',
  6: 'F1.6 Episodic arid rivers',
  7: 'F1.7 Large lowland rivers',
  8: 'F2.1 Large permanent freshwater lakes',
  9: 'F2.2 Small permanent freshwater lakes',
  10: 'F2.3 Seasonal freshwater lakes',
  11: 'F2.4 Freeze-thaw freshwater lakes',
  12: 'F2.5 Ephemeral freshwater lakes',
  13: 'F2.6 Permanent salt and soda lakes',
  14: 'F2.7 Ephemeral salt lakes',
  15: 'F2.8 Artesian springs and oases',
  16: 'F2.9 Geothermal pools and wetlands',
  17: 'F2.10 Subglacial lakes',
  18: 'F3.1 Large reservoirs',
  19: 'F3.2 Constructed lacustrine wetlands',
  20: 'F3.3 Rice paddies',
  21: 'F3.4 Freshwater aquafarms',
  22: 'F3.5 Canals, ditches and drains',
  23: 'FM1.1 Deepwater coastal inlets',
  24: 'FM1.2 Permanently open riverine estuaries and bays',
  25: 'FM1.3 Intermittently closed and open lakes and lagoons',
  26: 'MFT1.1 Coastal river deltas',
  27: 'MFT1.2 Intertidal forests and shrublands',
  28: 'MFT1.3 Coastal saltmarshes and reedbeds',
  29: 'MT1.1 Rocky Shorelines',
  30: 'MT1.2 Muddy Shorelines',
  31: 'MT1.3 Sandy Shorelines',
  32: 'MT1.4 Boulder and cobble shores',
  33: 'MT2.1 Coastal shrublands and grasslands',
  34: 'MT2.2 Large seabird and pinniped colonies',
  35: 'MT3.1 Artificial shorelines',
  36: 'T1.1 Tropical/Subtropical lowland rainforests',
  37: 'T1.2 Tropical/Subtropical dry forests and thickets',
  38: 'T1.3 Tropical/Subtropical montane rainforests',
  39: 'T1.4 Tropical heath forests',
  40: 'T2.1 Boreal and temperate high montane forests and woodlands',
  41: 'T2.2 Deciduous temperate forests',
  42: 'T2.3 Oceanic cool temperate rainforests',
  43: 'T2.4 Warm temperate laurophyll forests',
  44: 'T2.5 Temperate pyric humid forests',
  45: 'T2.6 Temperate pyric sclerophyll forests and woodlands',
  46: 'T3.1 Seasonally dry tropical shrublands',
  47: 'T3.2 Seasonally dry temperate heath and shrublands',
  48: 'T3.3 Cool temperate heathlands',
  49: 'T3.4 Young rocky pavements, lava flows and screes',
  50: 'T4.1 Trophic savannas',
  51: 'T4.2 Pyric tussock savannas',
  52: 'T4.3 Hummock savannas',
  53: 'T4.4 Temperate woodlands',
  54: 'T4.5 Temperate subhumid grasslands',
  55: 'T5.1 Semi-desert steppe',
  56: 'T5.2 Succulent or Thorny deserts and semi-deserts',
  57: 'T5.3 Sclerophyll hot deserts and semi-deserts',
  58: 'T5.4 Cool deserts and semi-deserts',
  59: 'T5.5 Hyper-arid deserts',
  60: 'T6.1 Ice sheets, glaciers and perennial snowfields',
  61: 'T6.2 Polar/alpine cliffs, screes, outcrops and lava flows',
  62: 'T6.3 Polar tundra and deserts',
  63: 'T6.4 Temperate alpine grasslands and shrublands',
  64: 'T6.5 Tropical alpine grasslands and herbfields',
  65: 'T7.1 Annual croplands',
  66: 'T7.2 Sown pastures and fields',
  67: 'T7.3 Plantations',
  68: 'T7.4 Urban and industrial ecosystems',
  69: 'T7.5 Derived semi-natural pastures and old fields',
  70: 'TF1.1 Tropical flooded forests and peat forests',
  71: 'TF1.2 Subtropical/temperate forested wetlands',
  72: 'TF1.3 Permanent marshes',
  73: 'TF1.4 Seasonal floodplain marshes',
  74: 'TF1.5 Episodic arid floodplains',
  75: 'TF1.6 Boreal, temperate and montane peat bogs',
  76: 'TF1.7 Boreal and temperate fens',
  77: 'M1.1 Seagrass meadows',
  78: 'M1.10 Rhodolith/Maërl beds',
  79: 'M1.2 Kelp forests',
  80: 'M1.3 Photic coral reefs',
  81: 'M1.4 Shellfish beds and reefs',
  82: 'M1.5 Photo-limited marine animal forests',
  83: 'M1.6 Subtidal rocky reefs',
  84: 'M1.7 Subtidal sand beds',
  85: 'M1.8 Subtidal mud plains',
  86: 'M1.9 Upwelling zones',
  87: 'M2.1 Epipelagic ocean waters',
  88: 'M2.2 Mesopelagic ocean water',
  89: 'M2.3 Bathypelagic ocean waters',
  90: 'M2.4 Abyssopelagic ocean waters',
  91: 'M2.5 Sea ice',
  92: 'M3.1 Continental and island slopes',
  93: 'M3.2 Submarine canyons',
  94: 'M3.3 Abyssal plains',
  95: 'M3.4 Seamounts, ridges and plateaus',
  96: 'M3.5 Deepwater biogenic beds',
  97: 'M3.6 Hadal trenches and troughs',
  98: 'M3.7 Chemosynthetic-based-ecosystems (CBE)',
  99: 'M4.1 Submerged artificial structures',
  100: 'M4.2 Marine aquafarms'
};

// Assign color codes to each value based on their meaning
var valueColors = {
  1: '#0000ff',  // Blue for streams
  2: '#1e90ff',  // Dodger blue for rivers
  3: '#4682b4',  // Steel blue for freeze-thaw rivers
  4: '#5f9ea0',  // Cadet blue for seasonal upland streams
  5: '#87cefa',  // Light sky blue for seasonal lowland rivers
  6: '#00bfff',  // Deep sky blue for arid rivers
  7: '#add8e6',  // Light blue for large lowland rivers
  8: '#0000cd',  // Medium blue for large permanent freshwater lakes
  9: '#1e90ff',  // Dodger blue for small permanent freshwater lakes
  10: '#00ced1', // Dark turquoise for seasonal freshwater lakes
  11: '#20b2aa', // Light sea green for freeze-thaw freshwater lakes
  12: '#40e0d0', // Turquoise for ephemeral freshwater lakes
  13: '#48d1cc', // Medium turquoise for permanent salt and soda lakes
  14: '#00fa9a', // Medium spring green for ephemeral salt lakes
  15: '#00ff7f', // Spring green for artesian springs and oases
  16: '#3cb371', // Medium sea green for geothermal pools and wetlands
  17: '#2e8b57', // Sea green for subglacial lakes
  18: '#66cdaa', // Medium aquamarine for large reservoirs
  19: '#8fbc8f', // Dark sea green for constructed lacustrine wetlands
  20: '#98fb98', // Pale green for rice paddies
  21: '#00ff00', // Lime for freshwater aquafarms
  22: '#7fff00', // Chartreuse for canals, ditches and drains
  23: '#7cfc00', // Lawn green for deepwater coastal inlets
  24: '#32cd32', // Lime green for riverine estuaries and bays
  25: '#9acd32', // Yellow green for intermittently closed and open lakes
  26: '#228b22', // Forest green for coastal river deltas
  27: '#006400', // Dark green for intertidal forests and shrublands
  28: '#556b2f', // Dark olive green for coastal saltmarshes and reedbeds
  29: '#8b4513', // Saddle brown for rocky shorelines
  30: '#a0522d', // Sienna for muddy shorelines
  31: '#d2691e', // Chocolate for sandy shorelines
  32: '#8b0000', // Dark red for boulder and cobble shores
  33: '#b22222', // Firebrick for coastal shrublands and grasslands
  34: '#ff4500', // Orange red for large seabird and pinniped colonies
  35: '#ff8c00', // Dark orange for artificial shorelines
  36: '#ffa500', // Orange for tropical/subtropical lowland rainforests
  37: '#ff6347', // Tomato for tropical/subtropical dry forests
  38: '#ff7f50', // Coral for tropical/subtropical montane rainforests
  39: '#ff4500', // Orange red for tropical heath forests
  40: '#ff0000', // Red for boreal and temperate high montane forests
  41: '#ff69b4', // Hot pink for deciduous temperate forests
  42: '#ff1493', // Deep pink for oceanic cool temperate rainforests
  43: '#ff00ff', // Magenta for warm temperate laurophyll forests
  44: '#ba55d3', // Medium orchid for temperate pyric humid forests
  45: '#9400d3', // Dark violet for temperate pyric sclerophyll forests
  46: '#8a2be2', // Blue violet for seasonally dry tropical shrublands
  47: '#9370db', // Medium purple for seasonally dry temperate heath
  48: '#7b68ee', // Medium slate blue for cool temperate heathlands
  49: '#6a5acd', // Slate blue for young rocky pavements
  50: '#483d8b', // Dark slate blue for trophic savannas
  51: '#663399', // Rebecca purple for pyric tussock savannas
  52: '#8b008b', // Dark magenta for hummock savannas
  53: '#800080', // Purple for temperate woodlands
  54: '#4b0082', // Indigo for temperate subhumid grasslands
  55: '#191970', // Midnight blue for semi-desert steppe
  56: '#000080', // Navy for succulent or thorny deserts
  57: '#0000cd', // Medium blue for sclerophyll hot deserts
  58: '#00008b', // Dark blue for cool deserts
  59: '#1e90ff', // Dodger blue for hyper-arid deserts
  60: '#00bfff', // Deep sky blue for ice sheets
  61: '#87cefa', // Light sky blue for polar/alpine cliffs
  62: '#add8e6', // Light blue for polar tundra
  63: '#4682b4', // Steel blue for temperate alpine grasslands
  64: '#5f9ea0', // Cadet blue for tropical alpine grasslands
  65: '#7fff00', // Chartreuse for annual croplands
  66: '#00ff00', // Lime for sown pastures
  67: '#32cd32', // Lime green for plantations
  68: '#228b22', // Forest green for urban and industrial ecosystems
  69: '#006400', // Dark green for semi-natural pastures
  70: '#9acd32', // Yellow green for tropical flooded forests
  71: '#6b8e23', // Olive drab for subtropical/temperate forested wetlands
  72: '#808000', // Olive for permanent marshes
  73: '#556b2f', // Dark olive green for seasonal floodplain marshes
  74: '#8b4513', // Saddle brown for episodic arid floodplains
  75: '#a0522d', // Sienna for boreal, temperate and montane peat bogs
  76: '#d2691e', // Chocolate for boreal and temperate fens
  77: '#ff0000', // Red for seagrass meadows
  78: '#ff7f00', // Dark orange for rhodolith/Maërl beds
  79: '#ff4500', // Orange red for kelp forests
  80: '#ff6347', // Tomato for photic coral reefs
  81: '#ff7f50', // Coral for shellfish beds
  82: '#ff8c00', // Dark orange for photo-limited marine animal forests
  83: '#ffa500', // Orange for subtidal rocky reefs
  84: '#ff4500', // Orange red for subtidal sand beds
  85: '#ff6347', // Tomato for subtidal mud plains
  86: '#ff7f50', // Coral for upwelling zones
  87: '#ff8c00', // Dark orange for epipelagic ocean waters
  88: '#ffa500', // Orange for mesopelagic ocean water
  89: '#ff4500', // Orange red for bathypelagic ocean waters
  90: '#ff6347', // Tomato for abyssopelagic ocean waters
  91: '#ff7f50', // Coral for sea ice
  92: '#ff8c00', // Dark orange for continental and island slopes
  93: '#ffa500', // Orange for submarine canyons
  94: '#ff4500', // Orange red for abyssal plains
  95: '#ff6347', // Tomato for seamounts, ridges and plateaus
  96: '#ff7f50', // Coral for deepwater biogenic beds
  97: '#ff8c00', // Dark orange for hadal trenches
  98: '#ffa500', // Orange for chemosynthetic-based ecosystems
  99: '#ff4500', // Orange red for submerged artificial structures
  100: '#ff6347' // Tomato for marine aquafarms
};

// Polyfill for Object.values() in case it is not available
function getObjectValues(obj) {
  var values = [];
  for (var key in obj) {
    if (obj.hasOwnProperty(key)) {
      values.push(obj[key]);
    }
  }
  return values;
}

// Convert objects to lists
var valueKeys = Object.keys(valueLabels);
var valueLabelList = getObjectValues(valueLabels);
var valueColorList = getObjectValues(valueColors);

// Print the keys for verification
print(valueKeys);

exports.valueLabels = valueLabels;
exports.valueColors = valueColors;
exports.valueKeys = valueKeys;
exports.valueLabelList = valueLabelList;
exports.valueColorList = valueColorList;
