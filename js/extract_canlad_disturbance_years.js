// ---------------------------------------------------------------------------- //
// Script Name: Extract CanLaD year of construction for all unpaved roads
// Author: Xavier Giroux-Bougard
// Description: This script extracts harvest years from CanLaD disturbance
//              products using statistics extracted from variable buffer sizes
//              around unpaved road segments (Ontario and Quebec) to estimate
//              their year of construction
// Date: 2025-02-21 
// License: Apache 2.0 License
// ---------------------------------------------------------------------------- //
// Note: Use and redistribution of this code must comply with the Apache 2.0 
// License and attribution requirements.

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- // 
// Step 0: Load required modules and define required functions/parameters

// Import modules
var lineTools = require("users/yourusername/tools:lineStringTools");

// CandLaD year of disturbance visualization params
var palette = ['#9400D3', '#4B0082', '#0000FF', '#00FF00', '#FFFF00', '#FF7F00', '#FF0000'];
var yodVisParams65_84 = {min: 1965, max: 1984, palette: palette};
var yodVisParams85_20 = {min: 1985, max: 2020, palette: palette};
var yodVisParams = {min: 1965, max: 2020, palette: palette};

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- // 
// Step 1: Import unpaved roads, and prep for downstream analyses

// Import unapved roads from ORN/MNRF and AQRP
var unpaved = ee.FeatureCollection('projects/your-project-repo/assets/unpaved_roads')
                // Add length as property
                .map(function(ft){return ft.set('length', ft.length())});
print('Test subset:', unpaved.limit(10)); // Testing
Map.addLayer(unpaved, {}, 'Unpaved');

// Subset into short and long roads
var shortRoads = unpaved.filter(ee.Filter.lte('length', 180));
var longRoads = unpaved.filter(ee.Filter.gt('length', 180));

// Shorten longer roads by 90 metres from start and end points
// (reduces chances of sampling change detection for perpendicular roads)
var longRoadsShortened = longRoads.map(function(ft){
  return ee.Feature(lineTools.shortenLineString(ft, 90));
});
// print('Long roads shortened by 180 m:', longRoadsShortened.limit(10)); // Testing

// Merge roads into single collection again
var roads = shortRoads.merge(longRoadsShortened);
// print('Merged road collection:', roads.limit(10)); // Testing

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- // 
// Step 2: Import CanLaD datasets

// Import CanLaD 1965 to 1984 harvest year data product
var canlad65 = ee.Image('projects/your-project-repo/assets/canlad_1965_1984_harvest_year').toUint16();
print('CanLaD 1965-1984:', canlad65); // Testing

// Import CanLaD 1985 to 2020 harvest year data product
var canlad85 = ee.Image('projects/your-project-repo/assets/canlad_1985_2020_harvest_year');
print('CanLaD 1985-2020:', canlad85); // Testing

// Visualize
Map.addLayer(canlad65, yodVisParams, 'CanLaD 1965-1984');
Map.addLayer(canlad85, yodVisParams, 'CanLaD 1985-2020');

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- // 
// Step 3: Extract dates from road segment buffers

// // Test road
// var road = ee.Feature(aqrp.first());
// print('Test road', road);
// print('Test road length', road.length());
// Map.addLayer(road, {}, 'Test road');
// Map.centerObject(road);

// // Shorten road and add small buffer
// var shortRoad = shortenLineString(road, 90);
// print('Test short road', shortRoad);
// print('Test short road length', ee.Feature(shortRoad).length());
// Map.addLayer(ee.Feature(shortRoad), {}, 'Test short road');

// Extract disturbance years from CanLaD in 30-m buffer
var roadsConstYearData = roads.map(function(ft){
  
  // Extract 5th percentile of CanLaD 1965-1984 pixel distribution in 30-m buffer
  var pre85_buf30 = canlad65.select('b1').reduceRegion({
    reducer: ee.Reducer.percentile({percentiles:[5]}),
    geometry: ft.geometry().buffer(30),
    scale: 30,
  });
  
  // Extract 5th percentile of CanLaD 1985-2020 pixel distribution in 30-m buffer
  var post85_buf30 = canlad85.select('b1').reduceRegion({
    reducer: ee.Reducer.percentile({percentiles:[5]}),
    geometry: ft.geometry().buffer(30),
    scale: 30,
  });
  
  // Return road with statistics
  return ft.set('yod_canlad_65', pre85_buf30.get('b1'))
  .set('yod_canlad_85', post85_buf30.get('b1'));
});

print('Test statistics extraction:', roadsConstYearData.limit(10));

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- // 
// Step 4: Export validation subset to drive
Export.table.toDrive({
  collection: roadsConstYearData,
  description: 'canlad_years_unpaved_roads',
  folder: 'logging_roads',
  fileNamePrefix: 'canlad_years_unpaved_roads',
  selectors: [
    // Road IDs
    'id', 
    // CanLaD disturbance data - pre-1985 (1965-1984)
    'yod_canlad_65',
    // CanLaD disturbance data - post-1985 (1985-2020)
    'yod_canlad_85'
  ]
});