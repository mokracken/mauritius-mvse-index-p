// ======================================================================
// 1. Load MVCA polygons
// ======================================================================
var MVCA = ee.FeatureCollection('projects/era-5-amt/assets/MVCA');

// Check properties of one feature (single-argument print)
var sample_feature = MVCA.first();
print(sample_feature.propertyNames());

// Visualize polygons
Map.addLayer(MVCA, {color: 'red'}, 'MVCA Polygons');
Map.centerObject(MVCA, 8);

// ======================================================================
// 2. Date range
// ======================================================================
var start_date = ee.Date('2013-12-01');
var end_date   = ee.Date('2024-11-01');

// ======================================================================
// 3. ERA5 DAILY_AGGR temperature + dewpoint dataset
// ======================================================================
var era5_temp = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')
  .select([
    'temperature_2m',           // tmean
    'temperature_2m_min',       // tmin
    'temperature_2m_max',       // tmax
    'dewpoint_temperature_2m'   // tdew
  ])
  .filterDate(start_date, end_date);

// ======================================================================
// 4. Function to extract daily temperature values for each polygon
// ======================================================================
function extractERA5Data(feature) {

  // store geometry once
  var geom = feature.geometry();

  var temp_ts = era5_temp.map(function(image) {

    var date = image.date().format('YYYY-MM-dd');

    // First attempt: extract values using original geometry
    var stats = image.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: geom,
      scale: 5000,
      crs: 'EPSG:4326',
      bestEffort: true,
      maxPixels: 1e9
    });

    // Extract Kelvin values (may be null)
    var tmeanK = stats.get('temperature_2m');
    var tminK  = stats.get('temperature_2m_min');
    var tmaxK  = stats.get('temperature_2m_max');
    var tdewK  = stats.get('dewpoint_temperature_2m');

    // fallback geometry (5 km buffer)
    var bufferedGeom = geom.buffer(5000);

    var stats_buffered = image.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: bufferedGeom,
      scale: 5000,
      crs: 'EPSG:4326',
      bestEffort: true,
      maxPixels: 1e9
    });

    var tdewK_buffer = stats_buffered.get('dewpoint_temperature_2m');

    // use fallback value only if original is null
    var tmeanK_fixed = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tmeanK, null),
      stats_buffered.get('temperature_2m'),
      tmeanK
    );

    var tminK_fixed = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tminK, null),
      stats_buffered.get('temperature_2m_min'),
      tminK
    );

    var tmaxK_fixed = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tmaxK, null),
      stats_buffered.get('temperature_2m_max'),
      tmaxK
    );

    var tdewK_fixed = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tdewK, null),
      tdewK_buffer,
      tdewK
    );

    // Convert Kelvin → Celsius safely
    var tmean = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tmeanK_fixed, null),
      null,
      ee.Number(tmeanK_fixed).subtract(273.15)
    );

    var tmin  = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tminK_fixed, null),
      null,
      ee.Number(tminK_fixed).subtract(273.15)
    );

    var tmax  = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tmaxK_fixed, null),
      null,
      ee.Number(tmaxK_fixed).subtract(273.15)
    );

    var tdew = ee.Algorithms.If(
      ee.Algorithms.IsEqual(tdewK_fixed, null),
      null,
      ee.Number(tdewK_fixed).subtract(273.15)
    );

    return ee.Feature(null, {
      'date': date,
      'site_id': feature.get('VCA'),
      'tmean': tmean,
      'tmin':  tmin,
      'tmax':  tmax,
      'tdew':  tdew
    });
  });

  return temp_ts;
}

// ======================================================================
// 5. Apply to all polygons
// ======================================================================
var results = MVCA.map(extractERA5Data).flatten();

// Print a small sample (single-argument print)
print(results.limit(10));

// ======================================================================
// 6. Export as CSV
// ======================================================================
Export.table.toDrive({
  collection: results,
  description: 'ERA5_Temperature_Daily_MVCA',
  fileFormat: 'CSV',
  selectors: ['date', 'site_id', 'tmean', 'tmin', 'tmax', 'tdew']
});
