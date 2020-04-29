View air quality data from the World Air Quality Index.

The simplest way to view AQI info is with 'M-x aqi-report' which
displays air quality info for your algorithmically derived location
(equivalent to the location "here") or the name of a place.  A place
can be the name of a city (in which case the nearest monitoring
station is used) or the name of specific monitoring station.

To use the data programmatically, the functions 'aqi-report-full'
and 'aqi-report-brief' return the report as a string.  The function
'aqi-city-aqi' will return the AQI for a given city as a number.
