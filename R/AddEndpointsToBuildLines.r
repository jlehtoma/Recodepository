# This programme reads in a table of sequential GPS points that belong to a polygon
# and creates a table of lines (with startpoints and endpoints) based on the points.
# This can then be used in ArcGIS for drawing polygons based on the lines.
# I had to do this, as I got habitat patch data from the field in point format.
# The data can contain several polygons, each having a distinct polygon ID.
# Henna Fabritius, May 2012.

# Read in the point data in a three-column table: polygonID, longitude, latitude
setwd("C:/HY-data/EHHFABRI/documents/ArcGIS maps")
GPSpoints <- read.table("Tummaverkkoperhoslaikut_2011_Satakunta_E-P_ETRS-TM35FIN.txt", header=TRUE)

# Add two columns for adding the endpoints to each startpoint for creating lines
GPSpoints$EndLong = 0
GPSpoints$EndLat = 0

# Store the first point of a polygon for later use
PointStorageLongitude <- GPSpoints[1, 2]
PointStorageLatitude <- GPSpoints[1, 3]

# For each line/data point (except the last one) in the file...
for (i in 1:(nrow(GPSpoints)-1)){
	# If this point is NOT the last point of a polygon (i.e. the polygon ID is the same as in the next row)
	if (equals(GPSpoints[i+1, 1], GPSpoints[i, 1])){
		# Copy the (starting) point on the next row to serve as the endpoint of the line described in this row
		GPSpoints[i, 4] = GPSpoints[i+1, 2]
		GPSpoints[i, 5] = GPSpoints[i+1, 3]
	}
	# If this point IS the last point of a polygon (i.e. the point in the next row belongs to a different polygon)
	else {
		# Retrieve the first point of the polygon from the storage and place it here as the endpoint of this last line (to close the polygon)
		GPSpoints[i, 4] = PointStorageLongitude
		GPSpoints[i, 5] = PointStorageLatitude
		# And copy the next row's points (i.e. the first points of the next polygon) to the storage
		PointStorageLongitude = GPSpoints[i+1, 2]
		PointStorageLatitude = GPSpoints[i+1, 3]
	}
}

# Finally, for the last row/point of the file, add the last data points from the storage (i.e. the first points of the last polygon) as endpoints
GPSpoints[nrow(GPSpoints), 4] = PointStorageLongitude
GPSpoints[nrow(GPSpoints), 5] = PointStorageLatitude

#Write table

