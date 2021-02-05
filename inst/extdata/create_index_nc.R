library(RNetCDF)
file1 <- "inst/extdata/retro_feature_ids.nc"
nc    <- create.nc(file1, format = 'netcdf4')

v1 = open.nc('http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/nwm_retro_full.ncml')
v1_ids = var.get.nc(v1, "feature_id")

v2 = open.nc('http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/nwm_v2_retro_full.ncml')
v2_ids = var.get.nc(v2, "feature_id")

dim.def.nc(nc, "feature_ids_v_one", length(v1_ids))
dim.def.nc(nc, "feature_ids_v_two", length(v2_ids))

##  Create three variables, one as coordinate variable
var.def.nc(nc, "feature_ids_v_one", "NC_DOUBLE", "feature_ids_v_one")
var.def.nc(nc, "feature_ids_v_two", "NC_DOUBLE", "feature_ids_v_two")
print.nc(nc)

##  Put all of the data:
var.put.nc(nc, "feature_ids_v_one", v1_ids)
var.put.nc(nc, "feature_ids_v_two", v2_ids)
close.nc(nc)

system(paste('ncks -4 -L 3 -O',  file1, file1))

nc2 = open.nc(file1)
v12 = var.get.nc(nc2, "feature_id_v12")
