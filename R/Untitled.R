

day = readNWMdata(101) %>% aggregate_daily()

com = day$comid %>% unique()

day$comid %in% gage_lu$comid 


COMID
USGS
file()