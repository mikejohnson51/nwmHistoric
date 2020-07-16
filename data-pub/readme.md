# NOAA National Water Model (NWM) Reanalysis Data at RENCI

The NOAA [National Water Model](http://water.noaa.gov/about/nwm) (NWM) simulates streamflow across on the medium resolution (1:100,000 scale) National Hydrography Dataset Plus Version 2 (NHDPlus). The NHDPlus describes the hydrographic network of the United States discretized into units  representing surface water features that share similar hydrologic characteristics. Each surface water feature can be represented as the catchment divide (archetypal watershed for that segment), its flowline, and/or outlet (using the Open Geospatial Consortium HY_Feature terminology). Computationally, NWM forecasts are made by routing water from a land surface model (NOAH-MP) over a 250 meter grid centered on the NHD flowline to the outlet of each NHD catchment. The COMID scheme was adopted as the indexing unit in the NWM such that a forecast for a given COMID is the routed flow rate at the NHD outlet at any given hour.

With each new version of the NWM, a multi-decadal, hourly reanalysis product - forced with the North America Land Data Assimilation System (NLDAS) and the North American Regional Reanalysis (NARR) data - is released.  The raw data, stored as hourly netCDF files, are made publicly available on Amazon and Google by the Open Cloud Consortium [here](https://registry.opendata.aws/nwm-archive/).

This data release contains the NWM v1.2 and 2.0 structured for feature level extraction. The impact of this is that user can query time series for a given NHDPlusV2 COMID without downloading the hourly CONUS files and extracting the sample of relevant values. This data is hosted on the RENCI THREDDS Data Server and is accessible via OPeNDAP at:

## [Version 1.2](http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/nwm_retro_full.ncml.html)
 -  1993-01-01 to 2017-12-31
 - Contains 219,144 hourly time steps for
 - 2729077 NHD reaches

## [Version 2.0](http://thredds.hydroshare.org/thredds/dodsC/nwm_retrospective/nwm_v2_retro_full.ncml.html)
 -  1993-01-01 to 2018-12-31
 - Contains 227,903 hourly time steps for
 - 2,729,076 NHD reaches

## DDS

The data description structure (DDS) can be viewed at the NcML page for each respective resource (linked above). More broadly each resource includes:

- A _1D_ time array -  **hours** since 1970-01-01 00:00
- A _1D_ latitude array -  **coordinate** (Y) information
- A _1D_ longitude array - **coordinate** (X) information WGS84
- A _1D_ feature_id array - **NHDPlus V2 COMID** (NWM forecast ID)
- A _2D_ streamflow array - **Q (cms)** [feature_id, time]


## R package

The `nwmHistoric` R package provides easier interaction with the OPeNDAP resources. Package documentation can be found [here](https://mikejohnson51.github.io/nwmHistoric/) and the GitHub repository [here](https://github.com/mikejohnson51/nwmHistoric).

# Collaborators:

[Mike Johnson](https://mikejohnson51.github.io/), [David Blodgett](https://www.usgs.gov/staff-profiles/david-l-blodgett?qt-staff_profile_science_products=3#qt-staff_profile_science_products)

# Support:

This effort is supported by the Consortium of Universities for the Advancement of Hydrologic Science, Inc. under the HydroInformatics Fellowship. See program [here](https://www.cuahsi.org/data-models/hydroinformatics-innovation-fellowship/)

# Publications

 Johnson J M, Blodgett, D L, Clarke K C, Pollak J. 26 years of Hourly Streamflow for every River in the Continental USA, _Nature Scientific Data_ (**In submission**)
