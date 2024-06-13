# Meteorology-AWS-data-processing (height correction to 10m)

The Python code 'pyscript_htcorrection.py' shall recompute the dry bulb temperature, wind speed, specific humidity from existing height to 10m height using Monin-Obukhov Similarity theory by iterative method. The height correction algorithm is present in the FORTRAN script. The Python script calls the FORTRAN routine through the '.so' file. The Python code takes in relative humidity for obtaining specific humidity and relative humidity at 10m. Wind speed is recomputed to the height of air temperature measurement considering neutral stability. After that all the three variables are recomputed to 10 m using stability criteria. 

Give the path for the input files to the variable 'inpath1' in the Python code.
Files format : csv (comma separated).
Below are the column names and units of the data:
1. AIRT for air temperature (deg Celcius)
2. SST for sea surface temperature (deg Celcius)
3. SLP for sea level pressure (hPa)
4. RH for relative humidity (percentage)
5. WSPD for wind speed (m/s)

References :
1. da Silva, A. M., Young, C. C., & Levitus, S. (1994). Atlas of surface marine data 1994, Vol. 1: Algorithms and procedures. Noaa atlas nesdis, 6(83), 20910-3282.
2. Large, W. G., & Pond, S. (1981). Open ocean momentum flux measurements in moderate to strong winds. Journal of physical oceanography, 11(3), 324-336.
3. Large, W. G., & Pond, S. (1982). Sensible and latent heat flux measurements over the ocean. Journal of physical Oceanography, 12(5), 464-482.
4. Smith, S. D. (1980). Wind stress and heat flux over the ocean in gale force winds. Journal of Physical Oceanography, 10(5), 709-726.

Kindly acknowledge this Github repository if using it..thank you and all the best!
