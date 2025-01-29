


# ---- load-packages -----------------------------------------------------------
library(Hmisc)
library(readxl)
library(miechv3) #borrow miechv functions - have to comment this out to run the flow file if you don't have access
require(RODBC, quietly=TRUE)
require(REDCapR, quietly=TRUE)
require(dplyr, quietly = TRUE)
require(readr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(openintro, quietly = TRUE)
require(Hmisc, quietly = TRUE)


credential1 <- REDCapR::retrieve_credential_mssql(dsn="BbmcSecurity", project_id=2845L, instance='bbmc')
ds <- REDCapR::redcap_read(batch_size = 2000L, redcap_uri=credential1$redcap_uri, token=credential1$token)$data

rm(credential1)

500000.00+
  551050.00+
   33250.00+
  275619.00+
   31563.00+
   73275.00+
  342402.00+
  119048.00+
  212014.00+
  373477.00+
   50000.00+
  550000.00+
   76726.00+
  337731.00


663589.00+
 10000.00+
424677.00+
252469.00+
  1000386.00+
 78007.00+
681982.00+
100000.00