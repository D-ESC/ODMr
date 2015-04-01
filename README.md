# ODMr
A R package for working with an instance of the CUAHSI ODM running on MS-SQL. Works in our office, may or may not be useful to others outside our work-site. The development version can be installed using:

```R
# install.packages("devtools")
devtools::install_github("D-ESC/ODMr")
```

In order to talk to the database, you'll need to an ODBC connection to the ODM database on the machine that you're working on. To do this in Win7, go to Control Panel -> Administrative Tools, then choose "Data Sources (ODBC)". You'll want to add a "User DSN".

Here is a quick example. You'll need the package RODBC in order to establish a connection using the ODBC connection you just defined. odbcConnect establishes a connection to the specified DSN. sqlFetch is used to import the series catalog.

```R
require(RODBC)
ODM <- odbcConnect("Connection", "User id", "Password")
Catalog = sqlFetch(ODM, "SeriesCatalog")
```

Using the established connection and referencing the series catalog we can import data from the database. The series catalog is not stable so only use this method to quickly look at certain data. A more stable reference can be had by specifying SiteID, VariableID, MethodID and QualityControlLevelID.

```R
Data <- ODMselect(ODM, CatalogID = 10, "2013-06-01", "2014-06-01")
```
