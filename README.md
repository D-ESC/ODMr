# ODMr
A R package for working with an instance of the CUAHSI ODM running on MS-SQL. Works in our office, may or may not be useful to others outside our work-site. The development version can be installed using:

```R
# install.packages("devtools")
devtools::install_github("D-ESC/ODMr")
```
### ODBC
In order to talk to the database, you'll need to establish an ODBC connection to the ODM database on the machine that you're working on. To do this in Win7, go to Control Panel -> Administrative Tools, then choose "Data Sources (ODBC)". You'll want to add a "User DSN".

### The Basics
This will all be easier to use and understand if you have a fundamental understanding of the underlying data model. The model and the underlying motives for it's design can be found at http://www.cuahsi.org 

Here is a quick example. You'll need the package RODBC in order to establish a connection using an ODBC connection. `odbcConnect` establishes a connection to the specified DSN. `sqlFetch` is used to import the series catalog or any of the ODM tables.

```R
require(ODMr)
ODM <- odbcConnect("Connection", "User id", "Password")
Catalog = sqlFetch(ODM, "SeriesCatalog")
```

Using the established connection and referencing the series catalog we can import data from the database. The series catalog is not stable so only use this method to quickly look at certain data.

```R
Data <- ODMselect(ODM, SeriesID = 10, startDate = "2013-06-01",
  endDate = "2014-06-01")
```

A more stable reference can be had by specifying SiteID, VariableID, MethodID and QualityControlLevelID.

```R
Data <- ODMselect(ODM, SiteID = 1, VariableID = 1, MethodID = 9, QCLevelID = 0)
```

Multiple data series can be queried at once. The function makes use of the IN
operator in the underlying SQL statement to specify multiple values. 

```R
tmp = ODMselect(ODM, SiteID = c(1,5) , VariableID = 1, MethodID = 9,
  QCLevelID = 0, startDate = "2013-06-01", endDate = "2013-07-01")
```

If you have to deal a lot with time series data then it is well worth your time to become familiar with the dygraphs package. You can find out more about it at it's gitgub page http://rstudio.github.io/dygraphs/

```R
# install.packages("dygraphs")
require(dygraphs)
dygraph(Data$DataValue)
```
