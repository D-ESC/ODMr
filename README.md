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

Here is a quick example. You'll need the package RODBC in order to establish a connection using an ODBC connection. `DBI::dbConnect()` establishes a connection to the specified DSN and `ODMgetCatalog()` can be used to import the series catalog.

```R
library(ODMr)
ODM <- DBI::dbConnect(odbc::odbc(), dsn = "ODM", database = "OD", UID = "update", 
    PWD = "update", Port = 1433)
Catalog = odm_read_tbl()
```

Using the established connection and referencing the series catalog we can import data by specifying site_id, variable_id, method_id and level_id.

```{r}
Data <- odm_read(site_id = 15, variable_id = 3, method_id = 1, level_id = 1,
                  start_date = "2016-03-01", end_date = "2016-06-01")
```

Multiple data series can be queried at once. The function makes use of the IN operator in the underlying SQL statement to specify multiple values. 

```R
Data <- odm_read(site_id = c(1,5) , variable_id = 1, method_id = 9,
  level_id = 0, start_date = "2013-06-01", end_date = "2013-07-01")
```
