
## Open rga package
library(rga)

# Get authorization
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
rga.open(instance="ga") # open browser get auth code and paste in console below

### 1. SELECT VIEW(s) 

# Add desired view names to list
viewnames <- c("01 NFS Main","01 TS Main") 

nviews <- length(viewnames)

# Get view id based on name, for use in query 
gaViewSelect <- data.frame()
for(i in 1:nviews){
    gaViewi <- gaProfiles1[(gaProfiles1$name==viewnames[i]),c("name","id")]
    gaViewSelect <- rbind(gaViewSelect,gaViewi)
}
rownames(gaViewSelect) <- NULL # ids in gaViewSelect will be used in query

### 2. SELECT DATE RANGE
startdate="2015-12-01"
enddate="2015-12-01"
###

### 3. SELECT METRICS

users <- "ga:users"
sessions <- "ga:sessions"
threepg <- "threepg" # populate eventually with specific goal for the view
buypgcr <- "ga:goal2ConversionRate"
socialfollow <- "ga:goal17Completions"
videogoal <- "ga:goal15Completions"

## Specify metrics to use by assiging to variables for use in queries

# this needs to be populated with correct goal number for each profile
metric <- c(sessions,threepg)

metriclist <- paste(metric,collapse=",")



### 4. SELECT DIMENSIONS

# Sample dimensions, assigned to variables
# added others if needed
datedim <- "ga:date"
yr <- "ga:year"
mth <- "ga:month"

# Select dimensions for use in query
dimen <- c(yr)
dimenlist <- paste(dimen, collapse=",")
dimenlist

### 7. RUN QUERY
#### MAIN PART OF PROCEDURE

# create dataframe
GAdata <- data.frame()
# Set i to number of views (EasView items) 
# Run loop using i with GAview - goes to number of views listed in GAview
for(i in 1:nviews){
    # need code here to identify relevant goal slots for each view
    #
    # metricslist needs to contain full list of metrics, incl ga:goalXCompletions
    #
    GAdataview <- ga$getData(gaViewSelect[i,"id"],                         
                             start.date=startdate, 
                             end.date=enddate, 
                             metrics=metriclist, 
                             dimensions=dimenlist, 
                             sort="", 
                             filters="",
                             segment="",
                             max=60000)
    
    # API is limited to 10,000 rows :[
    # add col to hold view name
    GAdataview$viewId <- gaViewSelect[i,"name"]
    GAdata <- rbind(GAdata, GAdataview)
}
View(GAdata)