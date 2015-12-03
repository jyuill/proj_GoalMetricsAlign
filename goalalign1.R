
## Open rga package
library(rga)
# open dplyr
library(dplyr)

# Get authorization
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
rga.open(instance="ga") # open browser get auth code and paste in console below

### 1. SELECT VIEW(s) 

gaProfiles <- ga$getProfiles()
gaProfiles1<- gaProfiles[,c("webPropertyId","name","id","websiteUrl")]
gaProfiles1 <- gaProfiles1[order(gaProfiles1$webPropertyId,gaProfiles1$name),]
rownames(gaProfiles1) <- NULL

# Add desired view names to list
viewname1 <- "01 NFS Main"
viewname2 <- "01 TS Main"

viewlist <- filter(gaProfiles1,name==viewname1|name==viewname2) %>% select(name,id)
write.csv(viewlist,file="viewlist.csv", row.names=FALSE)
nviews <- length(viewlist)

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

## Get GOAL slot table
goalview <- read.csv()
goalslots <- read.csv("goalslots.csv")
goals <- c("Buypg")
goalcols <- match(goals,names(goalslots))

# filter for selected goal for selected view, for each view
# TEST FOR ONE VIEW, ONE GOAL
g1v1 <- filter(goalslots,Profile.Name==viewname1) %>% select(goalcols)
# populate goal variable with correct number
goal1 <- paste(c("ga:goal",g1v1,"Completions"),collapse="")

# NEED TO GET EACH GOAL FOR EACH VIEW
for(v in 1:nviews){
    gv <- filter(goalslots,Profile.Name==viewname[i]) %>% select(goalcols)
}

# this needs to be populated with correct goal number for each profile
metric <- c(sessions,goal1)

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