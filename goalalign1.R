
## Open rga package
library(rga)
# open dplyr
library(dplyr)

# Get authorization to enable use of GA API
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
rga.open(instance="ga") # open browser get auth code and paste in console below

### 1. SELECT VIEW(s) 
# get complete list of Profiles (Views) if needed
gaProfiles <- ga$getProfiles()
gaProfiles1<- gaProfiles[,c("webPropertyId","name","id","websiteUrl")]
gaProfiles1 <- gaProfiles1[order(gaProfiles1$webPropertyId,gaProfiles1$name),]
rownames(gaProfiles1) <- NULL

# List desired view names if needed
viewname1 <- "01 NFS Main"
viewname2 <- "01 TS Main"

# filter view list for desired names and id
viewlist <- filter(gaProfiles1,name==viewname1|name==viewname2) %>% select(name,id)
# write to csv to avoid above process in future unless necessary to get new views
write.csv(viewlist,file="viewlist.csv", row.names=FALSE)

# Read in Viewlist table
gaViews <- read.csv("viewlist.csv", header=TRUE)
gaViewSelect <- select(gaViews,id)
nviews <- length(gaViewSelect)
# TEST collect view number
#view1 <- filter(gaViewSelect,row_number()==1) # gets the number but not right kind of object for dplyr
view1 <- gaViewSelect[1,]

### 2. SELECT DATE RANGE
startdate="2015-12-01"
enddate="2015-12-01"
###

### 3. SELECT METRICS

users <- "ga:users"
sessions <- "ga:sessions"

# add selected metrics from list above
metric <- c(users,sessions)
# create comma-separated list of metrics to be combined with goal numbers
metriclist <- paste(metric,collapse=",")

## Get GOAL slot table
goalslots <- read.csv("goalslots.csv")
# create list of potential goals from table
g1 <- "X3pg"
g2 <- "Buypg"
g3 <- "SocialShare"
g4 <- "RCA"
# identify goals needed for query
goals <- c(g1,g2,g3,g4)

# determine col numbers of goals - can't just use values in goals variable
goalcols <- match(goals,names(goalslots))

# filter for selected view and get goal numbers for selected goals
goalv1 <- filter(goalslots,View.id==view1) %>% select(goalcols)
#
allgoals <- NULL
for(g in 1:length(goalv1)){
    eachgoal <- paste(c("ga:goal",goalv1[,g],"Completions"),collapse="")
    allgoals <- paste(c(allgoals,eachgoal),collapse=",")
}
# combine list of goals with other metrics selected above
allmetrics <- paste(c(metriclist,allgoals),collapse=",")
allmetrics

# NEED TO GET EACH GOAL FOR EACH VIEW
gva <- "x"
for(v in 1:nviews){
    gv <- filter(goalslots,Profile.Name==viewname[i]) %>% select(goalcols)
    gva <- c(gva,gv)
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