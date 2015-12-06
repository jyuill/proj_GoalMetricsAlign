
## Open rga package
library(rga)
# open dplyr
library(dplyr)

# Get authorization to enable use of GA API
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
rga.open(instance="ga") # open browser get auth code and paste in console below

### 1. SELECT VIEW(s) 
## get complete list of Profiles (Views) if needed - not needed if already in viewlist.csv file
## if desired views already in viewlist.csv, skip to next ## section below
gaProfiles <- ga$getProfiles()
gaProfiles1<- gaProfiles[,c("webPropertyId","name","id","websiteUrl")]
gaProfiles1 <- gaProfiles1[order(gaProfiles1$webPropertyId,gaProfiles1$name),]
rownames(gaProfiles1) <- NULL

# List desired view names (if not already in existing viewlist.csv file)
viewname1 <- "01 NFS Main"
viewname2 <- "01 TS Main"

# filter view list for desired names and id
viewlist <- filter(gaProfiles1,name==viewname1|name==viewname2) %>% select(name,id)
# write to csv to avoid above process in future unless necessary to get new views
write.csv(viewlist,file="viewlist.csv", row.names=FALSE)
## 

## Read in viewlist table (whether created previously or above)
gaViews <- read.csv("viewlist.csv", header=TRUE)
gaViewSelect <- select(gaViews,name,id)
nviews <- length(gaViewSelect)

### 2. SELECT DATE RANGE
startdate="2015-12-01"
enddate="2015-12-01"
###

### 3. SELECT METRICS
# common metrics
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

## THIS SECTION SHOWS HOW TO GET GOALS FOR SINGLE VIEW
# filter for selected view and get goal numbers for selected goals
#goalv1 <- filter(goalslots,View.id==view1) %>% select(goalcols)
viewgoals <- filter(goalslots,View.id==gaViewSelect[1,2]) %>% select(goalcols)
length(viewgoals)
#
allgoals <- NULL
allgoalsrename <- NULL
for(g in 1:length(viewgoals)){
    eachgoal <- paste(c("ga:goal",viewgoals[,g],"Completions"),collapse="")
    allgoalsrename <- c(allgoals,eachgoal)
    allgoals <- paste(c(allgoals,eachgoal),collapse=",")
}
# combine list of goals with other metrics selected above
allmetrics <- paste(c(metriclist,allgoals),collapse=",")
allmetrics
##

## TEST - rename cols to match friendly goal names for consistency across views
# uses GAdata created by running initial GA query
# process:
# 
# each query needs GA goal names
# subsequent queries for next view can't use these for rbind
# GAdataview has to use goal names GA can recognize
# GAdata has to match friendly consistent names
# when data is collected in GAdataview needs to be converted for adding to GAdata
# how to do that?
# 
# cols include dimensions and non-goal metrics - they don't need to be changed
# unknown number of dimensions and non-goal metrics
# but cols that need change should be known because they are derived from 'allgoals'
# need to revert back from 'allgoals' to goals variable - same order
# easier to do it pre-collapse into 'allgoals'
# consistent names are in goalslots table 

# created new variable allgoalsrename -> goals that correspond to goals variable in same order
# how to use goals variable to rename allgoalsrename
allgoalsrename <- goals # works great for simple list but need to rename data frame cols
# to determine col numbers start with length of dimension list, add length of metricslist
# use length of goals list
lmd <- length(metric) + length(dimen)+1
lg <- length(goals)-1
lgm <- lmd+lg
names(GAdataview)[lmd:lgm] <- goals


##

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
    viewgoals <- filter(goalslots,View.id==gaViewSelect[i,2]) %>% select(goalcols)
    allgoals <- NULL
    for(g in 1:length(viewgoals)){
        eachgoal <- paste(c("ga:goal",viewgoals[,g],"Completions"),collapse="")
        allgoals <- paste(c(allgoals,eachgoal),collapse=",")
        #allgoalsrename <- c(allgoals,eachgoal) # for renaming back to friendly consistent names
    }
    # combine list of goals with other metrics selected above
    allmetrics <- paste(c(metriclist,allgoals),collapse=",")
    #
    GAdataview <- ga$getData(gaViewSelect[i,"id"],                         
                             start.date=startdate, 
                             end.date=enddate, 
                             metrics=allmetrics, 
                             dimensions=dimenlist, 
                             sort="", 
                             filters="",
                             segment="",
                             max=60000)
    
    # API is limited to 10,000 rows :[
    # change col names to be consistent across views - needed for goals
    lmd <- length(metric) + length(dimen)+1 # get number of cols for dims and std metrics to get start of goals
    lg <- length(goals)-1 # determine goal cols
    lgm <- lmd+lg # set end col for goals
    names(GAdataview)[lmd:lgm] <- goals # rename cols
    # add col to hold view name
    GAdataview$viewName <- gaViewSelect[i,"name"]
    GAdata <- rbind(GAdata, GAdataview)
}
View(GAdata)