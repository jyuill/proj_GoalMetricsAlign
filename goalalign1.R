
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
viewname3 <- "01 SWBF"
viewname4 <- "F: FIFA MKT"
viewname5 <- "01 MEdge Main"

# filter view list for desired names and id
viewlist <- filter(gaProfiles1,name==viewname1|name==viewname2|name==viewname3|
                       name==viewname3|
                       name==viewname4|
                       name==viewname5) %>% select(name,id)
# write to csv to avoid above process in future unless necessary to get new views
write.csv(viewlist,file="viewlist.csv", row.names=FALSE)
## 

## Read in viewlist table (whether created previously or above)
gaViews <- read.csv("viewlist.csv", header=TRUE)
gaViewSelect <- select(gaViews,name,id)
nviews <- nrow(gaViewSelect) # gen number of views for looping in query

### 2. SELECT METRICS - std metrics + goals
# common metrics
users <- "ga:users"
sessions <- "ga:sessions"
pageviews <- "ga:pageviews"
timespent <- "ga:sessionDuration"
bounces <- "ga:bounces"

# add selected metrics from list above
metric <- c(users,sessions,pageviews,timespent,bounces)
# create comma-separated list of metrics to be combined with goal numbers
metriclist <- paste(metric,collapse=",")

## Get GOAL slot table
goalslots <- read.csv("goalslots.csv") # read in table
# create list of potential goals from table
g1 <- "X3pg"
g2 <- "Buypg"
g3 <- "SocialShare"
g4 <- "RCA"
# identify goals needed for query
goals <- c(g1,g2,g3,g4)

# determine col numbers of goals - can't just use values in goals variable
goalcols <- match(goals,names(goalslots))

### 3. SELECT DIMENSIONS

# Sample dimensions, assigned to variables
# added others if needed
datedim <- "ga:date"
yr <- "ga:year"
mth <- "ga:month"
newret <- "ga:userType"

# Select dimensions for use in query
dimen <- c(datedim,newret)
dimenlist <- paste(dimen, collapse=",")
dimenlist

### 4. SELECT DATE RANGE
startdate="2015-11-01"
enddate="2015-12-05"
###

### 5. RUN QUERY
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
# write to file for future use
write.csv(GAdata,file="GAdata.csv", row.names=FALSE)
# read in prev data
GAdata <- read.csv("GAdata.csv",header=TRUE)

## some quick viz -> this could also be done in a separate file
library(ggplot2)

# create a panel plot to compare variables

# scatterplot with facets: RCA relative to 3+ pgs by New v Return for each title
qplot(X3pg,RCA,data=GAdata,facets=viewName~.~userType) 

# get data for MEC only
mec <- filter(GAdata,viewName=="01 MEdge Main")
qplot(date,sessions,data=mec,col=userType) # plots sessions by date for New and Return in dots
# create same thing but with line
p1 <- ggplot(mec,aes(x=date,y=sessions,col=userType))
p1 + geom_line()

# group both new and return together by date and summarize sessions by date
mecdaily <- group_by(mec,date)
(mecdsess <- summarize(mecdaily,dsess=sum(sessions))) # need group and summarize
# create line chart with total sessions (new and return) by date
qplot(date,dsess,data=mecdsess)
plot_mecdsess <- ggplot(mecdsess,aes(x=date,y=dsess))
plot_mecdsess + geom_line()

# using the mecdaily group, sum other metrics by date
mecdsum <- summarize(mecdaily,dUser=sum(users),dsess=sum(sessions),dPV=sum(pageviews),dX3pg=sum(X3pg),dBuy=sum(Buypg),dRCA=sum(RCA))
# select data columns - remove factor cols
mecsum <- select(mecdsum,dX3pg,dBuy,dRCA)
# print separate plots
qplot(dX3pg,dBuy,data=mecdsum)
qplot(dX3pg,dRCA,data=mecdsum)

# side-by-side scatterplot (axes not nec. consistent)
par(mfrow=c(1,2))
plot(dBuy~dX3pg,data=mecdsum)
abline(lm(mecsum$dBuy~mecdsum$dX3pg)) # add linear regression line
plot(dRCA~dX3pg,data=mecdsum)
abline(lm(mecsum$dRCA~mecdsum$dX3pg)) # add linear regression line

# check correlation of metrics
cor(mecsum)

# go back to MEC without new v return combined and look at relationship
# of 3+pg and Buy pg, and 3+pg and RC by new v return
## don't know how
# can create facets with ggplot for new v return, but don't know how to do different metrics
# combinations side by side
# can do different metrics side by side with base plot, but don't know how to create facets
# could create separate objects for new v return and layout in 2x2 but would prefer not all the work
# - not conducive to rapid examination of data on different parameters
# this is right idea separately but would like both together side-by-side
# par(mfrow=c(1,2)) # doesn't work with qplot :(
qplot(X3pg,RCA,data=mec,facets=userType~.)
qplot(X3pg,Buypg,data=mec,facets=userType~.)

# showing a set of different charts together
names(mecdsum)
# user trend
# user histogram
# boxplot with sessions by userType
# scatterplot 3+pgs% v PVpS
par(mfrow=c(2,2)) # establish 2x2 grid
# par(mfrow=c(1,1)) # use to test charts individually
plot(dUser~date,data=mecdsum,type="n") # blank plot
lines(dUser~date,data=mecdsum) # add line to show user trend
hist(mecdsum$dUser) # user histogram
boxplot(mecdaily$sessions~mecdaily$userType) # boxplot sessions by usertype
plot(dPV~dX3pg,data=mecdsum) # 3+pg v PVpS with linear regression line
abline(lm(mecdsum$dPV~mecdsum$dX3pg))
