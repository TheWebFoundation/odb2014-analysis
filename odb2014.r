## Scripts to generate the 2014 Open Data Barometer, based on raw inputs

# This generates a number of key tables:
# dataset_matrix - containing isOpen and other useful values
# data_scores - containing raw survey scores
# data_norm - containing normalised survey scores

## Normalisation can be run against 2013 or 2014 values to compare the impacts of this
# Survey & secondary data normalisation is set on line 93
# Dataset normalisation is set around line 250

## Different weightings can be used.
# Weighting is set on line 289

## Clear workspace
rm(list = ls())

## Set up working directory (You will likely need to edit this)
setwd("~/IDRC/ODB2014/Analysis")

## We read in the raw data.
## We assume this is already ordered by country (though we make no assumption about question ordering)
raw<-read.csv("primary_data/raw-wi-aggregated.csv")

### We need to fix for the extra space that exists in Saudi Arabia's label
lev<-with(raw, levels(Country))
lev[lev=="Saudi Arabia "]<-"Saudi Arabia"
raw<-within(raw,levels(Country)<-lev)

### Now we need to get hold of some secondary data
secondary_fh<-read.csv("secondary_data/14-FH-Ordered.csv")
secondary_osi<-read.csv("secondary_data/14-UN.OSI-Ordered.csv")
secondary_wbnet<-read.csv("secondary_data/14-WB.NetUsers-Ordered.csv")
secondary_wefGCI<-read.csv("secondary_data/14-WEF.GCI.9.02-Ordered.csv")
secondary_wefGITR<-read.csv("secondary_data/14-WEF.GITR.8.01-Ordered.csv")

## Build the scores matrix
scores<-data.frame(levels(raw$Country),
                   raw[raw$Question.ID=='WI.2014.P5','Response'],
                   raw[raw$Question.ID=='WI.2014.P6','Response'],
                   raw[raw$Question.ID=='ODB.2013.C1','Response'],
                   raw[raw$Question.ID=='ODB.2013.C4','Response'],
                   raw[raw$Question.ID=='ODB.2013.C6','Response'],
                   raw[raw$Question.ID=='ODB.2013.C5','Response'],
                   raw[raw$Question.ID=='ODB.2013.C7','Response'],
                   raw[raw$Question.ID=='ODB.2013.I1','Response'],
                   raw[raw$Question.ID=='ODB.2013.I2','Response'],
                   raw[raw$Question.ID=='ODB.2013.I3','Response'],
                   raw[raw$Question.ID=='ODB.2013.I4','Response'],
                   raw[raw$Question.ID=='ODB.2013.I5','Response'],
                   raw[raw$Question.ID=='ODB.2013.I6','Response'],
                   secondary_fh[3],
                   secondary_osi[3],
                   secondary_wbnet[4],
                   secondary_wefGCI[4],
                   secondary_wefGITR[4]
                   )

colnames(scores) <- c("Country","ODB.2013.C.DPL","ODB.2013.C.RTI","ODB.2013.C.INIT","ODB.2013.C.CSOC","ODB.2013.C.SUPIN","ODB.2013.C.CITY",
                      "ODB.2013.C.TRAIN","ODB.2013.I.GOV","ODB.2013.I.ACCOUNT","ODB.2013.I.ENV","ODB.2013.I.INC","ODB.2013.I.ECON","ODB.2013.I.ENTR",
                      "FH","UN.OSI","WB.NetUsers","WEF.GCI.9.02","WEF.GITR.8.01")


## Normalise data

### Read in the 2013 Mean and Standard Deviation

stDev13<-read.csv("reference_data/13-StdDev.csv")

### Generate a 2014 set for comparison
avg<-sapply(scores,mean)
sd<-sapply(scores,sd)

stDev14<-data.frame(names(avg[2:length(avg)]),avg[2:length(avg)],sd[2:length(avg)])
colnames(stDev14) <- c("Variable","Mean","StdDev")

## Z-Score the data

### Setup Function
# Val = value; var = variable to look for; ref = the stDev values to use
zscore<-function(val,var,ref) { 
   return((val - ref[ref$Variable==var,]$Mean)/ref[ref$Variable==var,]$StdDev)
}


## This can then be applied to a single value with:
# mapply(zscore,scores$ODB.2013.C.DPL,MoreArgs=list("ODB.2013.C.DPL",stDev14))
## But to apply it to the whole matrix we run the loop below
## To choose whether to use 2013 or 2014 StDev change the StDev14 
norm_scores = data.frame(levels(raw$Country))
colnames(norm_scores)<-c("Country")

for (i in 2:length(names(scores))) { 
  norm_scores[names(scores[i])[1]]<-mapply(zscore,scores[i],MoreArgs=list(names(scores[i])[1],stDev13))
}



### DATASETS ANALYSIS
### This function helps generate the matrix we need later
dataScore <- function(dataset,a,b,c,d,e,f,g,h,i,j) {
  score = 0
  isOpen = 0
  matrix = c(0,0,0,0,0,0,0,0,0,0)
  if(a) {
    score = 5
    matrix[1]<-5
    if(b) {
      score = score + 10
      matrix[2]<-10
      
      # Lose five points for not being timely. 
      if(!g) {
        score = score - 5
        matrix[7]<- -5
      }
      
      if(c) {
        score = score + 15
        matrix[3]<-15
        if(d) {
          score = score + 15
          matrix[4]<-15
        }
        if(e) {
          score = score + 15
          matrix[5]<-15
        }
        if(f) {
          score = score + 15
          matrix[6]<-15
        }
        # Gain 10 points if timely
        if(g) {
          score = score + 10
          matrix[7]<-10
        }
        
        if(h) {
          score = score + 5
          matrix[8]<-5
        }
        if(i) {
          score = score + 5
          matrix[9]<-5
        }
        if(j) {
          score = score + 5
          matrix[10]<-5
        }
      }
    }
  }
  
  if(matrix[3] & matrix[4] & matrix[5] & matrix[6]) {
    isOpen = 1
  }
  return(c(score,isOpen,matrix))
}

data_raw_full <-read.csv("primary_data/raw-aggregated.csv")
data_raw = data_raw_full[1:3]
data_raw = data_raw[grep("ODB.2013.D",data_raw$Question),]

### We need to fix for the extra space that exists in Saudi Arabia's label again
lev<-with(data_raw, levels(Country))
lev[lev=="Saudi Arabia "]<-"Saudi Arabia"
data_raw<-within(data_raw,levels(Country)<-lev)


## Setup blank output frames 
data_scores = data.frame(levels(data_raw$Country),rep(0,86),rep(0,86),rep(0,86),
                         rep(0,86),rep(0,86),rep(0,86),rep(0,86),rep(0,86),rep(0,86),
                         rep(0,86),rep(0,86),rep(0,86),rep(0,86),rep(0,86),rep(0,86))
colnames(data_scores) <-c("Country","ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")

dataset_matrix = vector()

## Now loop through the countries and build the dataset matrix
for(country in levels(data_raw$Country)) {

  country_data = data_raw[data_raw$Country==country,]
  
  datasets = list(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16)
  for(set in datasets) {
      tmp = dataScore(paste("ODB.2013.D",set,sep=""),
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".a",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".b",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".c",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".d",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".e",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".f",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".g",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".h",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".i",sep=""),]$Response,
      country_data[country_data$Question.ID==paste("ODB.2013.D",set,".j",sep=""),]$Response)
      
      data_scores[data_scores$Country==country,paste("ODB.2013.D",set,sep="")]<-tmp[1]
      
      dataset_matrix<-append(dataset_matrix,c(country,paste("ODB.2013.D",set,sep=""),tmp))
  }
}

dataset_matrix<-matrix(dataset_matrix,ncol=14,byrow=TRUE)
colnames(dataset_matrix)<-c("Country","Variable","Score","isOpen","a","b","c","d","e","f","g","h","i","j")

## Merge data together
full_scores<-merge(scores,data_scores,by='Country')

regions<-read.csv("reference_data/regions.csv")

full_scores<-merge(full_scores,regions,by='Country')

## Calculate our SubIndexes
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100

subindexes = data.frame(levels(full_scores$Country))
colnames(subindexes)<-c("Country")

subindexes['Readiness_Government']<-(norm_scores$ODB.2013.C.INIT + norm_scores$WEF.GITR.8.01 + norm_scores$UN.OSI + norm_scores$ODB.2013.C.CITY)/4

subindexes['Readiness_Government-Scaled']<-round(rescale(subindexes['Readiness_Government']))

subindexes['Readiness_Citizens']<-(norm_scores$ODB.2013.C.DPL + norm_scores$ODB.2013.C.RTI + norm_scores$FH + norm_scores$ODB.2013.C.CSOC )/4

subindexes['Readiness_Citizens-Scaled']<-round(rescale(subindexes['Readiness_Citizens']))

subindexes['Readiness_Entrepreneurs']<-(norm_scores$ODB.2013.C.TRAIN + norm_scores$ODB.2013.C.SUPIN + norm_scores$WEF.GCI.9.02 + norm_scores$WB.NetUsers)/4

subindexes['Readiness_Entrepreneurs-Scaled']<-round(rescale(subindexes['Readiness_Entrepreneurs']))

subindexes['Readiness'] = 1/3*subindexes['Readiness_Government'] + 1/3*subindexes['Readiness_Citizens'] + 1/3*subindexes['Readiness_Entrepreneurs']

subindexes['Readiness-Scaled']<-round(rescale(subindexes['Readiness']))

subindexes['Readiness-Rank']<-rank(-subindexes["Readiness"],ties.method="first")

subindexes['Datasets_Innovation']<-(full_scores$ODB.2013.D1 + full_scores$ODB.2013.D9 + full_scores$ODB.2013.D10	+ full_scores$ODB.2013.D13+ full_scores$ODB.2013.D16)/5

subindexes['Datasets_Society']<-(full_scores$ODB.2013.D2 + full_scores$ODB.2013.D4 + full_scores$ODB.2013.D11  + full_scores$ODB.2013.D12+ full_scores$ODB.2013.D14)/5

subindexes['Datasets_Accountability']<-(full_scores$ODB.2013.D5 + full_scores$ODB.2013.D6 + full_scores$ODB.2013.D7  + full_scores$ODB.2013.D8+ full_scores$ODB.2013.D15)/5

subindexes['Datasets_Average']<-(subindexes['Datasets_Innovation'] +subindexes['Datasets_Society']  + subindexes['Datasets_Accountability'])/ 3

# Below is normalisation against 2013
subindexes['Datasets_Normalised']<-mapply(zscore,subindexes['Datasets_Average'],MoreArgs=list('Datasets',stDev13))

# Or normalisation based on 2014
# subindexes['Datasets_Normalised']<-scale(subindexes['Datasets_Average'])

subindexes['Implementation']<-subindexes['Datasets_Normalised']

subindexes['Implementation-Scaled']<-round(rescale(subindexes['Implementation']))

subindexes['Implementation-Rank']<-rank(-subindexes["Implementation"],ties.method="first")


subindexes['Impact_Political']<-(norm_scores$ODB.2013.I.GOV + norm_scores$ODB.2013.I.ACCOUNT)/2

subindexes['Impact_Political-Scaled']<-round(rescale(subindexes['Impact_Political']))

subindexes['Impact_Social']<-(norm_scores$ODB.2013.I.ENV + norm_scores$ODB.2013.I.INC)/2

subindexes['Impact_Social-Scaled']<-round(rescale(subindexes['Impact_Social']))

subindexes['Impact_Economic']<-(norm_scores$ODB.2013.I.ECON + norm_scores$ODB.2013.I.ENTR)/2

subindexes['Impact_Economic-Scaled']<-round(rescale(subindexes['Impact_Economic']))

subindexes['Impact'] = 1/3*subindexes['Impact_Political'] + 1/3*subindexes['Impact_Social'] + 1/3*subindexes['Impact_Economic']

subindexes['Impact-Scaled']<-round(rescale(subindexes['Impact']))

subindexes['Impact-Rank']<-rank(-subindexes["Impact"],ties.method="first")


## Calculate Barometer 

#Using 2014 Weightings
subindexes['ODB']<-(1/4*subindexes['Readiness']+2/4*subindexes['Implementation']+1/4*subindexes['Impact'])/3

subindexes['ODB-Scaled']<-round(rescale(subindexes['ODB']),2)

# We generate the Rank off the rounded, scaled score
# This compensates against very small decimal variations between countries leading to different rank orderings, where these variations are within 
# the margin of error for scores. 

subindexes['ODB-Rank']<-round(rank(-round(subindexes["ODB-Scaled"]),ties.method="min"))

## Merge Subindexes to main table

full_scores<-merge(full_scores,subindexes,by='Country')


## Add a comparison of 2013 data to the main table

scores2013<-read.csv("reference_data/2013-ranks.csv",na.strings="#N/A")

full_scores<-merge(full_scores,scores2013,by='Country')

full_scores['Change']<-full_scores$X2013.Rank-full_scores['ODB-Rank']


## Read in the 2013 data
full2013<-read.csv("reference_data/ODB-2013-ScoresMatrix.csv")

core_rankings2013<-read.csv("reference_data/ODB-2013-Ranking-CoreColumns.csv")
names(core_rankings2013)<-c("ODB-Rank","ISO3","Country","Region","Readiness-Scaled","Implementation-Scaled","Impact-Scaled","ODB-Scaled")
core_rankings2013<-core_rankings2013[,c(1,2,5,6,7,8)]

## Fix some of the naming issues before we can attach ISO codes
lev<-with(full2013, levels(Country))
lev[lev=="Tanzania, United Republic of"]<-"Tanzania"
lev[lev=="UAE"]<-"United Arab Emirates"
lev[lev=="Venezuela"]<-"Venezuela (Bolivarian Republic of)"
full2013<-within(full2013,levels(Country)<-lev)

## Merge in the ISO code data, and then strip off the old country name
full2013<-merge(full2013,regions,by.x="Country",by.y="Web.Index.2013.Name")
full2013<-full2013[,2:length(full2013)]

#Then merge the rankings
full2013<-merge(full2013,core_rankings2013,by="ISO3")

## Add year information to both tables
full2013["Year"]<-rep(2013,77)
full_scores["Year"]<-rep(2014,86)

## Generate summary table
column_selection<-c("Year","Country","ISO2","ISO3","ODB.2013.C.INIT","ODB.2013.C.RTI","ODB.2013.C.DPL",
           "ODB.2013.C.CSOC","ODB.2013.C.CITY","ODB.2013.C.SUPIN","ODB.2013.C.TRAIN",
           "ODB.2013.I.GOV","ODB.2013.I.ACCOUNT","ODB.2013.I.ENV", "ODB.2013.I.INC",
           "ODB.2013.I.ENTR","ODB.2013.I.ECON",
           "FH","UN.OSI","WB.NetUsers","WEF.GCI.9.02","WEF.GITR.8.01",
           "ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15",
           "ODB-Rank","ODB-Scaled","Readiness-Scaled","Implementation-Scaled","Impact-Scaled")

merged_scores<-rbind(full2013[column_selection],full_scores[column_selection])

write.csv(merged_scores,"output/2013-14-Score-Comparison.csv",row.names=FALSE)


## Add in clusters
column_selection<-c("ODB.2013.C.INIT","ODB.2013.C.RTI","ODB.2013.C.DPL",
                    "FH","UN.OSI","WB.NetUsers","WEF.GCI.9.02","WEF.GITR.8.01",
                    "ODB.2013.C.CSOC","ODB.2013.C.CITY","ODB.2013.C.SUPIN","ODB.2013.C.TRAIN",
                    "ODB.2013.I.GOV","ODB.2013.I.ACCOUNT","ODB.2013.I.ENV", "ODB.2013.I.INC",
                    "ODB.2013.I.ENTR","ODB.2013.I.ECON")

d<-dist(scale(full_scores[column_selection]),method="euclidian")
fit <- hclust(d, method="ward.D")
groups <- cutree(fit, k=4)
full_scores$Cluster<-as.factor(groups)
levels(full_scores$Cluster)[as.numeric(full_scores$Cluster[full_scores$Country=="UK"])]<-"High capacity"
levels(full_scores$Cluster)[as.numeric(full_scores$Cluster[full_scores$Country=="India"])]<-"Emerging and advancing"
levels(full_scores$Cluster)[as.numeric(full_scores$Cluster[full_scores$Country=="Nepal"])]<-"Capacity constrained"
levels(full_scores$Cluster)[as.numeric(full_scores$Cluster[full_scores$Country=="Qatar"])]<-"One sided initiative"

### Write out rankings
hdi_data <- read.csv("reference_data/HDI and Stats.csv")

full_scores<-merge(full_scores,hdi_data[c("HDI","HDI.Rank","GDP.Per.Capita.Current.USD","ISO3")],by="ISO3")
compare<-round(full_scores['ODB-Scaled']-full_scores$X2013.Scaled.Score,2)
names(compare)<-"ODB.Change"
full_scores<-cbind(full_scores,compare)

column_selection <- c("Year","Region","Income","ISO2","ISO3","ODB-Rank","Country","ODB","Readiness","Implementation","Impact",
                    "ODB-Scaled","Readiness-Scaled","Implementation-Scaled","Impact-Scaled","Readiness_Government",
                    "Readiness_Government-Scaled","Readiness_Citizens","Readiness_Citizens-Scaled","Readiness_Entrepreneurs","Readiness_Entrepreneurs-Scaled","Readiness-Rank",
                    "Datasets_Innovation","Datasets_Society","Datasets_Accountability","Datasets_Average","Datasets_Normalised","Implementation","Implementation-Scaled",
                    "Implementation-Rank","Impact_Political","Impact_Political-Scaled","Impact_Social","Impact_Social-Scaled","Impact_Economic","Impact_Economic-Scaled",
                    "HDI","G20","G7","GDP.Per.Capita.Current.USD","X2013.Rank","X2013.Scaled.Score","ODB.Change","Change","Cluster")

## We have to deal with a few differences in column naming between 2013 and 2014
rankings_csv<-full_scores[column_selection]
names(rankings_csv)<-c("Year","Region","Income","ISO2","ISO3","ODB-Rank","Country","ODB-Score","Readiness","Implementation","Impact",
                       "ODB-Scaled","Readiness-Scaled","Implementation-Scaled","Impact-Scaled","Readiness_Government",
                       "Readiness_Government-Scaled","Readiness_Citizens","Readiness_Citizens-Scaled","Readiness_Entrepreneurs","Readiness_Entrepreneurs-Scaled","Readiness-Rank",
                       "Datasets_Innovation","Datasets_Social_Policy","Datasets_Accountability","Datasets_Average","Datasets_Normalised","Implementation","Implementation-Scaled",
                       "Implementation-Rank","Impact_Political","Impact_Political-Scaled","Impact_Social","Impact_Social-Scaled","Impact_Economic","Impact_Economic-Scaled",
                       "HDI","G20","G7","GDP-Per-Capita-Current-USD","2013-Rank","2013-ODB-Scaled","ODB Change","Rank Change","Cluster")

write.csv(rankings_csv,"output/ODB-2014-Rankings.csv",row.names=FALSE,na="")


### Write out matrix

dataset_names<-read.csv("reference_data/dataset-names.csv")
dataset_matrix<-merge(dataset_matrix,dataset_names,by="Variable")
dataset_matrix<-merge(dataset_matrix,regions[c("Country","ISO2")],by="Country")

dataset_matrix<-dataset_matrix[c("Variable","Country","ISO2","Dataset","Score","isOpen","a","b","c","d","e","f","g","h","i","j")]
names(dataset_matrix)<-c("Variable","Country","ISO2","Dataset","CalculatedScore","isOpen","aExists","bAvailable","cMachineReadable","dBulk","eFree","fLicense","gUpdated","hSustainable","iDiscoverable","jLinked")

#Sort matrix
dataset_matrix<-dataset_matrix[order(as.integer(gsub("ODB.2013.D","",dataset_matrix[,1]))),]
dataset_matrix<-dataset_matrix[order(dataset_matrix$Country),]

write.csv(dataset_matrix,"output/ODB-2014-Datasets-Scored.csv",row.names=FALSE)


### Write out survey data

survey_cols<- c("Country","Region","G20","HDI","Income","ISO2","ISO3","ODB.2013.C.DPL","ODB.2013.C.RTI","ODB.2013.C.INIT","ODB.2013.C.CSOC","ODB.2013.C.SUPIN","ODB.2013.C.CITY","ODB.2013.C.TRAIN","ODB.2013.I.GOV","ODB.2013.I.ACCOUNT","ODB.2013.I.ENV","ODB.2013.I.INC","ODB.2013.I.ECON","ODB.2013.I.ENTR","ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")
names(survey_cols)<-c("Country","Region","G20","HDI","Income","ISO2","ISO3","ODB.2013.C.DPL","ODB.2013.C.RTI","ODB.2013.C.INIT","ODB.2013.C.CSOC","ODB.2013.C.SUPIN","ODB.2013.C.CITY","ODB.2013.C.TRAIN","ODB.2013.I.GOV","ODB.2013.I.ACCOUNT","ODB.2013.I.ENV","ODB.2013.I.INC","ODB.2013.I.ECON","ODB.2013.I.ENTR","ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")
write.csv(full_scores[survey_cols],"output/ODB-2014-Survey-Ordered.csv",row.names=FALSE)

raw_out<-raw
lev<-with(raw_out, levels(Question.ID))
lev[lev=="WI.2014.P5"]<-"ODB.2013.C.DPL"
lev[lev=="WI.2014.P6"]<-"ODB.2013.C.RTI"
lev[lev=="ODB.2013.C1"]<-"ODB.2013.C.INIT"
lev[lev=="ODB.2013.C4"]<-"ODB.2013.C.CSOC"
lev[lev=="ODB.2013.C5"]<-"ODB.2013.C.SUPIN"
lev[lev=="ODB.2013.C6"]<-"ODB.2013.C.CITY"
lev[lev=="ODB.2013.C7"]<-"ODB.2013.C.TRAIN"
lev[lev=="ODB.2013.I1"]<-"ODB.2013.I.GOV"
lev[lev=="ODB.2013.I2"]<-"ODB.2013.I.ACCOUNT"
lev[lev=="ODB.2013.I3"]<-"ODB.2013.I.ENV"
lev[lev=="ODB.2013.I4"]<-"ODB.2013.I.INC"
lev[lev=="ODB.2013.I5"]<-"ODB.2013.I.ECON"
lev[lev=="ODB.2013.I6"]<-"ODB.2013.I.ENTR"
raw_out<-within(raw_out,levels(Question.ID)<-lev)

write.csv(raw_out[order(raw_out$Country),c("Country","Question.ID","Response","Justification")],"output/primary_data_context_impact.csv",row.names=FALSE)
