
odb2013<-read.csv("reference_data/ODB-2013-ScoresMatrix.csv")

dataset_names<-read.csv("reference_data/dataset-names.csv")

dataset_means<-apply(full_scores[,c("ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")],2,mean)

dataset_means_strong_initiative<-apply(full_scores[full_scores$ODB.2013.C.INIT > 5,c("ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")],2,mean)

dataset_means_2013<-apply(odb2013[,c("ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15")],2,mean)
dataset_means_2013["ODB.2013.D16"]<-0

dataset_means_2013_initiative<-apply(odb2013[odb2013$ODB.2013.C.INIT > 4,c("ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15")],2,mean)
dataset_means_2013_initiative["ODB.2013.D16"]<-0


dataset_table<-rbind(dataset_means[names(sort(dataset_means))],dataset_means_strong_initiative[names(sort(dataset_means))],dataset_means_2013[names(sort(dataset_means))],dataset_means_2013_initiative[names(sort(dataset_means))])
row.names(dataset_table)<-c("2014 all countries","2014 countries with OGD initiative","2013 all countries","2013 countries with OGD initiative")

write.csv(dataset_table,"output/datasets.csv")

## Add further contextual inforamtion to the dataset matrix
dataset_matrix<-merge(dataset_matrix,full_scores[c("Cluster","ODB.2013.C.INIT","ODB-Rank","ODB","ISO2")],by="ISO2")

## Which datasets are the most out-dated
table(dataset_matrix[dataset_matrix$gUpdated==-5,"Dataset"])


## Count the number of countries with at least one open dataset
length(unique(dataset_matrix[dataset_matrix$isOpen==1,"Country"]))


##Look at how many datasets in the top 11 countries are open.
table(dataset_matrix[dataset_matrix["ODB-Rank"] < 11,]$isOpen)



## Look at which datasets are most likely to be open

write.csv(table(dataset_matrix[dataset_matrix$isOpen == 1,"Dataset"]),"output/dataset_isopen.csv",row.names=FALSE)


## Look at which datasets are sustainable
machine_readable<-table(dataset_matrix[(dataset_matrix$cMachineReadable == 15),"Dataset"])
sustainable<-table(dataset_matrix[(dataset_matrix$hSustainable == 5),"Dataset"])
100/machine_readable*sustainable


## Generate timeliness matrix
table(dataset_matrix[dataset_matrix$gUpdated == 10,"Dataset"])
table(dataset_matrix[dataset_matrix$gUpdated == -5,"Dataset"])
table(dataset_matrix[dataset_matrix$gUpdated == 0,"Dataset"])
timeliness<-as.data.frame(t(rbind(table(dataset_matrix[dataset_matrix$gUpdated == 10,"Dataset"]),table(dataset_matrix[dataset_matrix$gUpdated == -5,"Dataset"]),table(dataset_matrix[dataset_matrix$gUpdated == 0,"Dataset"]))))
names(timeliness)<-c("Updated","Not Updated","Not Available")
write.csv(timeliness,"output/dataset_timeliness.csv")
