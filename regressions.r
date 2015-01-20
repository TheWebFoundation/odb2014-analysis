##Looking at predictors of different kinds of impact
##Run odb2014.r first

scaled_scores<-as.data.frame(scale(full_scores[c("FH","WB.NetUsers","WEF.GCI.9.02", "WEF.GITR.8.01", "ODB.2013.C.CITY", "ODB.2013.C.INIT", "ODB.2013.C.RTI", "ODB.2013.C.CSOC", "ODB.2013.C.SUPIN", "ODB.2013.C.DPL", "ODB.2013.C.TRAIN","ODB.2013.D1","ODB.2013.D2","ODB.2013.D4","ODB.2013.D5","ODB.2013.D6","ODB.2013.D7","ODB.2013.D8","ODB.2013.D9","ODB.2013.D10","ODB.2013.D11","ODB.2013.D12","ODB.2013.D13","ODB.2013.D14","ODB.2013.D15","ODB.2013.D16")]))
income_scaled<-scale(as.numeric(as.vector(full_scores$GDP.Per.Capita.Current.USD)))
scaled_scores<-cbind(scaled_scores,income_scaled)
scaled_scores<-cbind(scaled_scores,full_scores[c("Implementation","Readiness","Impact","Impact_Political","Impact_Social","Impact_Economic","Datasets_Innovation","Datasets_Society","Datasets_Accountability","ODB")])

## Model 1. How is Political Impact predicted by core contextual variables
fit<-lm(Impact_Political ~ WB.NetUsers +FH + WEF.GCI.9.02 + WEF.GITR.8.01 + ODB.2013.C.INIT + ODB.2013.C.CITY + ODB.2013.C.RTI + ODB.2013.C.CSOC + ODB.2013.C.SUPIN + ODB.2013.C.DPL + ODB.2013.C.TRAIN,data=scaled_scores)
summary(fit)

## Model 2. How is Social Impact predicted by core contextual variables
fit<-lm(Impact_Social ~ WB.NetUsers + FH + WEF.GCI.9.02 + WEF.GITR.8.01 + ODB.2013.C.INIT + ODB.2013.C.CITY + ODB.2013.C.RTI + ODB.2013.C.CSOC + ODB.2013.C.SUPIN + ODB.2013.C.DPL + ODB.2013.C.TRAIN,data=scaled_scores)
summary(fit)

## Model 3. How far Economic Impact predicted by core contextual variables
fit<-lm(Impact_Economic ~ WB.NetUsers + FH + WEF.GCI.9.02 + WEF.GITR.8.01 + ODB.2013.C.INIT + ODB.2013.C.CITY + ODB.2013.C.RTI + ODB.2013.C.CSOC + ODB.2013.C.SUPIN + ODB.2013.C.DPL + ODB.2013.C.TRAIN,data=scaled_scores)
summary(fit)

## Model 4. How for overall impact
fit<-lm(Impact ~ WB.NetUsers + FH + WEF.GCI.9.02 + WEF.GITR.8.01 + ODB.2013.C.INIT + ODB.2013.C.CITY + ODB.2013.C.RTI + ODB.2013.C.CSOC + ODB.2013.C.SUPIN + ODB.2013.C.DPL + ODB.2013.C.TRAIN,data=scaled_scores)
summary(fit)
table(round(summary(fit)$coefficients,3))

