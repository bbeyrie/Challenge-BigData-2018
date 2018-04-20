############################################################################################################################################
############################################################################################################################################
############################################# FONCTIONS ####################################################################################
############################################################################################################################################
############################################################################################################################################
transformVille = function(data,sens=1){
  
  if (sens==1)
  {
    data$insee[which(data$insee == 6088001)] = c("Nice")
    data$insee[which(data$insee == 31069001)] = c("Toulouse Blagnac")
    data$insee[which(data$insee == 33281001)] = c("Bordeaux-Mérignac")
    data$insee[which(data$insee == 35281001)] = c("Rennes")
    data$insee[which(data$insee == 59343001)] = c("Lille Lesquin")
    data$insee[which(data$insee == 67124001)] = c("Strasbourg Entzheim")
    data$insee[which(data$insee == 75114001)] = c("Paris-Montsouris")
  }
  else
  {
    data$insee[which(data$insee == "Nice")] = 6088001
    data$insee[which(data$insee == "Toulouse Blagnac")] = 31069001
    data$insee[which(data$insee == "Bordeaux-Mérignac")] = 33281001
    data$insee[which(data$insee == "Rennes")] = 35281001
    data$insee[which(data$insee == "Lille Lesquin")] = 59343001
    data$insee[which(data$insee == "Strasbourg Entzheim")] = 67124001
    data$insee[which(data$insee == "Paris-Montsouris")] = 75114001
  }
  return(data)
  
}


separateVille = function(data,station){
  
  return(data[which(data$insee == station),])
  
}



calculRegressionResiduOrdre2 = function(dataMeteo,dataTest=NULL,methodRegression="pls",
                                        ssize = 0.5,preprocs = c("scale","BoxCox","zv"),ctrl = "cv",nbctrl = 10,paramGrid = NULL,mod = NULL, ordre = 2){
  library(caret)

  # Indices echantillon d'apprentissage
  inTrain = sample(1:length(dataMeteo[,1]), size = floor(length(dataMeteo[,1])*ssize), replace = FALSE)
  
  trainPred = matrix(0,nrow = floor(length(dataMeteo[,1])*ssize), ncol = ordre)
  testPred = dataTest$tH2
  
  dataMeteo$residu = dataMeteo$tH2_obs - dataMeteo$tH2
  
  X=dataMeteo[,-which(names(dataMeteo) == "residu")]
  X = X[,-which(names(X) == "tH2")]
  X = X[,-which(names(X) == "tH2_obs")]
  Y=dataMeteo$residu
  
  
  # Extraction des échantillons
  trainDescr=X[inTrain,]
  trainY=Y[inTrain]
  
  
  # Normalisation
  xTrans=preProcess(trainDescr, method = preprocs) 
  
  trainDescr=predict(xTrans,trainDescr)
  
  if (is.null(mod) == FALSE){
    trainDescr = ExtractIdStat(trainDescr,mode=mod)
  }
  
  
  
  # Choix de la validation croisée
  Control=trainControl(method=ctrl,number=nbctrl)
  
  
  
  # Creation du classifieur
  monClassifieur = train(trainDescr, trainY,
                         method = methodRegression, tuneGrid = paramGrid,
                         trControl = Control)
  
  
  trainPred[,1] = predict(monClassifieur, newdata = trainDescr)
  
  
  # Donnees test
  testDescr = dataTest[,-which(names(dataTest)=="tH2")]
  testDescr=predict(xTrans,testDescr)
  if (is.null(mod) == FALSE){
    testDescr = ExtractIdStat(testDescr,mode=mod)
  }
  
  
  # Prediction
  testPred = testPred + predict(monClassifieur, newdata = testDescr)
  
  
  
  
  for (i in 2:(ordre)){
    dataMeteo = cbind(trainDescr,trainY,trainPred[,i-1])
    dataMeteo$residu = trainY - dataMeteo$`trainPred[, i - 1]`
    
    X=dataMeteo[,-which(names(dataMeteo) == "residu")]
    X = X[,-which(names(X) == "trainY")]
    X = X[,-which(names(X) == "trainPred[, i - 1]")]
    Y=dataMeteo$residu

    
    # Extraction des échantillons
    trainDescr=X
    trainY=Y
    
    # Creation du classifieur
    monClassifieur = train(trainDescr, trainY,
                           method = methodRegression, tuneGrid = paramGrid,
                           trControl = Control)
    
    
    # Donnees test
    trainPred[,i] = predict(monClassifieur, newdata = trainDescr)
    
    # Prediction
    testPred = testPred + predict(monClassifieur, newdata = testDescr)
  }
  
  
  
  return(testPred)
}




############################################################################################################################################
############################################################################################################################################
############################################# EXTRACTION DES DONNEES #######################################################################
############################################################################################################################################
############################################################################################################################################

# Lecture fichier test
library(lubridate)
PathTest = "D:/Benjamin/Documents/ProgC/M2_2017/ML/challenge/data_meteo/test.csv"
Meteo_DataTest=read.csv(PathTest,header=TRUE,sep=";", skipNul = FALSE)




Meteo_DataTest=data.frame(lapply(Meteo_DataTest, function(x) {gsub("\\,", "\\.", x)}))
Meteo_DataTest_date = Meteo_DataTest$date
Meteo_DataTest_mois = Meteo_DataTest$mois
Meteo_DataTest$Mois = as.numeric(substr(as.character(Meteo_DataTest$date),6,7))
Meteo_DataTest$Jour = yday(Meteo_DataTest$date)
Meteo_DataTest = Meteo_DataTest[,-which(names(Meteo_DataTest) == "date")]
Meteo_DataTest = Meteo_DataTest[,-which(names(Meteo_DataTest) == "mois")]
Meteo_DataTest=data.frame(lapply(Meteo_DataTest, function(x) {as.numeric(as.character(x))}))
Meteo_DataTest = transformVille(Meteo_DataTest)
Meteo_DataTest$Jour[which(Meteo_DataTest$Jour == 366)] = 365
Meteo_DataTest$ech2 = Meteo_DataTest$ech
Meteo_DataTest$ech3= Meteo_DataTest$ech
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(1))] = 1
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(2))] = 2
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(3,4))] = 3
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(5,6,7,8))] = 4
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(9,10,11,12))] = 5
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(13,14,15,16,17,18))] = 6
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(19,20,21,22,23,24))] = 7
Meteo_DataTest$ech2[which(Meteo_DataTest$ech %in%  c(25,26,27,28,29,30,31,32,33,34,35,36))] = 8
Meteo_DataTest$Heure = Meteo_DataTest$ech %% 24
Meteo_DataTest$Heure[which(Meteo_DataTest$ech3 == 0)] = 24
Meteo_DataTest$Saison[which(Meteo_DataTest$Jour %in%  c(80:171))] = 1
Meteo_DataTest$Saison[which(Meteo_DataTest$Jour %in%  c(172:263))] = 2
Meteo_DataTest$Saison[which(Meteo_DataTest$Jour %in%  c(264:354))] = 3
Meteo_DataTest$Saison[which(Meteo_DataTest$Jour %in%  c(1:79,355:365))] = 4





# Lecture fichier entrainement ALL
Meteo_Data = NULL
for (i in 1:36){
  
  Path = paste0("D:/Benjamin/Documents/ProgC/M2_2017/ML/challenge/data_meteo/train_",i,".csv",sep = '')
  Meteo_Data=rbind(Meteo_Data,read.csv(Path,header=TRUE,sep=";", skipNul = FALSE))
  
}

Meteo_Data=data.frame(lapply(Meteo_Data, function(x) {gsub("\\,", "\\.", x)}))
Meteo_Data_date = Meteo_Data$date
Meteo_Data_mois = Meteo_Data$mois
Meteo_Data$Mois = as.numeric(substr(as.character(Meteo_Data$date),6,7))
Meteo_Data$Jour = yday(Meteo_Data$date)
Meteo_Data = Meteo_Data[,-which(names(Meteo_Data) == "date")]
Meteo_Data = Meteo_Data[,-which(names(Meteo_Data) == "mois")]
Meteo_Data=data.frame(lapply(Meteo_Data, function(x) {as.numeric(as.character(x))}))
Meteo_Data = transformVille(Meteo_Data,1)
Meteo_Data$Jour[which(Meteo_Data$Jour == 366)] = 365
Meteo_Data$Heure = Meteo_Data$ech
Meteo_Data$Heure= Meteo_Data$ech
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(1))] = 1
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(2))] = 2
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(3,4))] = 3
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(5,6,7,8))] = 4
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(9,10,11,12))] = 5
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(13,14,15,16,17,18))] = 6
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(19,20,21,22,23,24))] = 7
Meteo_Data$ech2[which(Meteo_Data$ech %in%  c(25,26,27,28,29,30,31,32,33,34,35,36))] = 8
Meteo_Data$ech3 = Meteo_Data$ech %% 24
Meteo_Data$ech3[which(Meteo_Data$ech3 == 0)] = 24
Meteo_Data$Saison[which(Meteo_Data$Jour %in%  c(80:171))] = 1
Meteo_Data$Saison[which(Meteo_Data$Jour %in%  c(172:263))] = 2
Meteo_Data$Saison[which(Meteo_Data$Jour %in%  c(264:354))] = 3
Meteo_Data$Saison[which(Meteo_Data$Jour %in%  c(1:79,355:365))] = 4








################################################################################################################################################




################################################################################################################################################
# Imputer les données manquantes
################################################################################################################################################

library(missRanger)

# On vire celles des données d'apprentissage
Meteo_Data3 = na.omit(Meteo_Data)
# On prédit celles des données de test
Meteo_DataTest3 = missRanger(Meteo_DataTest, pmm.k = 10)





################################################################################################################################################
# Phase de test pour optimiser les parametres du Extreme Gradient Boosting
################################################################################################################################################
# préparation des données
library(caret)


residu = Meteo_Data3$tH2_obs - Meteo_Data3$tH2
X=Meteo_Data3[,-c(1,2,21)]
summary(X)
Y=residu
summary(Y)


inTrain = sample(1:length(Y), size = 0.80*length(Y), replace = FALSE)   # Ici on prend 80% des données en entrainement et 20% validation


# Extraction des échantillons
trainDescr=X[inTrain,]
testDescr=X[-inTrain,]
trainY=Y[inTrain]
testY=Y[-inTrain]
#
#
#
# Centrage et normalisation
# Normalisation CHOISIR UN DES DIFFERENTS TYPES
xTrans=preProcess(trainDescr, method = c("scale","center","BoxCox","zv"))
#
#
trainDescr=predict(xTrans,trainDescr)
testDescr=predict(xTrans,testDescr)



dataTrainmlr = cbind(trainDescr,trainY)
dataTestmlr = cbind(testDescr,testY)

library(mlr)
#create tasks
traintask <- makeRegrTask (data = dataTrainmlr,target = "trainY")
testtask <- makeRegrTask (data = dataTestmlr,target = "testY")

#do one hot encoding`<br/> 
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)
#create learner
lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list(eval_metric="error")
#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), makeIntegerParam("max_depth",lower = 3L,upper = 10L),
                        makeNumericParam("min_child_weight",lower = 1L,upper = 4L), makeNumericParam("subsample",lower = 0.5,upper = 1),
                        makeIntegerParam("nrounds",lower = 25L,upper = 500L),makeNumericParam("eta",lower = 0.01,upper = 0.2),
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),makeNumericParam("gamma",lower = 0,upper = 0.1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = F,iters=5L)

ctrl <- makeTuneControlRandom(maxit = 100L)
ctrl <- makeTuneControlGrid(maxit = 10L)


#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = 6)

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, par.set = params, control = ctrl, show.info = T)
mytune$x










################################################################################################################################################
# ICI POUR LE WORLD RECORD !!!!
################################################################################################################################################



parametersGrid <- expand.grid(eta = c(0.061), 
                              colsample_bytree=c(0.85),
                              max_depth=c(8),
                              nrounds=c(150),
                              gamma=c(0.06),
                              min_child_weight=c(2),
                              subsample = c(0.87))


myModel = "xgbTree"
cvct = 10
siz=0.98
mectrl = "cv"

eids = NULL
prep = c("scale","center","BoxCox","zv")
degr = 3

saveme = "D:/Benjamin/Documents/ProgC/M2_2017/ML/challenge/data_meteo/test_answer_template.csv"
saveme=read.csv(saveme,header=TRUE,sep=";", skipNul = FALSE,stringsAsFactors=FALSE)
saveme = transformVille(saveme)
saveme[,4] = matrix(0,length(saveme[,3]),1)

Meteo_Data4 = transformVille(Meteo_Data3,sens=0)
Meteo_Data4[,1] = as.numeric(Meteo_Data4[,1])

Meteo_DataTest4 = transformVille(Meteo_DataTest3,sens=0)
Meteo_DataTest4[,1] = as.numeric(Meteo_DataTest4[,1])

# Code qui met 500 minutes à tourner sur un ordi avec 8 coeurs
itermax = 50
for (i in 1:itermax){
	saveme[,4] = saveme[,4] + (1/itermax)*calculRegressionResiduOrdre2(Meteo_Data4,Meteo_DataTest4,methodRegression = myModel,ssize=siz,nbctrl = cvct, ctrl = mectrl,mod = eids, preprocs = prep, ordre = degr, paramGrid = parametersGrid)
}

saveme = transformVille(saveme,sens=0)
write.table(saveme,file = "D:/Benjamin/Documents/ProgC/M2_2017/ML/challenge/data_meteo/test_filled.csv",sep=";",row.names = FALSE, quote = FALSE)
