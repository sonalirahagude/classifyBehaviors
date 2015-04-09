trainRF = function(labelDir, featDirs, names, combineStanding=FALSE, strat=TRUE) {
  cat("loading training data\n")
  train = loadData(labelDir, featDirs, names)
  # train without the labels, load data will return labels in train[[1]] and all features in train[[2]]
  trainDat = train[[2]]
  trainDat$timestamp = NULL
  trainDat$PtID = NULL
  
  print(trainDat)
  # pre-process - center and scale features
  preProcValues = preProcess(trainDat, method = c("center", "scale"))
  
  trainDat = predict(preProcValues, trainDat)
  trainDat[is.na(trainDat)] = 0
  
  cat("training RF model\n")
  labels = train[[1]]$behavior
  trainDat = trainDat[labels!="NULL", ]
  labels = labels[labels!="NULL"]
  nsample = min(nrow(trainDat), 10000)
  if (strat) {
    nstrat = round(nsample / length(unique(labels)))
    s = stratSample(labels, 2000)
  } else {
    s = sample(nrow(trainDat), nsample)
  }
  rf = randomForest(trainDat[s, ], factor(labels[s]), sampsize=nsample)
  rf$preProcValues = preProcValues
  rf$groundTruth = labels[s]
  return(rf)
}
testRF = function(featDirs, modelName, saveDir, testNames, saved=TRUE){
  # load model
  if (saved){
    rf = loadModel(modelName, "rf")
  } else {
    rf = modelName
  }
  if (length(testNames)==0){
    stop("no test data\n")
  }
  for (i in 1:length(testNames)){
    testName = testNames[i]
    testDat = loadFeatures(featDirs,testName)
    if (nrow(testDat)>0){
      
      cat(testName, "\n")
      # remove timestamps
      timestamps = testDat$timestamp
      testDat$timestamp = NULL
    
      # pre-process - center and scale features
      cat("pre-processing\n")
      if (length(names(testDat))!=rf$preProcValues$dim[2]){
        stop("test data dimensions don't match model dimensions")
      }
      testDat = predict(rf$preProcValues, testDat)
      testDat[is.na(testDat)] = 0
  
      cat("applying RF model\n")
      pr = predict(rf,testDat)
      saveFile = file.path(saveDir,paste0(testName,".csv"))
      writePredictions(pr,timestamps,saveFile)
    }
  }
}
