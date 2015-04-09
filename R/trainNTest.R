trainModel = function(annotations, accelerometers=NULL, GPS=NULL, winSize=60, modelName="WaistModel", names=NULL, strat=TRUE) {
  # train a model from raw annotations, accelerometer, and GPS files

  # annotations
  labelDir = annotationsToLabels(annotations, winSize, names)
  # features
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  
  cat("\n")
  #train
  if (length(featDirs) > 0) {
    cat("training model from", length(featDirs), "devices\n")
    trainAllDir(labelDir, featDirs, modelName, winSize, names, strat)
  } else {
    stop("no data directories found")
  }
}

trainAllDir = function(labelDir, featDirs, modelName, winSize, names=NULL, strat=TRUE) {
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  rf = trainRF(labelDir, featDirs, names, strat=strat)
  hmm = trainHMM(labelDir, rf, names)
  save(rf, hmm, winSize, file=modelName)
  cat("model saved to", modelName, "\n")
}


classify = function(accelerometers=NULL, GPS=NULL, modelName, saveDir, names=NULL) {
  # classify data from accelerometer and GPS files
  winSize = loadModel(modelName, "winSize")
  
  # features
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  
  cat("\n")
  # test
  if (length(featDirs) > 0) {
    testAllDir(featDirs, modelName, saveDir, names)
  } else {
    stop("No data directories found")
  }
}


testAllDir = function(featDirs, modelName, saveDir, names=NULL) {
  if (is.null(names)) {
    names = list.files(featDirs[1])
  }
  saveDir1 = file.path(saveDir, "Temp")
  for (i in 1:length(names)) {
    testRF(featDirs, modelName, saveDir1, names[i])
    testHMM(saveDir1, modelName, saveDir, names[i])
  }
  cat("predictions saved to", saveDir, "\n")
  #unlink(saveDir1, recursive=TRUE)
}
