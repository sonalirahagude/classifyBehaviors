

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

# start here
# parameters
## annotations - labels file or directory
## accelerometers - accelerometer data file or directory
## GPS - GPS dat file or directory
## winSize - window size (60 secs or 1 minute by default, all features will be aggregated to this size)
looXval = function(annotations, accelerometers=NULL, GPS=NULL, winSize=60, saveDir="~/xval_predictions", names=NULL, strat=TRUE) {
  # annotations
  labelDir = annotationsToLabels(annotations, winSize, names)
  # features
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  
  #train
  if (length(featDirs) > 0) {
    cat("cross-validating model from", length(featDirs), "devices\n")
    looXvalFromFeats(labelDir, featDirs, saveDir, names, strat)
  }else{
    stop("no data directories found")
  }
}
calcPerformance = function(annotations, predictions, names=NULL, winSize=NULL, combineStanding=FALSE) {
  cat("\n")
  labelDir = annotationsToLabels(annotations, winSize, names)
  calcPerformanceFromLabels(labelDir, predictions, names, combineStanding)
}

# converts annotations file to labels that can be used directly by algorithm
annotationsToLabels = function(annotations, winSize, names=NULL) {
  if (!file.exists(annotations)) {
    stop("annotation file not found")
  }
  if (file.info(annotations)$isdir){
    labelDir = paste(annotations, "Labels", as.character(winSize), sep="_")
    if (!file.exists(labelDir)) {
      cat("extracting annotations...\n")
      labels = extractLabelsDir(annotations, labelDir, winSize, names)
    }
  } else {
    labelDir = paste(file_path_sans_ext(annotations), "Labels", as.character(winSize), sep="_")
    print(labelDir)
    if (!file.exists(labelDir)) {
      cat("extracting annotations...\n")
      labels = extractLabelsSingleFile(annotations, labelDir, winSize)
    }
  }
  return(labelDir)
}
sensorsToFeatures = function(accelerometers=NULL, GPS=NULL, winSize, names=NULL) {
  featDirs = character(0)
  
  # GPS
  if (!is.null(GPS)) {
    if (!file.exists(GPS)) {
      stop("GPS file/directory not found")
    }
    if (file.info(GPS)$isdir) {
      GPSFeatDir = paste(GPS, "Features", as.character(winSize), sep="_")
      if (!file.exists(GPSFeatDir)) {
        cat("extracting GPS features...\n")
        extractFeatsPALMSDir(GPS, GPSFeatDir, winSize, names)
      }
    }else {
      GPSFeatDir = paste(file_path_sans_ext(GPS), "Features", as.character(winSize), sep="_")
      if (!file.exists(GPSFeatDir)) {
        cat("extracting GPS features...\n")
        extractFeatsPALMSOneFile(GPS, GPSFeatDir, winSize)
      }
    }
    featDirs = c(featDirs, GPSFeatDir)
  }
  
  # accelerometers
  if (!is.null(accelerometers)) {
    for (i in 1:length(accelerometers)) {
      if (!file.exists(accelerometers[i])) {
        stop("accelerometer directory not found")
      }
      accFeatDir = paste(accelerometers[i], "Features", as.character(winSize), sep="_")
      if (!file.exists(accFeatDir)) {
        cat("extracting accelerometer features...\n")
        extractAllAccDir(accelerometers[i], accFeatDir, winSize, names)
      }
      featDirs = c(featDirs, accFeatDir)
    }
  }
  return(featDirs)
}

looXvalFromFeats = function(labelDir, featDirs, saveDir, names=NULL, strat=TRUE) {
  saveDir1 = paste(saveDir, "Temp", sep="")
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  for (i in 1:length(names)) {
    cat("test subject:", names[i], "\n")
    # leave i out
    testNames = names[i]
    trainNames = names[-i]
    trainTest(trainLabelDir=labelDir, trainFeatDirs=featDirs, trainNames=trainNames, testNames=testNames, saveDir1=saveDir1, saveDir2=saveDir, strat=strat)
    cat("----------------------------------\n")
  }
  cat("Overall RF\n")
  calcPerformance(labelDir, saveDir1, names)
  cat("Overall HMM\n")
  calcPerformance(labelDir, saveDir, names)
  cat("----------------------------------\n")
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

trainTest = function(trainLabelDir, testLabelDir=NULL, trainFeatDirs, testFeatDirs=NULL, trainNames, testNames, saveDir1, saveDir2, strat=TRUE) {
  # do two-level classification
  modelName1 = "temp.rf"
  modelName2 = "temp.hmm"
  
  if (is.null(testLabelDir)) {
    testLabelDir = trainLabelDir
  }
  if (is.null(testFeatDirs)) {
    testFeatDirs = trainFeatDirs
  }
  
  # first train RF
  rf = trainRF(trainLabelDir, trainFeatDirs, trainNames, strat=strat)
  testRF(testFeatDirs, rf, saveDir1, testNames, saved=FALSE)
  # calculate performance
  cat(testNames, "\n")
  calcPerformance(testLabelDir, saveDir1, testNames)
  
  # then apply HMM smoothing to RF outputs
  hmm = trainHMM(trainLabelDir, rf, trainNames)
  testHMM(saveDir1, hmm, saveDir2, testNames, saved=FALSE)
  # calculate performance
  cat(testNames,"\n")
  calcPerformance(testLabelDir, saveDir2, testNames)
  file.remove(modelName1)
  file.remove(modelName2)
}

trainRF = function(labelDir, featDirs, names, combineStanding=FALSE, strat=TRUE) {
  cat("loading training data\n")
  train = loadData(labelDir, featDirs, names)
  # train without the labels, load data will return labels in train[[1]] and all features in train[[2]]
  trainDat = train[[2]]
  trainDat$timestamp = NULL
  trainDat$PtID = NULL
  
  # pre-process - center and scale features
  cat("pre-processing\n")
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
loadModel = function(modelName, which) {
  if (file.exists(modelName)) {
    # if the model is a path to file, load it
    load(modelName)
  } else {
    # otherwise look in package data
    data(list=modelName, package="classifyBehaviors")
  }
  if (which=="winSize") {
    return(winSize)
  } else if (which=="rf") {
    return(rf)
  } else if (which=="hmm") {
    return(hmm)
  }
}

trainHMM = function(labelDir, rf, names, combineStanding=FALSE) {
  
  # load ground truth labels
  labels = loadLabels(labelDir, names)
  labels = labels$behavior
  
  cat("training HMM\n")
  # get the unique states
  states = sort(unique(labels))
  states = states[!grepl("NULL", states)]
  symbols = sort(rf$classes)
  # compute the transition probabilities
  transProbs = computeTransProbs(labels)
  # compute the emission probabilities
  emissionProbs = computeEmissionProbs(rf)
  # compute the prior probabilities
  startProbs = computePriorProbs(labels)
  # make the HMM
  hmm = initHMM(states, symbols, startProbs, transProbs, emissionProbs)
  return(hmm)
}
testHMM = function(predDir, modelName, saveDir, names, saved=TRUE) {
  # load model
  if (saved){
    hmm = loadModel(modelName, "hmm")
  } else {
    hmm = modelName
  }
  if (length(names) == 0) {
    stop("no test data\n")
  }
  for (i in 1:length(names)) {
    name = names[i]
    # load predictions
    predictions = loadPredictions(predDir, name)
  
    if (nrow(predictions) > 0) {
    # apply HMM
    cat("applying HMM\n")
    
    post = posterior(hmm, predictions$prediction)
    filtered = as.character(factor(max.col(t(post)), levels=1:length(hmm$States), labels=hmm$States))
    filtered = viterbi(hmm, predictions$prediction)
    
    # save predictions
    saveFile = file.path(saveDir, paste0(name, ".csv"))
    writePredictions(filtered, predictions$timestamp, saveFile)
    }
  }
}
computeTransProbs = function(stateSeq) {
  # get the list of unque states
  states = sort(unique(stateSeq))
  # remove NULL
  states = states[!grepl("NULL", states)]
  S = length(states)
  # set up the transition Probability matrix
  transProbs = matrix(.Machine$double.eps, nrow=S, ncol=S)
  rownames(transProbs) = states
  colnames(transProbs) = states
  # loop through state sequence and count transitions
  x = stateSeq[1]
  i = 2
  # skip nulls until you get the first non NULL state
  while (x == "NULL") {
    x = stateSeq[i]
    i = i + 1
  }
  nNull = 0
  for (ii in i:(length(stateSeq) - 1)) {
    if (stateSeq[ii] != "NULL") {
      if (nNull <= 4){
        transProbs[x,stateSeq[ii]] = transProbs[x, stateSeq[ii]] + 1
        x = stateSeq[ii]
        nNull = 0
      } else {
        nNull = 0
      }
    } else {
      nNull = nNull + 1
    }
  }
  transProbs = transProbs / rowSums(transProbs)
  return(transProbs)
}
computeEmissionProbs = function(rf) {
  # get the list of unque states
  states = sort(rf$classes)
  symbols = sort(rf$classes)
  S = length(states)
  # set up the emission Probability matrix
  emissionProbs = matrix(0, nrow=S, ncol=S)
  rownames(emissionProbs) = states
  colnames(emissionProbs) = states
  # loop through state sequence and count transitions
  for (k in 1:length(states)) {
    # obtain all the rows where the ground truth is state[k], now, for every observation, find out the no 
    # of times the observation is emitted by state k, that is, the total number of votes for k emitting
    # some state i over all related data points i.e. all rows in the votes matrix where groundTruth == states 
    # the vote matrix contains fractional values so that each row sums to 1
    # emission matrix, rows = states, cols = symbols, that is what initHMM expects
    emissionProbs[k, ] = colSums(rf$votes[rf$groundTruth == states[k], ]) / sum(rf$groundTruth == states[k])
    # alternatively, 
    for (o in 1:length(states)) {
      emissionProbs[k,o] = sum(rf$votes[rf$groundTruth == states[k], o]) / sum(rf$groundTruth == states[k])
    }
  }
  return(emissionProbs)
}
computePriorProbs = function(stateSeq) {
  states = sort(unique(stateSeq))
  states = states[!grepl("NULL", states)]
  priorProbs = tabulate(factor(stateSeq[stateSeq != "NULL"], levels=states))
  priorProbs = priorProbs / sum(priorProbs)
  return(priorProbs)
}

loadData = function(labelDir, featDirs, names=NULL) {
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  if (length(names) == 0) {
    return(NULL)
  }
  all_data = data.frame()
  all_labels = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    labelFiles = list.files(file.path(labelDir, names[i]))
    for (k in 1:length(labelFiles)) {
      # for each day
      checkFlag = TRUE
      for (j in 1:length(featDirs)) {
        # for each feature type
        featFile = file.path(featDirs[j], names[i], file_path_sans_ext(labelFiles[k]))
        # if there is no corresponding feature file for the given label file, skip it
        # could be done in a better way, extract a list of feature files and check the list
        if (!file.exists(featFile)) {
          #skip this day
          checkFlag = FALSE
          break
        }
      }
      if (checkFlag) {
        labels = read.csv(file.path(labelDir, names[i], labelFiles[k]), stringsAsFactors=FALSE)
        data = data.frame(timestamp=labels$timestamp, stringsAsFactors=TRUE)
        for (j in 1:length(featDirs)) {
          # for each feature type
          featFile = file.path(featDirs[j], names[i], file_path_sans_ext(labelFiles[k]))
          d = read.csv(featFile, header=TRUE, stringsAsFactors=TRUE)
          data = merge(d, data, by = "timestamp", all=FALSE)
        }
        l2 = data.frame(timestamp=data$timestamp, stringsAsFactors=FALSE)
        labels = merge(labels, l2, by="timestamp", all=FALSE)
        if (nrow(data) == nrow(labels)) {
          all_data = rbind(all_data, data)
          all_labels = rbind(all_labels, labels)
        } else {
          stop(names[i], labelFiles[k], "\n")
        } 
      }
    }
  }
  return(list(all_labels, all_data))
}
loadFeatures = function(featDirs,names=NULL) {
  if (is.null(names)) {
    names = list.files(predDir)
  }
  if (length(names) == 0) {
    return(NULL)
  }
  all_feats = data.frame()
  for (i in 1:length(names)) {
    days = list.files(file.path(featDirs[1], names[i]))
    if (length(days) == 0) {
      next
    }
    for (k in 1:length(days)) {
      # for each day
      checkFlag = TRUE
      if (length(featDirs) > 1) {
        for (j in 2:length(featDirs)) {
          # for each feature type
          featFile = file.path(featDirs[j], names[i], days[k])
          if (!file.exists(featFile)) {
            #skip this day
            checkFlag = FALSE
            break
          }
        }
      }
      if (checkFlag) {
        featFile = file.path(featDirs[1], names[i], days[k])
        feats = read.csv(featFile, header=TRUE, stringsAsFactors=TRUE)
        if (length(featDirs) > 1) {
          for (j in 2:length(featDirs)) {
            # for each feature type
            featFile = file.path(featDirs[j], names[i], days[k])
            f = read.csv(featFile, header=TRUE, stringsAsFactors=TRUE)
            feats = merge(f, feats, by = "timestamp", all=FALSE)
          }
        }
        all_feats = rbind(all_feats, feats)
      }
    }
  }
  return(all_feats)
}
loadPredictionsAndLabels = function(labelDir, predDir, names=NULL) {
  if (is.null(names)) {
    names = list.files(predDir)
  }
  all_predictions = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    name = file_path_sans_ext(names[i])
    predFile = paste0(file.path(predDir, name), ".csv")
    labelFile = file.path(labelDir, name)
    
    if (file.exists(predFile) & (file.exists(labelFile) | file.exists(paste0(labelFile, ".csv")))){
      predictions = read.csv(predFile, stringsAsFactors=FALSE)
      labels = loadLabels(labelDir, name)
      predictions = merge(labels, predictions, by="timestamp", all=FALSE)
      all_predictions = rbind(all_predictions, predictions)
    }
  }
  return(all_predictions)
}
loadLabels = function(labelDir, names=NULL) {
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  all_labels = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    if (file.exists(file.path(labelDir, names[i]))){
      labelFiles = list.files(file.path(labelDir, names[i]))
      for (k in 1:length(labelFiles)) {
        # for each day
        labels = read.csv(file.path(labelDir, names[i], labelFiles[k]), stringsAsFactors=FALSE)
        all_labels = rbind(all_labels, labels)
        }
    } else {
      labels = read.csv(file.path(labelDir, names[i]), stringsAsFactors=FALSE)
      all_labels = rbind(all_labels, labels)
    }
  }
  return(all_labels)
}
loadPredictions = function(predDir, names=NULL) {
  if (is.null(names)) {
    names = list.files(predDir)
  }
  all_predictions = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    name = file_path_sans_ext(names[i])
    predFile = file.path(predDir, paste0(name, ".csv"))
    if (file.exists(predFile)) {
      predictions = read.csv(predFile, stringsAsFactors=FALSE)
      all_predictions = rbind(all_predictions, predictions)
    }
  }
  return(all_predictions)
}
writePredictions = function(values, timestamps, saveFile) {
  if (length(values) != length(timestamps)) {
    stop("lengths dont match")
  }
  if (file.exists(saveFile)) {
    warning("overwriting file")
    file.remove(saveFile)
  }
  if (!file.exists(dirname(saveFile))) {
    dir.create(dirname(saveFile), recursive=TRUE)
  }
  cat("timestamp,prediction\n", file=saveFile, sep="", append=TRUE)
  for (i in 1:length(values)) {
    cat(paste(timestamps[i], as.character(values[i]), sep = ","), "\n", file=saveFile, sep = "", append=TRUE)
  }
}
calcPerformanceFromLabels = function(labelDir, predDir, names=NULL, combineStanding=FALSE) {
  data = loadPredictionsAndLabels(labelDir, predDir, names)
  if (nrow(data) > 0) {
    pr = data[data$behavior != "NULL", c("prediction")]
    gt = data[data$behavior != "NULL", c("behavior")]
    if (combineStanding) {
      pr[grepl("Standing", pr)] = "Standing"
      gt[grepl("Standing", gt)] = "Standing"
    }
    l = unique(c(pr, gt))
    pr = factor(pr, levels=l)
    gt = factor(gt, levels=l)
    m = confusionMatrix(pr, gt)
    print(m)
    cat("\n")
    return(m)
  }
  else {
    return(NULL)
  }
}

stratSample = function(labels, nsamp) {
  t = table(labels)
  lmat = data.frame(label=labels)
  lmat$idx = as.numeric(rownames(lmat))
  samples = numeric(0)
  for (i in 1:length(names(t))) {
    s = sample(lmat[lmat$label == names(t)[i], c("idx")], nsamp, replace=TRUE)
    samples = c(samples, s)
  }
  return(samples)
}

clearFiles = function(dir) {
  if (file.exists(dir) & file.info(dir)$isdir) {
    files = list.files(dir)
    for (k in 1:length(files)) {
      file.remove(file.path(dir, files[k]))
    }
  }
}
getDateFmt = function(inputString) {
  dF1 = "%Y-%m-%d %H:%M:%S"
  dF2 = "%m/%d/%Y %H:%M:%S"
  if (!is.na(strptime(str_trim(inputString), dF1))) {
    return(dF1)
    }
  if (!is.na(strptime(str_trim(inputString), dF2))) {
    return(dF2)
  }
  return(NULL)
}
extractLabelsSingleFile = function(inputFile, outputDir, winSize) {
  # splits a record file by identifier and by days
  # column names should be identifier,StartDateTime,EndDateTime,PA1
  
  # start reading record file
  all_bouts = read.csv(inputFile, header=TRUE, stringsAsFactors=FALSE)
  # ----------------------
  #dateFmt = getDateFmt(str_trim(all_bouts[1, ]$StartDateTime))
  annotations = unique(all_bouts$behavior)
  identifiers = unique(all_bouts$identifier)
  # ----------------------
  dateFmt = "%m/%d/%Y %H:%M:%S"
  annotations = unique(all_bouts$PA1)
  identifiers = unique(all_bouts$PtID)

  actNames = sub(" ", "", annotations)
  
  for (id in 1:length(identifiers)) {
    # ----------------------
    bouts = all_bouts[all_bouts$identifier == identifiers[id], ]
    #bouts = all_bouts[all_bouts$PtID == identifiers[id], ]
    
    # ----------------------
    outputFile = file.path(outputDir, identifiers[id])
    r = 1
    l = 1
    label = "NULL"
    #------------
    boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
    boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
    #print(str_trim(bouts[r,]$DT))
    #boutstart = strptime(str_trim(bouts[r, ]$DT), dateFmt)
    #boutstop = strptime(str_trim(bouts[r, ]$DT), dateFmt)

    #print(boutstop)
    #------------
    timestamp = alignStart(winSize, boutstart)
    print(timestamp)
    day = timestamp$mday
    out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
    cat(strftime(timestamp, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    if (file.exists(out)) {
      file.remove(out)
    }
    cat("timestamp,behavior\n", file=out, append=TRUE)
    
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize <= boutstop)) {
        # the window is within this bout - add the label
        label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
      } else if (timestamp + winSize > boutstop) {
        # move on to the next bout
        if (r == nrow(bouts)) {
          break
        }
        while (timestamp + winSize > boutstop) {
          if (r == nrow(bouts)) {
            break
          }
          r = r + 1
          boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
          boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
        }
        if (timestamp >= boutstart) {
          # the window is within this bout - add the label
          label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
        }
      }
      cat(strftime(timestamp, "%Y-%m-%d %H:%M:%S,"), file=out, append=TRUE)
      cat(label, file=out, append=TRUE)
      cat("\n", file=out, append=TRUE)
      # next window
      l = l + 1
      label = "NULL"
      timestamp = as.POSIXlt(timestamp + winSize)
      if (timestamp$mday != day) {
        day = timestamp$mday
        out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
        cat(strftime(timestamp, "%Y-%m-%d"), '\n')
        if (file.exists(out)) {
          file.remove(out)
        }
        cat("timestamp,behavior\n", file=out, append=TRUE)
      }
    }
  }
  return(actNames)
}
extractLabelsDir = function(inputDir, outputDir, winSize, names = NULL) {
  # splits annotation files in a directory by days
  # column names should be identifier,StartDateTime,EndDateTime,PA1 (posture labels)
  
  files = list.files(inputDir)
  annotations = character(0)
  for (i in 1:length(files)) {
    
    
    
    # start reading record file
    bouts = read.csv(file.path(inputDir, files[i]), header=TRUE, stringsAsFactors=FALSE)
    outputFile = file.path(outputDir, file_path_sans_ext(files[i]))
    annotations = c(annotations, unique(bouts$behavior))
    # extract data format from the header of the annotation files
    dateFmt = getDateFmt(str_trim(bouts[1, ]$StartDateTime))
    
    r = 1
    l = 1
    label = "NULL"
    boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
    boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
    timestamp = alignStart(winSize, boutstart)
    
    day = timestamp$mday
    out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
    cat(strftime(timestamp, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    if (file.exists(out)) {
      file.remove(out)
    }
    cat("timestamp,behavior\n", file=out, append=TRUE)
    
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize < boutstop)) {
        # the window is within this bout - add the label
        label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
      } else if (timestamp + winSize >= boutstop) {
        # move on to the next bout
        if (r == nrow(bouts)) {
          break
        }
        while (timestamp + winSize >= boutstop) {
          if (r == nrow(bouts)) {
            break
          }
          r = r + 1
          boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
          boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
        }
        if (timestamp >= boutstart) {
          # the window is within this bout - add the label
          label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
        }
      }
      cat(strftime(timestamp, "%Y-%m-%d %H:%M:%S,"), file=out, append=TRUE)
      cat(label, file=out, append=TRUE)
      cat("\n", file=out, append=TRUE)
      # next window
      l = l + 1
      label = "NULL"
      timestamp = as.POSIXlt(timestamp + winSize)
      if (timestamp$mday != day) {
        day = timestamp$mday
        out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
        cat(strftime(timestamp, "%Y-%m-%d"), '\n')
        if (file.exists(out)) {
          file.remove(out)
        }
        cat("timestamp,behavior\n", file=out, append=TRUE)
      }
    }
  }
}
extractFeatsPALMSOneFile = function(inputFile, outputDir, winSize) {
  # splits GPS file from PALMS by days
  
  all_GPS = read.csv(inputFile, header=TRUE, stringsAsFactors=FALSE)
  #figure out the sample rate
  t1 = strptime(all_GPS[1, c("dateTime")], "%Y-%m-%d %H:%M:%S")
  t2 = strptime(all_GPS[2, c("dateTime")], "%Y-%m-%d %H:%M:%S")
  sampleRate = as.numeric(t2 - t1)
  ws = winSize / sampleRate
  
  identifiers = unique(all_GPS$identifier)
  for (id in 1:length(identifiers)) {
    cat(identifiers[id], "...\n")
    cols = c("identifier","dateTime","speed","distance","duration","ele","elevationDelta","lat","lon","nsatUsed","nsatView","snrUsed","snrView","fixType")
    GPS = all_GPS[all_GPS$identifier == identifiers[id], cols]
    outputFile = file.path(outputDir, identifiers[id])
    r = 1
    lastCoordinates = GPS[r, c("lat", "lon")]
    st = strptime(GPS[r, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    newStart = alignStart(winSize, st)
    while (newStart > st) {
      lastCoordinates = GPS[r, c("lat", "lon")]
      r = r + 1
      st = strptime(GPS[r, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    }
    day = st$mday
    out = file.path(outputFile, strftime(st, "%Y-%m-%d"))
    cat(strftime(st, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    cat("timestamp,avgspeed,sdspeed,coefvar,netdistance,totaldistance,ratiodistance,elechange,avgele,nsat,snr\n", file=out, append=TRUE)
    
    # first feature
    while ((r + ws - 1) <= nrow(GPS)) {
      
      if (st$mday != day) {
        out = file.path(outputFile,strftime(st, "%Y-%m-%d"))
        cat(strftime(st, "%Y-%m-%d"), '\n')
        cat("timestamp,avgspeed,sdspeed,coefvar,netdistance,totaldistance,ratiodistance,elechange,avgele,nsat,snr\n", file=out, append=TRUE)
        day = st$mday
      }
      feat = computeOneGPSFeat(GPS[r:(r + ws - 1), ], lastCoordinates)
      st = strptime(GPS[r, c("dateTime")],"%Y-%m-%d %H:%M:%S")
      cat(strftime(st, "%Y-%m-%d %H:%M:%S,"), file=out, sep = "", append=TRUE)
      cat(feat, file=out, sep=",", append=TRUE)
      cat('\n', file=out, append=TRUE)
      lastCoordinates = GPS[r, c("lat","lon")]
      r = r + ws
      
    }
  }
}

# extract relevant GPS features from the GPS data file
extractFeatsPALMSDir = function(inputDir, outputDir, winSize, names = NULL) {
  # splits GPS file from PALMS by days
  if (is.null(names)) {
    names = list.files(inputDir)
  }
  for (i in 1:length(names)) {
    cat(names[i], "...\n")
    GPS = read.csv(file.path(inputDir, names[i]), header=TRUE, stringsAsFactors=FALSE)
    #figure out the sample rate
    t1 = strptime(GPS[1, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    t2 = strptime(GPS[2, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    sampleRate = as.numeric(t2 - t1)
    ws = winSize / sampleRate
  
    cols = c("identifier","dateTime","speed","distance","duration","ele","elevationDelta","lat","lon","nsatUsed","nsatView","snrUsed","snrView","fixType")
    outputFile = file.path(outputDir, file_path_sans_ext(names[i]))
    r = 1
    lastCoordinates = GPS[r, c("lat","lon")]
    st = strptime(GPS[r, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    newStart = alignStart(winSize, st)
    while (newStart > st) {
      lastCoordinates = GPS[r, c("lat", "lon")]
      r = r + 1
      st = strptime(GPS[r, c("dateTime")], "%Y-%m-%d %H:%M:%S")
    }
    day = st$mday
    out = file.path(outputFile, strftime(st, "%Y-%m-%d"))
    cat(strftime(st, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    cat("timestamp,avgspeed,sdspeed,coefvar,netdistance,totaldistance,ratiodistance,elechange,avgele,nsat,snr\n", file=out, append=TRUE)
    
    # first feature
    while ((r + ws - 1) <= nrow(GPS)) {
      
      if (st$mday != day) {
        out = file.path(outputFile, strftime(st, "%Y-%m-%d"))
        cat(strftime(st, "%Y-%m-%d"), '\n')
        cat("timestamp,avgspeed,sdspeed,coefvar,netdistance,totaldistance,ratiodistance,elechange,avgele,nsat,snr\n", file=out, append=TRUE)
        day = st$mday
      }
      feat = computeOneGPSFeat(GPS[r:(r + ws - 1), ], lastCoordinates)
      st = strptime(GPS[r, c("dateTime")], "%Y-%m-%d %H:%M:%S")
      cat(strftime(st, "%Y-%m-%d %H:%M:%S,"), file=out, sep = "", append=TRUE)
      cat(feat, file=out, sep=",", append=TRUE)
      cat('\n', file=out, append=TRUE)
      lastCoordinates = GPS[r, c("lat", "lon")]
      r = r + ws
    }
  }
}

# agregates features over a window size, so returns features for one data point
computeOneGPSFeat = function(w, lastCoordinates) {
  #input: identifier,dateTime,speed,distance,duration,ele,elevationDelta,lat,lon,nsatUsed,nsatView,snrUsed,snrView,fixType
  fAvgSpeed = mean(w[, c("speed")])  # average speed
  fSdSpeed = sd(w[, c("speed")])  # std of speed
  if (fAvgSpeed > 0) {
    fCoefVar = fSdSpeed / fAvgSpeed  # coefficient of variation
  } else {
    fCoefVar = 0
  }
  fNetDistance = distance(lastCoordinates, w[nrow(w), c("lat", "lon")])  # net distance
  d = distance(lastCoordinates, w[1, c("lat", "lon")])
  for (t in 1:(nrow(w) - 1)) {
    d = d + distance(w[t, c("lat", "lon")], w[t + 1, c("lat", "lon")])
  }
  fTotalDistance = d # total distance
  if (fTotalDistance > 0) {
    fRatioDistance = fNetDistance / fTotalDistance
  } else {
    fRatioDistance = 0
  }
  fEleChange = sum(abs(w[, c("elevationDelta")]))  # total elevation change
  fAvgEle = mean(w[, c("ele")])  # average elevation
  fNsat = mean(w[, c("nsatView")])  # average nsatView
  fSNR = mean(w[, c("snrView")])  # average snr
  
  return(c(fAvgSpeed, fSdSpeed, fCoefVar, fNetDistance, fTotalDistance, fRatioDistance, fEleChange, fAvgEle, fNsat, fSNR))
}
distance = function(origin, destination) {
  lat1 = origin$lat
  lon1 = origin$lon
  lat2 = destination$lat
  lon2 = destination$lon
  radius = 6371 * 1000 # m
  
  dlat = (lat2 - lat1) * pi / 180
  dlon = (lon2 - lon1) * pi / 180
  a = sin(dlat / 2) * sin(dlat / 2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2) * sin(dlon / 2)
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d = radius * c
  return(d)
}
computeOneAccFeat = function(w, Fs) {
  # axis 1: vertical (z)
  # axis 2: horizontal (y)
  # axis 3: perpindicular (x)
  
  # gravity component
  g = w[1, ]
  for (n in 1:nrow(w)) {
    g = 0.9 * g + 0.1 * w[n, ]
  }
  
  # v = vector magnitude
  v = sqrt(rowSums(w ^ 2))
  fMean = mean(v)
  fStd = sd(v)
  if (fMean > 0) {
    fCoefVariation = fStd / fMean
  } else {
    fCoefVariation = 0
  }
  fMedian = median(v)
  fMin = min(v)
  fMax = max(v)
  f25thP = quantile(v, 0.25)[[1]]
  f75thP = quantile(v, 0.75)[[1]]
  
  a = acf(v, plot=FALSE)
  fAutocorr = which.max(abs(a$acf[2:length(a$acf)])) / (nrow(w) / Fs)
  if ((sd(w[, 3]) > 0) & (sd(w[, 2]) > 0)) {
    fCorrxy = cor(w[, 3], w[, 2])
  } else {
    fCorrxy = 0
  }
  if ((sd(w[, 3]) > 0) & (sd(w[, 1]) > 0)) {
    fCorrxz = cor(w[, 3], w[, 1])
  } else {
    fCorrxz = 0
  }
  if ((sd(w[, 2]) > 0) & (sd(w[, 1]) > 0)) {
    fCorryz = cor(w[, 2], w[, 1])
  } else {
    fCorryz = 0
  }
  
  if (is.na(fCorrxy)) fCorrxy = 0
  if (is.na(fCorrxz)) fCorrxz = 0
  if (is.na(fCorryz)) fCorryz = 0
  
  fAvgRoll = mean(atan2(w[, 2],w[, 1]))
  fAvgPitch = mean(atan2(w[, 1],w[, 3]))
  fAvgYaw = mean(atan2(w[, 2],w[, 3]))
  
  fSdRoll = sd(atan2(w[, 2],w[, 1]))
  fSdPitch = sd(atan2(w[, 1],w[, 3]))
  fSdYaw = sd(atan2(w[, 2],w[, 3]))
  
  fRollG = atan2(g[2], g[1])
  fPitchG = atan2(g[1], g[3])
  fYawG = atan2(g[2], g[3])
  
  s = specgram(v, n=length(v), Fs=Fs)
  S = abs(s$S)
  f = S / max(S)
  freq = s$f
  f1 = f[freq >= 0.1]
  freq1 = freq[freq >= 0.1]
  fFmax = freq1[which.max(f1)]
  fPmax = max(f1)
  
  band = f[freq > 0.3 & freq < 3]
  fPmaxBand = max(band)
  freqband = freq[freq > 0.3 & freq < 3]
  fFmaxBand = freqband[which.max(band)]
  fEntropy = - sum(f * log(f))
  
  s = specgram(v, n=Fs, Fs=Fs)
  S = abs(s$S)
  f = S / max(S)
  freq = s$f
  f = rowSums(f) / ncol(f)
  FFT0 = f[1]
  FFT1 = f[2]
  FFT2 = f[3]
  FFT3 = f[4]
  FFT4 = f[5]
  FFT5 = f[6]
  FFT6 = f[7]
  FFT7 = f[8]
  FFT8 = f[9]
  FFT9 = f[10]
  FFT10 = f[11]
  FFT11 = f[12]
  FFT12 = f[13]
  FFT13 = f[14]
  FFT14 = f[15]
  
  return(c(fMean, fStd, fCoefVariation, fMedian, fMin, fMax, f25thP, f75thP, fAutocorr, fCorrxy, fCorrxz, fCorryz, fAvgRoll, fAvgPitch, fAvgYaw, fSdRoll, fSdPitch, fSdYaw, fRollG, fPitchG, fYawG, fFmax, fPmax, fFmaxBand, fPmaxBand, fEntropy, FFT0, FFT1, FFT2, FFT3, FFT4, FFT5, FFT6, FFT7, FFT8, FFT9, FFT10, FFT11, FFT12, FFT13, FFT14))
}
extractAccFeats = function(inputFile, outputFile, winSize) {
  con = file(inputFile, open = "r")
  line = readLines(con, n = 1)
  Fs = as.numeric(str_match(line, "(\\d+) Hz")[1, 2])
  dateFmt = str_match(line, "date format ([a-z,A-Z,/]*)")[1, 2]
  dateFmt = gsub("yyyy", "%Y", dateFmt)
  dateFmt = gsub("M", "%m", dateFmt)
  dateFmt = gsub("d", "%d", dateFmt)
  line = readLines(con, n = 1)
  line = readLines(con, n = 1)
  StartTime = gsub("Start Time ", "", line)
  line = readLines(con, n = 1)
  StartDate = gsub("Start Date ", "", line)
  line = readLines(con, n = 6)
  st = strptime(paste(StartDate, StartTime), paste(dateFmt, "%H:%M:%S"))
  day = st$mday
  out = file.path(outputFile,strftime(st, "%Y-%m-%d"))
  cat(strftime(st, "%Y-%m-%d"), '\n')
  if (!file.exists(outputFile)) {
    dir.create(outputFile, recursive=TRUE)
  }
  cat("timestamp,mean,sd,coefvariation,median,min,max,25thp,75thp,autocorr,corrxy,corrxz,corryz,avgroll,avgpitch,avgyaw,sdroll,sdpitch,sdyaw,rollg,pitchg,yawg,fmax,pmax,fmaxband,pmaxband,entropy,fft0,fft1,fft2,fft3,fft4,fft5,fft6,fft7,fft8,fft9,fft10,fft11,fft12,fft13,fft14\n", file=out, append=TRUE)
  
  while (length(line <- readLines(con, n = Fs * winSize)) >= Fs * winSize) {
    line = gsub("\"", "", line)
    M = as.matrix(strsplit(line, " "))
    M = sapply(M, strsplit, ",")
    M = sapply(M, as.numeric)
    M = t(M)
    feat = computeOneAccFeat(M, Fs)
    cat(strftime(st, "%Y-%m-%d %H:%M:%S,"), file=out, sep = "", append=TRUE)
    cat(feat, file=out, sep=",", append=TRUE)
    cat('\n', file=out, append=TRUE)
    
    st = as.POSIXlt(st + winSize)
    if (st$mday != day) {
      out = file.path(outputFile,strftime(st, "%Y-%m-%d"))
      cat(strftime(st, "%Y-%m-%d"), '\n')
      cat("timestamp,mean,sd,coefvariation,median,min,max,25thp,75thp,autocorr,corrxy,corrxz,corryz,avgroll,avgpitch,avgyaw,sdroll,sdpitch,sdyaw,rollg,pitchg,yawg,fmax,pmax,fmaxband,pmaxband,entropy,fft0,fft1,fft2,fft3,fft4,fft5,fft6,fft7,fft8,fft9,fft10,fft11,fft12,fft13,fft14\n", file=out, append=TRUE)
      day = st$mday
    }
  }
  close(con)
}
extractAllAccDir = function(inputDir, outputDir, winSize, names = NULL) {
  if (is.null(names)) {
    names = list.files(inputDir)
  }
  if (length(names) == 0) {
    stop("couldn't find any accelerometer files\n")
  }
  for (i in 1:length(names)) {
    cat(names[i], "...\n")
    outputFile = file.path(outputDir, file_path_sans_ext(names[i]))
    extractAccFeats(file.path(inputDir, names[i]), outputFile, winSize)
  }
}

# ??
alignStart = function(winSize, start) {
  d0 = trunc(start, "days")
  s = as.numeric(difftime(start, d0, units="secs"))
  w = ceiling(s / winSize)
  newStart = as.POSIXlt(d0 + w * 60)
  return(newStart)
}

senseCamLabels = function(inputFile, outputFile){
  bouts = read.csv(inputFile, header=TRUE, stringsAsFactors=FALSE)
  bouts$behavior = "NULL"
  #bouts$identifier = bouts$PtID
  bouts$PtID = NULL
  
  for (i in 1:nrow(bouts)){
    labels = c(bouts[i, ]$PA1, bouts[i, ]$PA2, bouts[i, ]$PA3, bouts[i, ]$PA4)
    labels[is.na(labels)] = " "
    
    if ("02A. Sedentary" %in% labels){
      if (!("03H. Car" %in% labels)&!("03I. Other Vehicle" %in% labels)&!("03D. Sports" %in% labels)){
        bouts[i, ]$behavior = "Sedentary"
      }
    }
    if (("02B. Standing Still" %in% labels)&!("03D. Sports" %in% labels)){
      bouts[i, ]$behavior = "StandingStill"
    }
    if (("02C. Standing Moving" %in% labels)&!("03D. Sports" %in% labels)){
      bouts[i, ]$behavior = "StandingMoving"
    }
    if (("02D. Walking/Running" %in% labels)&!("03D. Sports" %in% labels)){
      bouts[i, ]$behavior = "Walking"
    }
    if (("02E. Biking" %in% labels)){
      bouts[i, ]$behavior = "Biking"
    }
    if (("03H. Car" %in% labels)|("03I. Other Vehicle" %in% labels)){
      bouts[i, ]$behavior = "Vehicle"
    }
  }
  bouts$PA1 = NULL
  bouts$PA2 = NULL
  bouts$PA3 = NULL
  bouts$PA4 = NULL
  bouts = bouts[c("identifier","StartDateTime","EndDateTime","behavior")]
  write.csv(bouts, file=outputFile, quote=FALSE, row.names=FALSE)
}
senseCamLabelsDir = function(inputDir, outputDir){
  files = list.files(inputDir)
  if (!file.exists(outputDir)) {
    dir.create(outputDir, recursive=TRUE)
  }
  for (i in 1:length(files)){
    cat(files[i], "\n")
    senseCamLabels(file.path(inputDir, files[i]), file.path(outputDir, files[i]))
  }
}