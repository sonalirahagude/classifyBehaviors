loadData = function(labelDir, featDirs, names=NULL) {
  print (labelDir)
  print (featDirs)
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  print(names)
  if (length(names) == 0) {
    return(NULL)
  }
  all_data = data.frame()
  all_labels = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    labelFiles = list.files(file.path(labelDir, names[i]))
    print(labelFiles)
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
          print("skipping the following")
          print(featFile)
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
          print('*********************')
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
