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
    #print (accelerometers)
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
