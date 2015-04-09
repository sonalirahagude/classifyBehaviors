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
