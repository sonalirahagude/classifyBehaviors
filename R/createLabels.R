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
    print(files[i])
    print(dateFmt)
    print(bouts[r, ]$StartDateTime)
    if(nrow(bouts) < 2)
      next
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
