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
