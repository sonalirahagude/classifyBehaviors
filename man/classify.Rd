\name{classify}
\alias{classify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classify sensor data with behavior labels
}
\description{
This function takes sensor data (accelerometer and/or GPS) and outputs behavior labels. The function (1) extracts features, (2) applies a classification model and (3) outputs predicted behavior labels.
}
\usage{
classify(accelerometers=NULL, GPS=NULL, modelName, saveDir, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{accelerometers}{
Path to a directory (or a vector of paths to multiple directories, corresponding to different accelerometer placement) containing accelerometer data files. Accelerometer data files should be .csv files output in "raw" format by ActiLife (without timestamps), and named by the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{GPS}{
Path to a GPS data file (or a directory containing GPS data files). GPS data files should be in .csv format with the following fields: \emph{identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView}. \emph{identifier} should be the participant identifier, \emph{e.g.} 001_01. The PALMS system will output the required fields. If \emph{GPS} is a path to a directory, each file in the directory should correspond to a participant, and the file name should be the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{modelName}{
Name of model to use to classify behaviors. Precomputed models included in this package include:

- \emph{BikeTrainAnkle}: Trained from bike train ankle accelerometer

- \emph{BikeTrainWaist}: Trained from bike train waist accelerometer

- \emph{BikeTrainAnkleGPS}: Trained from bike train ankle accelerometer and GPS

- \emph{BikeTrainAnkleWaist}: Trained from bike train ankle accelerometer and waist accelerometer

- \emph{BikeTrainWaistGPS}: Model trained from bike train waist accelerometer and GPS

- \emph{BikeTrainAnkleWaistGPS}: Model trained from bike train ankle accelerometer, waist accelerometer and GPS

Alternatively, provide the path to a model you trained using the \emph{trainModel} function.
}
  \item{saveDir}{
Path to a directory where predictions will be saved. Predictions will be saved in .csv format with two fields: \emph{timestamp,prediction}.
}
  \item{names}{
(Optional) Vector of participant identifiers to use. If not specified, all participants found in the data directories will be used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Katherine Ellis
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{trainModel}}, \code{\link{calcPerformance}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
