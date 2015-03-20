\name{trainModel}
\alias{trainModel}
\title{
Train a behavior classification model
}
\description{
This function extracts features from sensor data and learns a behavior classification model.
}
\usage{
trainModel(annotations, accelerometers = NULL, GPS = NULL, winSize = 60, modelName = "~/model", names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to annotation file or directory containing annotation files. Annotations should be in .csv format with the following fields: \emph{identifier, StartDateTime, EndDateTime, behavior}. \emph{identifier} is an identifier for the participant performing the behavior. \emph{StartDateTime} and \emph{EndDateTime} are the start and ending times of the behavior, either formatted as mm/dd/yyyy HH:MM:SS or yyyy-mm-dd HH:MM:SS. \emph{behavior} is a string naming the behavior. If \emph{annotations} is a path to a directory, each file in the directory should be a .csv file with the required format, and the name of each file should be the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{accelerometers}{
Path to a directory (or a vector of paths to multiple directories, corresponding to different accelerometer placement) containing accelerometer data files. Accelerometer data files should be .csv files output in "raw" format by ActiLife (without timestamps), and named by the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{GPS}{
Path to a GPS data file (or a directory containing GPS data files). GPS data files should be in .csv format with the following fields: \emph{identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView}. \emph{identifier} should be the participant identifier, \emph{e.g.} 001_01. The PALMS system will output the required fields. If \emph{GPS} is a path to a directory, each file in the directory should correspond to a participant, and the file name should be the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{winSize}{
The size of the data window, in seconds. 
}
  \item{modelName}{
Path to file to save model (\emph{e.g.}, "~/model.RData").
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
\code{\link{classify}}, \code{\link{calcPerformance}}
}
\examples{
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
