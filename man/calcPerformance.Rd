\name{calcPerformance}
\alias{calcPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the performance of a classifier
}
\description{
Calculates the performance of a classifier by comparing predicted behaviors to true annotations. Computes confusion matrix, precision, recall, and balanced accuracy. 
}
\usage{
calcPerformance(annotations, predDir, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to annotation file or directory containing annotation files. Annotations should be in .csv format with the following fields: \emph{identifier,StartDateTime,EndDateTime,behavior}. \emph{identifier} is an identifier for the participant performing the behavior. \emph{StartDateTime} and \emph{EndDateTime} are the start and ending times of the behavior, either formatted as mm/dd/yyyy HH:MM:SS or yyyy-mm-dd HH:MM:SS. \emph{behavior} is a string naming the behavior. If \emph{annotations} is a path to a directory, each file in the directory should be a .csv file with the required format, and the name of each file should be the participant identifier, \emph{e.g.}, 001_01.csv.
}
  \item{predDir}{
Path to directory containing files with predicted behaviors. Each file should be a .csv file with the following fields: \emph{timestamp,behavior}. This is the format output by the \emph{classify} function.
}
  \item{names}{
(Optional) Vector of participant identifiers to use. If not specified, all participants found in the prediction directory will be used.
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
\code{\link{trainModel}}, \code{\link{classify}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
