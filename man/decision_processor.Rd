% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decision_processor.R
\name{decision_processor}
\alias{decision_processor}
\title{SCOTUS Decision Text Processor}
\usage{
decision_processor(dir_path)
}
\arguments{
\item{dir_path}{A directory path to a folder on your local machine containing decisions by the United States Supreme Court. *Note*: Files must be in PDF format.}
}
\value{
Parsed and cleaned text of decisions, separated by main text, footnotes, and opinion type, as well as opinion author and joinining justices (if any).
}
\description{
SCOTUS Decision Text Processor
}
\examples{
\dontrun{
#Replace "<Folder Directory>" with appropriate location.
sample_decision <- decision_processor(dir_path = "<Folder Directory>")
}
}
