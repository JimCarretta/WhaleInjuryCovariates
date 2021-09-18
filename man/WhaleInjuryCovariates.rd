% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Large Whale Injury Covariates.R
\name{WhaleInjuryCovariates}
\alias{WhaleInjuryCovariates}
\title{WhaleInjuryCovariates()}
\usage{
WhaleInjuryCovariates(x)
}
\arguments{
\item{x}{an object of class 'data.frame'}
}
\description{
Identify Covariates for Whale Injury Assessments from
Narratives + Append Covariates to existing data.frame.

Covariates are identified from words/phrases in field 'Narrative'.
Presence / absence of words / phrases is used to code covariates
as binary presence (1) or absence (0), except for vessel vessels
coded as factors (Vessel Size and Speed: small, large, slow, fast, unknown)

Resulting data.frame is used with randomForest models to predict
health status of whales involved in entanglements or vessel strikes.

Package includes data.frame 'whales'
}
\examples{
names(whales)

new.data.frame <- WhaleInjuryCovariates(whales)

names(new.data.frame)

}
\author{
Jim Carretta \href{mailto:jim.carretta@noaa.gov}{jim.carretta@noaa.gov}
}
