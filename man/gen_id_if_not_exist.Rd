% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-dm-3-integration-gen-id.r
\name{gen_id_if_not_exist}
\alias{gen_id_if_not_exist}
\title{Generate ID if not exist}
\usage{
gen_id_if_not_exist(df, id_column = "id")
}
\arguments{
\item{df}{data frame}

\item{id_column}{ID column name}
}
\value{
data frame
}
\description{
Generate ID if not exist
}
\examples{
df <- data.frame(
  a = c(1, 2, 3),
  b = c(4, 5, 6)
)
df <- gen_id_if_not_exist(df)
df <- gen_id_if_not_exist(df, 'id')
}
\author{
Chunzhe ZHANG
}
