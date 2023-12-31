% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metablock.R
\name{metablock}
\alias{metablock}
\title{Find all general blocks}
\usage{
metablock(blocks)
}
\arguments{
\item{blocks}{A matrix defining the blocks of the model. The number of rows
must be the number of blocks, each row represents a block and contains the
item numbers. The number of columns present the number of items per block.}
}
\value{
Returns a list of items that form meta interlinked blocks.
}
\description{
This function creates meta interlinked blocks within a Thurstonian
design.
}
\details{
This function creates meta interlinked blocks of a block design.
These are blocks, that have at least one link from each of its items to
any other of its items. If there is not such a link between every item,
there are at least two meta blocks.
}
\examples{

# Define 30 items divided by ten triplets as blocks
blocks <- matrix(c(1:30), ncol = 3)

# Add one block to link the first three blocks.
blocks <- rbind(blocks,c(1,2,3))

# Find mata blocks
metablock(blocks)
  
}
