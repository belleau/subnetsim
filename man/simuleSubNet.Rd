% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subnetsimInternalMethods.R
\name{simuleSubNet}
\alias{simuleSubNet}
\title{Generating simulated subnetworks.}
\usage{
simuleSubNet(netAll, nodesAll, nbIter, nbNodes, seedV)
}
\arguments{
\item{netAll}{a \code{hash} representing the global network.}

\item{nodesAll}{\code{list} of \code{character} string representing
all the nodes in the global network.}

\item{nbIter}{a single \code{integer} indicating the number of iterations
to be run.}

\item{nbNodes}{a \code{integer} indicating the number of nodes that each
simulated sub-network will contain.}

\item{seedV}{a \code{integer}, a seed used when reproducible results are
needed. When a value inferior or equal to zero is given, a random integer
is used.}
}
\value{
a \code{data.frame} containing the information from all simulated
subnetworks. Each row of the \code{data.frame} corresponds to one simulation:
\itemize{
\item nbLink: Number of links in the simulated subnetworks
\item nbNodesOneLink: Number of nodes of the simulated subnetworks
 including the first-degree neighbor nodes.
\item nbLinkOneLink Number of links of the simulated subnetworks
 including all the links from the first-degree neighbor nodes.
}
}
\description{
Generationg multiple simulated sub-networks from a network.
All subnetworks have the same number of nodes.
}
\examples{

# Access demo files stored in the subnetsim package
networkFile <- system.file("extdata", "demo_network.sif", package="subnetsim")
subnetworkFile <- system.file("extdata", "demo_subnetwork.sif", package="subnetsim")

# Load a global network and its tested subnetwork using SIF format files
demo_network_with_sub <- network(netFileName = networkFile,
    subNetFileName = subnetworkFile)

# Run simulation
simulatedSubnetwork <- subnetsim:::simuleSubNet(netAll = demo_network_with_sub$netAll,
    nodesAll = demo_network_with_sub$nodesAll,
    nbIter = 2, nbNodes <- length(demo_network_with_sub$nodesSubNet),
    seedV = 3122)

# To see a summary of the content of the "subnetwork" object
simulatedSubnetwork

}
\author{
Pascal Belleau, Astrid Deschenes
}
\keyword{internal}
