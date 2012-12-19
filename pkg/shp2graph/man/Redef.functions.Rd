\name{Redef.functions}
\title{A collection of functions for redefining attribute of the new edge}
\description{
The function is used in optimization functions \link{PN.amalgamation} and \link{ME.simplification}.
}
\usage{
Redef.functions(v, typ=1)
}
\arguments{
 \item{v}{An input vector for the specified function;}
 \item{typ}{An integral key to specify the function: 1->sum(v), 2->min(v),3->max(v),4->mean(v)}
}
\note{
It is easy to extend this collection with some other functions depended on the applications.
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
