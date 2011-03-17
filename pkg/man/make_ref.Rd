\name{make_ref}
\alias{make_ref}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_ref(ref_x, ref_y, ref_yr, ref_scen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ref_x}{
%%     ~~Describe \code{ref_x} here~~
}
  \item{ref_y}{
%%     ~~Describe \code{ref_y} here~~
}
  \item{ref_yr}{
%%     ~~Describe \code{ref_yr} here~~
}
  \item{ref_scen}{
%%     ~~Describe \code{ref_scen} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(ref_x, ref_y, ref_yr, ref_scen) {

# FIND GRIDCELL

x1 <- (floor(ref_x*2)/2)+0.25
y1 <- (floor(ref_y*2)/2)+0.25
x2 <- Xvec==x1
y2 <- Yvec==y1
index <- (x2+y2)==2

if(sum(index) != 1) stop("NOT A TERRESTRIAL GRID CELL, TRY AGAIN...")

# MAKE REFERENCE GRIDCELL

if (ref_yr=="present") {
ref_tmp <- tmp6190[index,]
ref_pre <- pre6190[index,]
ref_dtr <- dtr6190[index,] }

else {
scen <- colnames(scen_gT)==ref_scen
deltaT <- scen_gT[(ref_yr-2000),scen]

ref_tmp <- tmp6190[index,] + (deltaT * tmp_p[index,])
ref_pre <- pre6190[index,] + (deltaT * pre_p[index,])
ref_dtr <- dtr6190[index,] + (deltaT * dtr_p[index,]) }

x_dir <- ifelse(ref_x >= 0, "E", "W")
y_dir <- ifelse(ref_y >= 0, "N", "S")

if (ref_yr=="present") {
ref_label <- paste(abs(round(ref_x, 2)), x_dir, " ", abs(round(ref_y, 2)), y_dir, " HadCRU TS3 1961-1990 monthly means", sep="") }

else {
ref_label <- paste(abs(round(ref_x, 2)), x_dir, " ", abs(round(ref_y, 2)), y_dir, " in ", ref_yr, " (HadCM3 ", ref_scen, " scenario)", sep="") }

return(list(tmp=ref_tmp, pre=ref_pre, dtr=ref_dtr, ref_label=ref_label, ref_x=ref_x, ref_y=ref_y)) }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
