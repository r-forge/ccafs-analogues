\name{lag_plot}
\alias{lag_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lag_plot(c_plot_obj, dis_obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c_plot_obj}{
%%     ~~Describe \code{c_plot_obj} here~~
}
  \item{dis_obj}{
%%     ~~Describe \code{dis_obj} here~~
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
function(c_plot_obj, dis_obj) {

x11(height=7.2, width=9.4)

oldpar <- par(omi=c(0.4,0.1,0.4,0), mai=c(0.1,0,0,0.1), mfrow=c(4,3), fg="grey40")

for (i in 1:12) {
col_lag <- ifelse(dis_obj$lag==i, c_plot_obj, NA)

image(lon, lat, frame, asp=1, xlab="", ylab="", axes=F, col=col_lag)

lines(coast, col="grey60")
text(-170, -80, paste(i-1, ifelse(i==2,"MONTH","MONTHS")), font=1, adj=c(0,0))
box() }

mtext("CLIMATIC DISSIMILARITY MINIMISED AT LAG TIME:", side=3, adj=0, outer=T, font=2, line=0.5)

mtext(paste(dis_obj$ref_label, "vs. all gridcells HadCRU TS3 1961-1990 monthly means"), side=1, adj=0, outer=T, font=1, line=0.7)

par(oldpar) }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
