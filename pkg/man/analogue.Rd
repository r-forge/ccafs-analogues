\name{analogue}
\alias{analogue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
analogue(x, y, method = "ccafs", scenario = "A2", year = 2030, col_scheme = "eq_freq", n_map_col = 32, col_lim = c(-99, -99), lag_panel = F, export = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{scenario}{
%%     ~~Describe \code{scenario} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{col_scheme}{
%%     ~~Describe \code{col_scheme} here~~
}
  \item{n_map_col}{
%%     ~~Describe \code{n_map_col} here~~
}
  \item{col_lim}{
%%     ~~Describe \code{col_lim} here~~
}
  \item{lag_panel}{
%%     ~~Describe \code{lag_panel} here~~
}
  \item{export}{
%%     ~~Describe \code{export} here~~
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
function(x, y, method="ccafs", scenario="A2", year=2030, col_scheme="eq_freq", n_map_col=32, col_lim=c(-99,-99), lag_panel=F, export=F) {

ref_out <- make_ref(x, y, year, scenario)

dis_out <- dis_lag(ref_out, method)

c_plot_out <- c_plot(dis_out, nbin=n_map_col, look_up=col_scheme, col_bar_lim=col_lim)

if (lag_panel==T) {
lag_plot(c_plot_out, dis_out) }

if (export==T) {
temp <- cbind(Xvec, Yvec, diss_data$diss)
colnames(temp) <- c("X", "Y", "dissimilarity")
f_name <- paste("ccafs_analogues.txt")
write.table(temp, f_name, sep="\t", header=T) } }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
