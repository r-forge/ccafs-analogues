c_plot <-
function(dis_obj, nbin, look_up, col_bar_lim) {

x11(height=6.5, width=11)
oldpar <- par(omi=c(0.5,0,0,0), mai=c(0.5,0.5,0.5,0.5), col.axis="grey40", fg="grey40")

# DEFINE BINS FOR COLOR LOOK-UP
spec_col_bar <- F
if (col_bar_lim[1] != -99) {
spec_col_bar <- T
dat1 <- ifelse(dis_obj$dis < col_bar_lim[1], NA, dis_obj$dis)
dat1 <- ifelse(dis_obj$dis > col_bar_lim[2], NA, dat1) }

else { dat1 <- dis_obj$dis }

if (look_up=="eq_freq") {
dat2 <- rank(dat1, na.last="keep") }

else { dat2 <- dat1 }

dr0 <- range(dis_obj$dis, na.rm=T)
dr2 <- range(dat2, na.rm=T)
diff2 <- dr2[2] - dr2[1]

bins <- seq(dr2[1], dr2[2], diff2/nbin)
nudge <- diff2 * 0.0001
bins[1] <- bins[1] - nudge
bins[nbin+1] <- bins[nbin+1] + nudge

# DEFINE COLORS

pal <- colorRampPalette(c("red", "yellow", "green", "cyan", "blue", "purple"))

col1 <- pal(nbin)
dat3 <- cut(dat2, breaks=bins, labels=F)
col2 <- col1[dat3]

if (spec_col_bar) {
col2 <- ifelse(dis_obj$dis < col_bar_lim[1], "grey80", col2)
col2 <- ifelse(dis_obj$dis > col_bar_lim[2], "grey30", col2) }

# DRAW MAP

image(lon, lat, frame, asp=1, xlab="", ylab="", axes=F, col=col2)
lines(coast, col="grey60")

xat <- seq(-150, 150, 50)
xpos <- sign(xat) + 2
xdir <- c("*W", "", "*E")
xaxlab <- parse(text = paste(abs(xat), "*degree", xdir[xpos])) 

yat <- seq(-60, 60, 30)
ypos <- sign(yat) + 2
ydir <- c("*S", "", "*N")
yaxlab <- parse(text = paste(abs(yat), "*degree", ydir[ypos])) 

axis(1, at=xat, labels=xaxlab, tick=T)
axis(2, at=yat, labels=yaxlab, tick=T)
axis(3, at=xat, labels=xaxlab, tick=T)
axis(4, at=yat, labels=yaxlab, tick=T)

# COLOR_BAR

if (look_up=="eq_freq") {
fv <- unlist(tapply(dis_obj$dis, dat3, max))
f1 <- max(dr0[1], col_bar_lim[1], na.rm=T)
bins2 <- c(f1, fv) }

else {bins2 <- bins }

if (look_up=="eq_int_hist") {
h <- hist(dat2, breaks=bins, plot=F)
hmx <- max(h$counts)
col_bar_y2 <- ((h$counts  / hmx) * 15) - 90 }

else {col_bar_y2 <- rep(-75, nbin) }

r_bins2 <- range(bins2)
d_bins2 <- (r_bins2[2] - r_bins2[1])/36

if (spec_col_bar) {
if (dr0[1] < col_bar_lim[1]) {
bins2 <- c(min(bins2) - d_bins2, bins2)
col1 <- c("grey80", col1)
col_bar_y2 <- c(-75, col_bar_y2) }
if (dr0[2] > col_bar_lim[2]) {
bins2 <- c(bins2, max(bins2) + d_bins2)
col1 <- c(col1, "grey30")
col_bar_y2 <- c(col_bar_y2, -75) } }

r2_bins2 <- range(bins2)
n_bins2 <- length(bins2)

col_bar_x <- (((bins2 - r2_bins2[1]) / (r2_bins2[2] - r2_bins2[1])) * 360) - 180
col_bar_x1 <- col_bar_x[-n_bins2]
col_bar_x2 <- col_bar_x[-1]

col_bar_y1 <- rep(-90, n_bins2)

rect(col_bar_x1, col_bar_y1, col_bar_x2, col_bar_y2, col=col1, border=NA)
rect(-180, -90, 180, -75, col=NA)

values <- pretty(dat1, 10)
col_bar_at <- (((values - r2_bins2[1]) / (r2_bins2[2] - r2_bins2[1])) * 360) - 180
text(col_bar_at, -75, values, adj=c(0.5, -0.5), font=4, cex=0.8)

mtext(paste("CLIMATIC DISSIMILARITY BETWEEN", dis_obj$ref_label, "AND HadCRU TS3 (MONTHLY MEANS) FOR ALL GRIDCELLS"), side =1, adj=0.5, outer=T, font=1, line=0.6, cex=1)

points(dis_obj$ref_x, dis_obj$ref_y, pch=3, cex=2.2, col="grey60")
points(dis_obj$ref_x, dis_obj$ref_y, pch=1, cex=1.8, col="grey60")

box()
par(oldpar)

return(col2=col2) }

