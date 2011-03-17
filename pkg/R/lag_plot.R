lag_plot <-
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

