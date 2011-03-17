make_ref <-
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

