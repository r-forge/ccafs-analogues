dis_lag <-
function(ref_obj, dis_method) {

ref_tmp <- as.vector(as.matrix(ref_obj$tmp))
ref_pre <- as.vector(as.matrix(ref_obj$pre))
ref_dtr <- as.vector(as.matrix(ref_obj$dtr))

roll <- matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12,
2,3,4,5,6,7,8,9,10,11,12,1,
3,4,5,6,7,8,9,10,11,12,1,2,
4,5,6,7,8,9,10,11,12,1,2,3,
5,6,7,8,9,10,11,12,1,2,3,4,
6,7,8,9,10,11,12,1,2,3,4,5,
7,8,9,10,11,12,1,2,3,4,5,6,
8,9,10,11,12,1,2,3,4,5,6,7,
9,10,11,12,1,2,3,4,5,6,7,8,
10,11,12,1,2,3,4,5,6,7,8,9,
11,12,1,2,3,4,5,6,7,8,9,10,
12,1,2,3,4,5,6,7,8,9,10,11), ncol=12, byrow=T)

ccafs <- function(lag) {
t1 <- t(tmp6190)-ref_tmp[lag]
p1 <- t(pre6190)-ref_pre[lag]
d1 <- t(dtr6190)/ref_dtr[lag]

t2 <- d1*t1*t1
p2 <- p1*p1

v1 <- colSums(t2)
v2 <- colSums(p2)

res <- sqrt(v1+v2)
return(res) }


hallegatte <- function(lag) {
p1 <- sum(ref_pre[lag])
p2 <- rowSums(pre6190)
p3 <- abs(p1-p2)/p1

p4 <- abs(scale(pre6190, center=ref_pre[lag], scale=ifelse(ref_pre[lag]==0, 1, ref_pre[lag])))
p5 <- rowSums(p4)/12

t1 <- abs(scale(tmp6190, center=ref_tmp[lag], scale=F))
t2 <- rowSums(t1)/12

pre_limit <- (p3 <= 0.15) + (p5 <= 0.3) == 2

res <- ifelse(pre_limit==T, t2, NA)
return(res) }

if (dis_method=="ccafs") {
res_all <- apply(roll, 1, ccafs) }

if (dis_method=="hallegatte") {
res_all <- apply(roll, 1, hallegatte) }

res <- apply(res_all, 1, function(x) {
if (sum(is.na(x))==12) {
return(c(NA, NA)) }
else {
r1 <- which.min(x)
r2 <- x[r1]
return(c(r1, r2)) } } )

return(list(lag=res[1,], dis=res[2,], dis_nolag=res_all[,1], ref_label=ref_obj$ref_label, ref_x=ref_obj$ref_x, ref_y=ref_obj$ref_y)) }

