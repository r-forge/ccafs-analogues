analogue <-
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

