### find the reason why they change the sampling design? ###
plotcode <-unique(survey$PLOT_ID_CO)
plot.code.df <- data.frame(post = substr(plotcode, start = 1, stop = 6),
           hab = substr(plotcode, start = 8, stop = 9),
           subplot = substr(plotcode, start = 11, stop = 12))

# number of subplot per plot per habitat type
no.of.sub.per.plot.hab <- aggregate(subplot ~ post + hab, data = plot.code.df, FUN =  length)
head(no.of.sub.per.plot.hab)
table(no.of.sub.per.plot.hab$subplot)
#1    2    4    5    7    8    9   10   14   15   20 
#1    1    7 3130    1    6   11 1232    1   37   17 

# the size pattern of plot in the same habitat type
subplot <- read_xlsx("P:\\Personal\\Yimei\\invasive species data\\original dataset from prof Hsieh\\IM_SPLOTDATA.xlsx")

subplot.size.hab <- aggregate(SUB_TYPE ~ HAB_TYPE, data = subplot, FUN = table)
head(subplot.size.hab)
subplot.size.hab$SUB_TYPE[[6]]
subplot.size.hab$SUB_TYPE <- as.list(subplot.size.hab$SUB_TYPE)
subplot.size.hab.mat <- matrix(0, ncol = 19, nrow = 4) 
for (i in 1:19) {
  vec <- c(0,0,0,0)
  names(vec) <- 1:4
  TF <- names(vec) %in% names(subplot.size.hab$SUB_TYPE[[i]])
  vec[TF] <- subplot.size.hab$SUB_TYPE[[i]]
  subplot.size.hab.mat[,i] <- vec
}
rownames(subplot.size.hab.mat) <- 1:4
colnames(subplot.size.hab.mat) <- 1:19
setwd("z:\\NTU\\master research\\figures\\plot size pattern_200909")
png(filename = "plot size pattern-habitat type.png", width = 700, height = 700)
x <- barplot(subplot.size.hab.mat, beside = T, col = 1:4, ylim = c(0,5000), legend.text = T, args.legend = list(legend = c("1*1 m", "5*5 m", "2*5 m", "2*5 m")), cex.names = 0.8, main = "the plot size pattern in the view of habitat type")

text(x, subplot.size.hab.mat+50, labels = as.character(subplot.size.hab.mat), cex = 0.6)
dev.off()
  # 8,9 is forest

# size pattern - year
subplot$date <- substr(subplot$SUBPDATE,start = 1, stop = 4)
plot.size.date <- aggregate(SUB_TYPE ~ date, data = subplot, FUN = table)
plot.size.date.mat <- matrix(0, ncol = 7, nrow = 4)
for (i in 1:7) {
  vec <- c(0,0,0,0)
  names(vec) <- 1:4
  TF <- names(vec) %in% names(plot.size.date$SUB_TYPE[[i]])
  vec[TF] <- plot.size.date$SUB_TYPE[[i]]
  plot.size.date.mat[,i] <- vec
}
colnames(plot.size.date.mat) <- plot.size.date$date
rownames(plot.size.date.mat) <- 1:4
plot.size.date.mat
plot.size.date.mat[,1] <- plot.size.date.mat[,1] + plot.size.date.mat[,6] + plot.size.date.mat[,7]
plot.size.date.mat <- plot.size.date.mat[,1:4]
plot.size.date.mat
png(filename = "plot size pattern-year.png", width = 700, height = 700)
x <- barplot(plot.size.date.mat, beside = T, ylim = c(0,8000), col = 1:4, legend.text = T, args.legend = list(legend = c("1*1 m", "5*5 m", "2*5 m", "2*5 m")), main = "the plot size in different year")
text(x, plot.size.date.mat+150, labels = as.character(plot.size.date.mat))
dev.off()
table(subplot$HAB_TYPE[subplot$date == "2009"])
01   03   04   05   06   07   08   09   10   11   12   13   14   15   16   17 
2273 1395  495  825  665  305  569  580   70   30 1001  100  385  820  354  355 

# size pattern - date
subplot$SUBPDATE <- as.numeric(subplot$SUBPDATE)
range(subplot$SUBPDATE[subplot$date == "2009" & subplot$SUB_TYPE == "1"])
# [1] 20090302 20091104 old sampling method
range(subplot$SUBPDATE[subplot$SUB_TYPE %in% c("3", "4") & subplot$date == "2009"])
# [1] 20090315 20091106 new sampling method