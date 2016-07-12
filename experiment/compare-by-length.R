# read file name and data from .csv file
args  <- commandArgs(trailingOnly = TRUE)
dataPath1 <- args[1]
dataPath2 <- args[2]
outPath <- args[3]
data1 <- read.csv(file=dataPath1, sep=",")
data2 <- read.csv(file=dataPath2, sep=",")

# plot to PDF file
# pdf(outPath,width=7,height=4)
pdf(outPath)

getArcs = function(data) {
  xs <- data$sent.length
  ys <- (data$chart.arcs.1 + data$agenda.arcs.1) / data$chart.arcs.2
  local1 <- data.frame(propo=xs, size=ys)
  aggregate(local1$size, list(propo=local1$propo), mean)
}

# stats about arcs
arcs1 <- getArcs(data1)
arcs2 <- getArcs(data2)

ymin <- min(arcs1$x, arcs2$x)
ymax <- max(arcs1$x, arcs2$x)

# plot info about data1
plot(arcs1$propo, arcs1$x, type="b", col='red'
	, pch=4
	, xlab='sentence length'
	# , ylim=c(ymin,ymax)
	, ylim=c(0.3,ymax)
	, ylab='% of the arcs explored' )

# plot info about data2
lines(arcs2$propo, arcs2$x, type="b", col='orange', pch=1)

# add the legend
legend('bottom',
       legend=c(dataPath1, dataPath2),
       col=c('red','orange'),
       pch=c(4, 1),
       cex=1.0, pt.cex = 1, bty='n', lty=1)
