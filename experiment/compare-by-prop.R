# read file name and data from .csv file
args  <- commandArgs(trailingOnly = TRUE)
dataPath1 <- args[1]
dataPath2 <- args[2]
outPath <- args[3]
print(dataPath1)
print(dataPath2)
data1 <- read.csv(file=dataPath1, sep=",")
data2 <- read.csv(file=dataPath2, sep=",")

# plot to PDF file
pdf(outPath,width=7,height=4)

# stats about arcs for data1
xs1 <- data1$mwe.deriv.size / data1$reg.deriv.size
ys1 <- (data1$chart.nodes.1 + data1$agenda.nodes.1) / data1$chart.nodes.2
local1 <- data.frame(propo=xs1, size=ys1)
arcs1 <- aggregate(local1$size, list(propo=local1$propo), mean)

# stats about arcs
xs2 <- data2$mwe.deriv.size / data2$reg.deriv.size
ys2 <- (data2$chart.arcs.1 + data2$agenda.arcs.1) / data2$chart.arcs.2
local2 <- data.frame(propo=xs2, size=ys2)
arcs2 <- aggregate(local2$size, list(propo=local2$propo), mean)

ymin <- min(arcs1$x, arcs2$x)
ymax <- max(arcs1$x, arcs2$x)

# plot info about data1
plot(arcs1$propo, arcs1$x, type="b", col='red'
	, pch=4
	, xlab='|MWE derivation| / |Regular derivation|'
	, ylim=c(ymin,ymax)
	, ylab='% of the arcs explored' )

# plot info about data2
lines(arcs2$propo, arcs2$x, type="b", col='orange', pch=1)

# add the legend
legend('bottom',
       legend=c(dataPath1, dataPath2),
       col=c('red','orange'),
       pch=c(4, 1),
       cex=1.0, pt.cex = 1, bty='n', lty=1)
