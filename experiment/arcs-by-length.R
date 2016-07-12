# read file name and data from .csv file
args <- commandArgs(trailingOnly = TRUE)
data <- read.csv(file=args[1], sep=",")

# plot to window or to file?
# x11()
# pdf(args[2],width=7,height=4)
pdf(args[2])

# margins?
# par(mar=c(4,4,1,1))

# number of arcs in 1 step
xs <- data$sent.length
ys <- data$chart.arcs.1 + data$agenda.arcs.1
local <- data.frame(length=xs, size=ys)
arcs1 <- aggregate(local$size, list(length=local$length), mean)

# number of arcs in 2 step
ys <- data$chart.arcs.2 + data$agenda.arcs.2
local <- data.frame(length=xs, size=ys)
arcs2 <- aggregate(local$size, list(length=local$length), mean)

ymin <- min(arcs1$x, arcs2$x)
ymax <- max(arcs1$x, arcs2$x)

plot(arcs1$length, arcs1$x, type="b", col='green'
	, pch=4
	# , xlab='(a)\n'
	, xlab='sentence length'
	, ylim=c(ymin,ymax)
	, ylab='# of arcs of the hypergraph' )

lines(arcs2$length, arcs2$x, type="b", col='red', pch=1)


# add the legend
legend('bottom',
       legend=c('Final', 'Checkpoint'),
       col=c('red', 'green'),
       pch=c(4, 1),
       cex=1.0, pt.cex = 1, bty='n', lty=1)

# useful if plotting to window:
# Sys.sleep(10)
# dev.off()
