# read file name and data from .csv file
args <- commandArgs(trailingOnly = TRUE)
data <- read.csv(file=args[1], sep=",")

# plot to window or to file?
# x11()
pdf(args[2],width=7,height=4)

# margins?
# par(mar=c(4,4,1,1))

# number of arcs in 1 step
xs <- data$sent.length
ys <- data$chart.arcs.1 + data$agenda.arcs.1
local <- data.frame(length=xs, size=ys)
aggr <- aggregate(local$size, list(length=local$length), mean)

plot(aggr$length, aggr$x, type="b", col='green'
	, pch=4
	# , xlab='(a)\n'
	, xlab='sentence length'
	# , xlim=c(4,20)
	# , ylim=c(1,2000)
	, ylim=c(1,4500)
	, ylab='# of arcs of the hypergraph' )


# number of arcs in 2 step
ys <- data$chart.arcs.2 + data$agenda.arcs.2
local <- data.frame(length=xs, size=ys)
aggr <- aggregate(local$size, list(length=local$length), mean)
lines(aggr$length, aggr$x, type="b", col='red', pch=1)

# add the legend
legend('bottom',
       legend=c('#(Nodes)', '#(Edges)'),
       col=c('green', 'red'),
       pch=c(4, 1),
       cex=1.0, pt.cex = 1, bty='n', lty=1)

# useful if plotting to window:
# Sys.sleep(10)
# dev.off()
