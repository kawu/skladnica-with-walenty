# read file name and data from .csv file
args <- commandArgs(trailingOnly = TRUE)
data <- read.csv(file=args[1], sep=",")

# plot to window or to file?
# x11()
pdf(args[2],width=7,height=4)

# margins?
# par(mar=c(4,4,1,1))

# stats in local variables
xs <- data$sent.length
ys <- (data$chart.nodes.1 + data$agenda.nodes.1) / data$chart.nodes.2
# ys <- data$chart.nodes.1 + data$agenda.nodes.1
local <- data.frame(length=xs, size=ys)
aggr <- aggregate(local$size, list(length=local$length), mean)

# plot info about nodes
plot(aggr$length, aggr$x, type="b", col='green'
	, pch=4
	# , xlab='(a)\n'
	, xlab='sentence length'
	# , xlim=c(1,20)
	, ylim=c(0.5,0.95)
	, ylab='% of the hypergraph explored' )


# compute and plot info about arcs
ys <- (data$chart.arcs.1 + data$agenda.arcs.1) / data$chart.arcs.2
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
