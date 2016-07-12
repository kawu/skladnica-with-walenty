# read file name and data from .csv file
args <- commandArgs(trailingOnly = TRUE)
data <- read.csv(file=args[1], sep=",")

# plot to PDF file
pdf(args[2],width=7,height=4)

# margins?
# par(mar=c(4,4,1,1))

# stats about nodes
xs <- data$sent.length
ys <- (data$chart.nodes.1 + data$agenda.nodes.1) / data$chart.nodes.2
local <- data.frame(length=xs, size=ys)
nodes <- aggregate(local$size, list(length=local$length), mean)

# stats about arcs
ys <- (data$chart.arcs.1 + data$agenda.arcs.1) / data$chart.arcs.2
local <- data.frame(length=xs, size=ys)
arcs <- aggregate(local$size, list(length=local$length), mean)

ymin <- min(nodes$x, arcs$x)
ymax <- max(nodes$x, arcs$x)

# plot info about nodes
plot(nodes$length, nodes$x, type="b", col='green'
	, pch=4
	# , xlab='(a)\n'
	, xlab='sentence length'
	# , xlim=c(1,20)
	, ylim=c(ymin,ymax)
	, ylab='% of the hypergraph explored' )

# plot info about arcs
lines(arcs$length, arcs$x, type="b", col='red', pch=1)

# add the legend
legend('bottom',
       legend=c('#(Nodes)', '#(Edges)'),
       col=c('green', 'red'),
       pch=c(4, 1),
       cex=1.0, pt.cex = 1, bty='n', lty=1)
