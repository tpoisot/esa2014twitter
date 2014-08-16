load("temporal_edgelist.Rdata")

library(igraph)
library(plyr)
library(RColorBrewer)

n_slices <- 60

dseq <- seq(from=min(temporal_edgelist$time), to=max(temporal_edgelist$time), length.out = n_slices)
xseq <- seq(from=min(temporal_edgelist$time), to=max(temporal_edgelist$time), length.out = n_slices + n_slices - 1)
xseq <- xseq[!xseq %in% dseq]

temporal <- list()
aggregated <- list()
for(i in c(2:length(dseq)))
{
  tweets <- subset(temporal_edgelist, time >= dseq[i-1] & time <= dseq[i])
  a_tweets <- subset(temporal_edgelist, time <= dseq[i])
  temporal[[i-1]] <- graph.data.frame(tweets)
  aggregated[[i-1]] <- graph.data.frame(a_tweets)
}

complete <- graph.data.frame(temporal_edgelist)
complete_layout <- layout.fruchterman.reingold(complete, area = 50*vcount(complete)^2)
comms <- walktrap.community(complete, steps=10)

comm_color <- colorRampPalette(brewer.pal(10, 'Spectral'))(length(unique(comms$membership)))[comms$membership]

plot(complete, layout=complete_layout, vertex.label=NA, vertex.frame.color=NA, vertex.color=comm_color, edge.arrow.size=0, vertex.size=5)

# v_index <- c(1:vcount(complete))
# names(v_index) <- V(complete)$name
# png(file = "temp_%04d.png", width = 950, height = 950)
# for(i in c(1:length(temporal)))
# {
#   temp <- temporal[[i]]
#   plot(complete, layout=complete_layout,
#        rescale = FALSE,
#        xlim=range(complete_layout[,1]), ylim=range(complete_layout[,2]),
#        vertex.label=NA, vertex.size=2,
#        vertex.color='lightgrey',
#        vertex.frame.color='darkgrey',
#        edge.color='lightgrey',
#        edge.arrow.size=0)
#   title(xseq[i])
#   t_vertices <- v_index[V(temp)$name]
#   plot(temp, layout=complete_layout[t_vertices,],
#        add=TRUE, rescale=FALSE,
#        vertex.color='purple',
#        vertex.frame.color='purple',
#        vertex.size=5,
#        edge.arrow.size=0,
#        vertex.label=NA,
#        xlim=range(complete_layout[,1]), ylim=range(complete_layout[,2]),
#        edge.color='black')
# }
# dev.off()
# 
# # The we use ImageMagick to convert all images to a gif file
# system("convert -delay 50 temp_*.png animation_esa.gif")
# # And remove the images
# file.remove(list.files(pattern = "temp_"))