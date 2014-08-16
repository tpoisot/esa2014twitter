library(lubridate)

mentions <- read.table("temporal.txt", h=F)
colnames(mentions) <- c('from', 'to', 'wd', 'm', 'd', 'hms', 'tz', 'yr')
mentions$time <- ymd_hms(str_c("2014-08-", mentions$d, " ", mentions$hms))

n_slices <- 45

dseq <- seq(from=min(mentions$time), to=max(mentions$time), length.out = n_slices)
xseq <- seq(from=min(mentions$time), to=max(mentions$time), length.out = n_slices + n_slices - 1)
xseq <- xseq[!xseq %in% dseq]

temporal <- list()
aggregated <- list()
for(i in c(2:length(dseq)))
{
  tweets <- subset(mentions, time >= dseq[i-1] & time <= dseq[i])
  a_tweets <- subset(mentions, time <= dseq[i])
  temporal[[i-1]] <- graph.data.frame(tweets)
  aggregated[[i-1]] <- graph.data.frame(a_tweets)
}

mod <- function(x) modularity(walktrap.community(x))
conn <- function(x) length(E(x))/(length(V(x))^2)

tem_beta <- data.frame()
agr_beta <- data.frame()

for(i in c(1:length(temporal)))
{
  if(i == 1){j <- i} else {j <- i-1}
  tem_beta <- rbind(tem_beta, data.frame(betalink(temporal[[i]], temporal[[j]])))
  agr_beta <- rbind(agr_beta, data.frame(betalink(aggregated[[i]], aggregated[[j]])))
}

## Contributions
cont <- data.frame()
G <- aggregated[[length(aggregated)]]
f <- mod
for(v in V(G))
{
  tG <- delete.vertices(G, v)
  cont <- rbind(cont, data.frame(v=v, c = f(G) - f(tG)))
}
