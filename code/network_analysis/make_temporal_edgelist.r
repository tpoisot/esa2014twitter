library(jsonlite)
library(lubridate)

tweets <- fromJSON('../../data/tweets.json')

twitime_to_time <- function(x)
{
  parts <- unlist(strsplit(x, " "))
  time_ <- paste(parts[[6]], "8", parts[[3]], parts[[4]],sep=" ")
  return(strptime(time_, "%Y %m %d %H:%M:%S"))
}

temporal_edgelist <- data.frame()
for(t in tweets)
{
  from <- t$user$screen_name
  if(! is.null(t$entities$user_mentions$screen_name))
  {
    for(to in t$entities$user_mentions$screen_name)
    {
      if(to != from)
      {
        mention <- data.frame(
          from = from,
          to = to,
          id = t$id_str,
          time = twitime_to_time(t$created_at)
        )
        temporal_edgelist <- rbind(temporal_edgelist, mention)
      }
    }
  }
}

save(temporal_edgelist, file="temporal_edgelist.Rdata")
