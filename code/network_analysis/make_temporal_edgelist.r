library(jsonlite)
library(lubridate)

tweets <- fromJSON('../../data/tweets.json')

temporal_edgelist <- data.frame()
for(t in tweets)
{
  from <- t$user$screen_name
  if(! is.null(t$entities$user_mentions$screen_name))
  {
    for(to in t$entities$user_mentions$screen_name)
    {
      mention <- data.frame(
        from = from,
        to = to,
        id = t$id_str,
        time = t$created_at
        )
      temporal_edgelist <- rbind(temporal_edgelist, mention)
    }
  }
}