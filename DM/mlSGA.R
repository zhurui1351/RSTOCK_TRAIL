#https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/11-SNA/01_google_sg.R
library(igraph)
library(RCurl)
library(RJSONIO)
twitter.network <- function(user) {
  api.url <- paste("https://socialgraph.googleapis.com/lookup?q=http://twitter.com/",
                   user, "&edo=1&edi=1", sep = "")
  api.get <- getURL(api.url)
  # To guard against web-request issues, we create this loop
  # to ensure we actually get something back from getURL.
  while(grepl("Service Unavailable. Please try again later.", api.get)) {
    api.get <- getURL(api.url)
  }
  api.json <- fromJSON(api.get)
  return(build.ego(api.json))
}
build.ego <- function(json) {
  # Extract only the Twitter nodes
  ego <- find.twitter(names(json$nodes))
  # Build the in- and out-degree edgelist for the user
  nodes.out <- names(json$nodes[[1]]$nodes_referenced)
  if(length(nodes.out) > 0) {
    # No connections, at all
    twitter.friends <- find.twitter(nodes.out)
    if(length(twitter.friends) > 0) {
      # No twitter connections
      friends <- cbind(ego, twitter.friends)
    }
    else {
      friends <- c(integer(0), integer(0))
    }
  }
  else {
    friends <- c(integer(0), integer(0))
  }
  nodes.in <- names(json$nodes[[1]]$nodes_referenced_by)
  if(length(nodes.in) > 0) {
    twitter.followers <- find.twitter(nodes.in)
    if(length(twitter.followers) > 0) {
      followers <- cbind(twitter.followers, ego)
    }
    else {
      followers <- c(integer(0), integer(0))
    }
  }
  else {
    followers <- c(integer(0), integer(0))
  }
  ego.el <- rbind(friends, followers)
  return(ego.el)
}

find.twitter <- function(node.vector) {
  twitter.nodes <- node.vector[grepl("http://twitter.com/", node.vector, fixed = TRUE)]
  if(length(twitter.nodes) > 0) {
    twitter.users <- strsplit(twitter.nodes, "/")
    user.vec <- sapply(1:length(twitter.users),
                       function(i) (ifelse(twitter.users[[i]][4] == "account", NA, twitter.users[[i]][4])))
    return(user.vec[which(!is.na(user.vec))])
  }
  else {
    return(character(0))
  }
}

twitter.snowball <- function(seed, k=2) {
  # Get the ego-net for the seed user. We will build onto
  # this network to create the full snowball search.
  snowball.el <- twitter.network(seed)
  
  # Use neighbors as seeds in the next round of the snowball
  new.seeds <- get.seeds(snowball.el, seed)
  rounds <- 1  # We have now completed the first round of the snowball!
  
  # A record of all nodes hit, this is done to reduce the amount of
  # API calls done.
  all.nodes <- seed
  
  
  # Begin the snowball search...
  while(rounds < k) {
    next.seeds <- c()
    for(user in new.seeds) {
      # Only get network data if we haven't already visited this node
      if(!user %in% all.nodes) {
        user.el <- twitter.network(user)
        if(dim(user.el)[2] > 0) {
          snowball.el <- rbind(snowball.el, user.el)
          next.seeds <- c(next.seeds, get.seeds(user.el, user))
          all.nodes <- c(all.nodes, user)
        }
      }
    }
    new.seeds <- unique(next.seeds)
    new.seeds <- new.seeds[!which(new.seeds %in% all.nodes)]
    rounds <- rounds + 1
  }
  # It is likely that this process has created duplicate rows.
  # As a matter of house-keeping we will remove them because
  # the true Twitter social graph does not contain parallel edges.
  snowball.el <- snowball.el[!duplicated(snowball.el),]
  return(graph.edgelist(snowball.el))
}
get.seeds <- function(snowball.el, seed) {
  new.seeds <- unique(c(snowball.el[,1], snowball.el[,2]))
  return(new.seeds[which(new.seeds != seed)])
}
user <- 'johnmyleswhite' 
user.net <- suppressWarnings(read.graph(paste("c:/data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml"))
user.net  <- set.vertex.attribute(user.net, "Label", value = get.vertex.attribute(user.net, "name"))
user.cores <- graph.coreness(user.net, mode = "in")
user.clean <- subgraph(user.net, which(user.cores > 1) - 1)
user.ego <- subgraph(user.net, c(0, neighbors(user.net, user, mode = "out")))

user.sp <- shortest.paths(user.ego)
user.hc <- hclust(dist(user.sp))

png(paste('../images/', user, '_dendrogram.png', sep=''), width=1680, height=1050)
plot(user.hc)
dev.off()

for(i in 2:10) {    
  user.cluster <- as.character(cutree(user.hc, k = i))
  user.cluster[1] <- "0"
  user.ego <- set.vertex.attribute(user.ego, name = paste("HC", i, sep = ""), value = user.cluster)
}

# Add k-means clustering data to network
for(i in 2:10) {
  user.km <- kmeans(dist(user.sp), centers = i)
  user.cluster <- as.character(user.km$cluster)
  user.cluster[1] <- "0"
  user.ego <- set.vertex.attribute(user.ego, name = paste("KM", i, sep = ""), value = user.cluster)
}

# Write files as GraphML format
write.graph(user.net, paste("data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml")
write.graph(user.clean, paste("data/", user, "/", user, "_clean.graphml", sep = ""), format = "graphml")
write.graph(user.ego, paste("data/", user, "/", user, "_ego.graphml", sep = ""), format = "graphml")

user <- "drewconway"
user.graph <- suppressWarnings(read.graph(paste("c:/data/", user, "/", user, "_net.graphml", sep = ""), format = "graphml"))
friends <- V(user.graph)$name[neighbors(user.graph, user, mode = "out") + 1]
user.el <- get.edgelist(user.graph)


non.friends <- sapply(1:nrow(user.el), function(i) ifelse(any(user.el[i,] == user | 
                                                                !user.el[i,1] %in% friends) | user.el[i,2] %in% friends, FALSE, TRUE))
non.friends.el <- user.el[which(non.friends == TRUE),]
friends.count <- table(non.friends.el[,2])
friends.followers <- data.frame(list(Twitter.Users = names(friends.count), 
                                     Friends.Following=as.numeric(friends.count)), stringsAsFactors = FALSE)

friends.followers$Friends.Norm <- friends.followers$Friends.Following / length(friends)
friends.followers <- friends.followers[with(friends.followers, order(Twitter.Users)),]

head(friends.followers)

write.csv(friends.followers, paste("data/", user, "/", user, "_friends_rec.csv", sep=""), row.names=FALSE)
user.ego <- suppressWarnings(read.graph(paste("data/", user, "/", user, "_ego.graphml", sep = ""), format = "graphml"))
friends.partitions <- cbind(V(user.ego)$HC8, V(user.ego)$name)
partition.follows <- function(i) {
  friends.in <- friends.partitions[which(friends.partitions[,1] == i),2]
  partition.non.follow <- non.friends.el[which(!is.na(match(non.friends.el[,1], friends.in))),]
  # If there are no matches for non-followers, return NA
  if(nrow(partition.non.follow) < 2) {
    return(c(i, NA))
  }
  # If there are, return the most popualr user followed by members of this partition
  else {
    partition.favorite <- table(partition.non.follow[,2])
    partition.favorite <- partition.favorite[order(-partition.favorite)]
    return(c(i, names(partition.favorite)[1]))
  }
}

# Run the partition.follow function over all parition, and remove the NAs and duplicate reccomendations
partition.recs <- t(sapply(unique(friends.partitions[,1]), partition.follows))
partition.recs <- partition.recs[!is.na(partition.recs[,2]) & !duplicated(partition.recs[,2]),]

# Get the node index for the entire graph, plus reccommended users
new.friends <- as.character(c(V(user.ego)$name, partition.recs[,2]))
new.index <- match(new.friends, V(user.graph)$name) - 1

# Take a new subgraph, which includes the new reccommendations
partition.graph <- subgraph(user.graph, new.index)

# Add some vertex attribute data for the visualization
all.partition <- rbind(cbind(get.vertex.attribute(user.ego, "HC8"), V(user.ego)$name), partition.recs)
all.index <- match(as.character(all.partition[,2]), V(partition.graph)$name) - 1

partition.graph <- set.vertex.attribute(partition.graph, "REC", index = all.index, value = all.partition[,1])

vertex.sizes <- c("3", rep("1", vcount(user.ego)-1), rep("2", nrow(partition.recs)))
partition.graph <- set.vertex.attribute(partition.graph, "SIZE", index = all.index, value = vertex.sizes)

# Save the resutls as GraphML
write.graph(partition.graph, paste("data/", user, "/", user, "_rec.graphml",sep = ""), format = "graphml")