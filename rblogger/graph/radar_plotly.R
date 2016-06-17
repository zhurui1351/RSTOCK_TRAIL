library(plotly)
library(dplyr)

# Read in data
df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Consumer%20Complaints.csv", 
               stringsAsFactors = F, check.names = F)

# Melt
df <- reshape2::melt(df, id = c("Company"))
colnames(df) <- c("Company", "Complaint", "Percent")

getPolarCoord <- function(r, matrix = F, na = F){
  # Get starting angle and angle increments
  theta <- 0
  dtheta <- 360 / length(r)
  dtheta <- (pi / 180) * dtheta  # in radians
  
  # Get polar coordinates
  x <- c()
  y <- c()
  
  for(i in 1:length(r)){
    
    x <- c(x, r[i] * cos(theta))
    y <- c(y, r[i] * sin(theta))
    
    theta <- theta + dtheta
  }
  
  x[length(x) + 1] <- x[1]
  y[length(y) + 1] <- y[1]
  
  if(na == T){
    x[length(x) + 1] <- NA
    y[length(y) + 1] <- NA
  }
  
  
  if(matrix == T){
    return(cbind(x, y))
  }else{
    return(list(x = x, 
                y = y))
  }
  
}

coords <- by(df, df[,"Complaint"], function(r){
  x <- getPolarCoord(r[,3])
  x <- cbind(x$x, x$y)
  x <- data.frame(rbind(r, r[1,]), x = x[,1], y = x[,2])
  return(x)
})

coords <- rbind(coords[[1]], coords[[2]], coords[[3]])
df <- data.frame(coords, txt = paste(coords$Company, "<br>", 
                                     coords$Complaint, ":", 
                                     round(coords$Percent*100, 2), "%"))

# Plot
smooth <- 1
bgcolor <- "white"

p <- plot_ly(data = df, 
             x = x, y = y, mode = "lines", 
             group = Complaint,
             fill = "toself",
             line = list(smoothing = smooth, shape = "spline"),
             hoverinfo = "text",
             text = txt) %>% 
  
  add_trace(data = df, 
            x = x, y = y, mode = "markers", 
            marker = list(color = "white", 
                          size = 10, 
                          line = list(width = 2)),
            hoverinfo = "none",
            showlegend = F) %>% 
  
  layout(xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                      domain = c(0.02, 0.48)),
         yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                      domain = c(0, 0.92)),
         font = list(family = "serif", size = 15),
         legend = list(x = 0.55, y = 0.9, bgcolor = "transparent"),
         plot_bgcolor = bgcolor,
         paper_bgcolor = bgcolor)

# Add grids
grid <- rbind(getPolarCoord(rep(0.05, 50), matrix = T, na = T),
              getPolarCoord(rep(0.10, 80), matrix = T, na = T),
              getPolarCoord(rep(0.15, 150), matrix = T, na = T),
              getPolarCoord(rep(0.20, 170), matrix = T, na = T),
              getPolarCoord(rep(0.25, 200), matrix = T, na = T))

grid <- as.data.frame(grid)

p <- add_trace(p, data = grid,
               x = x, y = y, mode = "lines",
               line = list(color = "#57788e", dash = "4px", width = 1),
               showlegend = F,
               hoverinfo = "none")

inner <- getPolarCoord(rep(0.06, 5))
outer <- getPolarCoord(rep(0.27, 5))

x = t(cbind(inner$x, outer$x))
y = t(cbind(inner$y, outer$y))

x <- as.numeric(apply(x, 2, function(vec){
  return(c(vec, NA))
}))

y <- as.numeric(apply(y, 2, function(vec){
  return(c(vec, NA))
}))

linegrid <- data.frame(x = x, y = y)

p <- add_trace(p, data = linegrid,
               x = x, y = y, mode = "lines",
               line = list(color = "#57788e", dash = "4px", width = 1),
               showlegend = F,
               hoverinfo = "none")

# Add text
banks <- c("Bank of<br>America",
           "Wells Fargo<br>&Company",
           "JP Morgan<br>Chase & Co.",
           "CitiBank",
           "Capital One")
labels <- paste0("<em>", banks, "</em>")
p <- add_trace(p, data = getPolarCoord(rep(0.28, 5)),
               x = x, y = y, mode = "text", text = labels,
               showlegend = F,
               hoverinfo = "none",
               textfont = list(family = "serif", color = "#808080"))

# Add a gray circle
p <- add_trace(p, data = getPolarCoord(rep(0.24, 200)),
               x = x, y = y,
               fill = "toself",
               fillcolor = "rgba(200, 200, 200, 0.3)",
               line = list(color = "transparent"),
               mode = "lines",
               hoverinfo = "none",
               showlegend = F)

# Add titles, description etc

p <- layout(p, 
            annotations = list(
              list(xref = "paper", yref = "paper", 
                   xanchor = "left", yanchor = "top",
                   x = 0.03, y = 1, 
                   showarrow = F, 
                   text = "<b>Consumer complaints for five large banks in the U.S.</b>",
                   font = list(family = "serif",
                               size = 25, 
                               color = "#4080bf")),
              
              list(xref = "paper", yref = "paper", 
                   xanchor = "left", yanchor = "top",
                   x = 0.03, y = 0.95, 
                   showarrow = F, 
                   text = '<em>Source: Consumer Financial Protection Bureau</em>',
                   font = list(family = "serif",
                               size = 16, 
                               color = "#679bcb")),
              
              list(xref = "paper", yref = "paper", 
                   xanchor = "left", yanchor = "top",
                   x = 0.60, y = 0.20, 
                   showarrow = F, 
                   align = "left",
                   text = "Complaints received by the Consumer Financial Protection Bureau<br>regarding financial products and services offered by five large banks in<br>in the United States expressed as a percentage of total nummber<br>of complaints.",
                   font = list(family = "arial",
                               size = 12)),
              
              list(xref = "paper", yref = "paper", 
                   xanchor = "left", yanchor = "top",
                   x = 0.60, y = 0.05, 
                   showarrow = F, 
                   align = "left",
                   text = '<a href = "https://catalog.data.gov/dataset/consumer-complaint-database">Click here to go to source</a>',
                   font = list(family = "arial",
                               size = 14))
            ),
            
            shapes = list(
              list(
                xref = "paper", yref = "paper",
                x0 = 0, x1 = 0.95,
                y0 = 0, y1 = 1,
                type = "rect",
                layer = "above",
                fillcolor = "rgba(191, 191, 191, 0.1)",
                line = list(color = "transparent"))
            ))

print(p)
