library(plotly)

p <- plot_ly()

for(i in 1:5){
  p <- add_trace(p, x = 1:10, y = rnorm(10), mode = "lines")
}

p

x = c("A","B","C")
y = c(10,15,12)
z = c(13,11,14)

tab <- data.frame(x,col = y, row.names = x)
tab2 <- data.frame(x,col = z, row.names = x)

x <- row.names(tab)

p <- plot_ly(
  tab,x = factor(x,levels = x), y = ~tab[,"col"], type = "scatter", mode = "lines"
)
p <- add_trace(p, x = factor(x,levels = x), y = ~tab2[,"col"], mode = "lines")
p
