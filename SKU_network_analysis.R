library(tidyverse)
library(statnet)
library(igraph)
library(ggplot2)
library(grid)
library(gridBase)
library(devtools)


dev.off()
rm(list=ls())
par(xpd=FALSE) # Specifies where in the plotting devicce an object can be plotted
par(mfrow=c(2,2)) #set how plots will share same sqaure space (rows x col)
#Nodes=read.csv(file="C:/Users/lengada1/NCSU/nodes.csv", colClasses=c("SKU"="factor"), header=TRUE)
#Edges=read.csv(file="C:/Users/lengada1/NCSU/edges.csv", colClasses=c("SKU_1"="factor","SKU_2"="factor") ,header=TRUE)
Nodes=read.csv(file="./nodes.csv", colClasses=c("SKU"="factor"), header=TRUE)
Edges=read.csv(file="./edges.csv", colClasses=c("SKU_1"="factor","SKU_2"="factor") ,header=TRUE)

#Subset used only for Node/Edge Graph
Top_nodes=Nodes[0:200,]

Edges$Top1 <- match(Edges$SKU_1, Top_nodes$SKU,nomatch = 0)
Edges$Top2 <- match(Edges$SKU_2, Top_nodes$SKU,nomatch = 0)
Edges$Top<- ifelse(Edges$Top1>=1, ifelse(Edges$Top2>=1,1,0),0)
Top_edges=subset(Edges, Top >= 1) 

#degrees_dist=read.csv(file="C:/Users/lengada1/NCSU/degrees_dist.csv", header=TRUE)
#strength_dist=read.csv(file="C:/Users/lengada1/NCSU/strength_dist.csv", header=TRUE)
#strength_dist=read.csv(file="C:/Users/lengada1/NCSU/strength_dist.csv", header=TRUE)
#order_size_dist=read.csv(file="C:/Users/lengada1/NCSU/order_sizes.csv", header=TRUE)
degrees_dist=read.csv(file="./degrees_dist.csv", header=TRUE)
strength_dist=read.csv(file="./strength_dist.csv", header=TRUE)
order_size_dist=read.csv(file="./order_sizes.csv", header=TRUE)
net<-graph.data.frame(Top_edges, directed=FALSE, vertices=Top_nodes)
V(net)$color = ifelse(Top_nodes$DIVISION=="APP/FTW", "blue", "orange") # Blue='APP/FTW', Orange='Hardware'

par("mar") # For "margins too large" error, you may get this: [1] 5.1 4.1 4.1 2.1
par(oma=c(.3,1,1,1))  # all sides have 3 lines of space  
par(mar=c(5,5,5,2) + 0.1)   

#E(net)$weight=Edges$X..Containing.Orders
#E(net)$width <- 1+10*E(net)$X..Containing.Orders/max(E(net)$X..Containing.Orders)
#edge.start <- get.edges(net, 1:ecount(net))[,1]
#edge.finish<- get.edges(net, 1:ecount(net))[,2]
net_pos<-layout_with_fr(net,weights = E(net)$X..Containing.Orders)
plot(net,vertex.size=Nodes$Sum/1000,vertex.label=NA, main="Top n SKU Pairs")
par(xpd=FALSE)

fit <- lm(Degrees ~ Sum, data=Nodes)
plot(Degrees ~ Sum, xlab = "Units", ylab = "No.of SKU Pairs", data=Nodes, main="Relationship between Units & Sku Pairs")
abline(coef(fit)[1:2])
cf1 <- round(coef(fit), 2)[1]
cf2 <- round(coef(fit), 3)[2]
sf=summary(fit)
sf=round(sf$r.squared,2)
## sign check to avoid having plus followed by minus for negative coefficients
eq1 <- paste0("Units = ", cf1,
              ifelse(sign(cf2)==1, " + ", " - "), abs(cf2*1000), " * (Thous.of Sku Pairs) ")
eq2 <- paste0( "R-Squared =",sf)

mtext(eq1, 3, line=-1.5 )
mtext(eq2, 3, line=-3)
### Plots 3 and 4

# TODO: Plot for degree distribution
# Get plot onto par grid
# Create the function.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Overlay mean, median, mode onto graph from nodes table 
#ggplot(degrees_dist, aes(x=Bin, y=BinMean)) + geom_point()
plot(degrees_dist[,0:2],main="SKU Pairs/SKU Connectedness",xlab="Sorted SKU Quantile",ylab="No. of SKU Pairs")

mtext(paste0("Mean = ",round(mean(Nodes$Degrees),1)) ,3,-2, adj=.05)
mtext(paste0("Median = ", round(median(Nodes$Degrees),1)) ,3,-4, adj=.05)
mtext(paste0("Mode = ", round(getmode(Nodes$Degrees),1)) ,3,-6, adj=.05)


# TODO: Plot for strength distribution
# Overlay mean, median, mode onto graph from nodes table 

plot(strength_dist[,0:2],main="SKU Volume (Units)",xlab="Sorted SKU Quantile",ylab="Units")
mtext(paste0("Mean = ",round(mean(Nodes$Sum),1)) ,3,-2, adj=.05)
mtext(paste0("Median = ", round(median(Nodes$Sum),1)) ,3,-4, adj=.05)
mtext(paste0("Mode = ", round(getmode(Nodes$Sum),1)) ,3,-6, adj=.05)

#  GGPLOT  Order size Dist
library(gridExtra)

order_size_dist$end=c(head(order_size_dist$Cum..,-1),0)
order_size_dist$beg=c(0,head(order_size_dist$Cum..,-1))

ord_size<-ggplot(order_size_dist[0:5,]) +
  geom_rect(aes(x = X..of.SKUs, xmin=X..of.SKUs-.3, xmax=X..of.SKUs+.3, ymin = beg, ymax = end))+
  labs(x = "Unique SKUs in Order", y="% of Orders",title="Unique SKUs per Order")+
  scale_y_continuous(labels = scales::percent)


# Plot 5
# 2D density plot

theme_update(plot.title = element_text(hjust = 0.5))
sp<- ggplot(Nodes, aes(x=Degrees, y=Sum))+
  xlim(0,7) +
  ylim(0,4000)+
  # geom_point() +
  geom_density_2d()+
  labs(x = "No. of SKUs paired with", y="Units",title="SKU Density: Center of Gravity")


#CORRELATION MATRIX

library(reshape2)
library(ggplot2)
daily_top_n_skus=read.csv(file="./daily_top_n_skus.csv", header=TRUE)
cormat <- round(cor(daily_top_n_skus), 2)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat, diag = TRUE)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# # Flip matrix for plot
upper_tri <- upper_tri[,c(ncol(upper_tri):1),drop = FALSE]
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)



###Weighted Regression

library(utils)
dly_sku_sums <- colSums(daily_top_n_skus)

pair_names <- expand.grid(names(dly_sku_sums), names(dly_sku_sums))
pair_values <- rowSums(expand.grid(dly_sku_sums, dly_sku_sums))
dly_pair_volumes <- data.frame(SKU1=pair_names$Var1, SKU2=pair_names$Var2, Volume=pair_values)

# Volume Matrix
dly_sku_vol_mat <- as.matrix(xtabs(Volume ~ SKU1 + SKU2, data=dly_pair_volumes))
attr(dly_sku_vol_mat, "class") <- NULL 
dly_sku_vol_mat <- dly_sku_vol_mat[sort(colnames(dly_sku_vol_mat)), sort(colnames(dly_sku_vol_mat))]
dly_sku_vol_mat_low_tri <- get_lower_tri(dly_sku_vol_mat)
diag(dly_sku_vol_mat_low_tri) <- NA
dly_sku_corr <- cormat[sort(colnames(cormat)),sort(colnames(cormat))]
total_sku_vol <- sum(dly_sku_vol_mat_low_tri, na.rm=TRUE)
wt_corr <- round(sum(dly_sku_corr * dly_sku_vol_mat_low_tri, na.rm = TRUE)/total_sku_vol,2)
#####

# Create a ggheatmap

wc <- paste0( "Top SKUs: Weighted Correlation = ",wt_corr)

#mtext(wc, 3, line=-1.5 )

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  ggtitle(wc) +
  #labs(title=wc)+
  
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 7, hjust = 1))+
  theme(axis.text.y = element_text(size=7))
coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.3) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.9, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))




###  ADD Map here.   Overlay mtext() or clean table with the 3x3 format on the notebook paper:
#                  R1    R2     R3
#% Vol            .2   .2      .4
#% Orders         .25  .25     .5
#Avg Units/Order  1.2   1.4    1.35

grid.arrange( sp,ord_size ,ncol=2)

library(choroplethrZip)
library(gridExtra)

df_pop_zip <- read.csv("region_identity.csv", colClasses = c("region"="character"))
region_metrics <- read.csv("region_metrics.csv")

ec_states = c("alabama", "arkansas", "american samoa", "arizona", "california", "colorado", "connecticut", "district of columbia", "delaware", "florida", "georgia", "guam", "iowa", "idaho", "illinois", "indiana", "kansas", "kentucky", "louisiana", "massachusetts", "maryland", "maine", "michigan", "minnesota", "missouri", "mississippi", "montana", "north carolina", "north dakota", "nebraska", "new hampshire", "new jersey", "new mexico", "nevada", "new york", "ohio", "oklahoma", "oregon", "pennsylvania", "puerto rico", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "virginia", "virgin islands", "vermont", "washington", "wisconsin", "west virginia", "wyoming")

# Create environment
choro = choroplethrZip::ZipChoropleth$new(df_pop_zip)
choro$prepare_map()

data(zip.regions)
choro$legend = "ABCD"
ec_zips = zip.regions[zip.regions$state.name %in% ec_states, "region"]

# All possible boundary lat/long points for all zip codes (choropleth.df)
ec_df   = choro$choropleth.df[choro$choropleth.df$region %in% ec_zips, ]
ec_plot = choro$render_helper(ec_df, "", choro$theme_clean()) + 
  ggtitle("Addidas Region Metrics")

grid.arrange(
  ec_plot + coord_map()+scale_fill_manual(values=c("#F8766D", "#00BA38","#619CFF")), 
  tableGrob(region_metrics), nrow=2, as.table=TRUE, heights=c(3,1))




