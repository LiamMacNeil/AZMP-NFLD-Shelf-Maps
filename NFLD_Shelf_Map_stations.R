install.packages("rgdal")
install.packages("raster")
install.packages("readr")
install.packages("colormap")
library(marmap)
library(rgdal)
library(raster)
library(ggplot2)
library(sf)
library(readr)
library(colormap)
library(lattice)

setwd("~/Documents/Chp2-Figures")

#Bathlymmetry Data
NFLD <- getNOAA.bathy(lon1 = -40, lon2 = -60,
                        lat1 = 55, lat2 = 38, resolution = 1)
#Sampling Locations
Full_samples <- read_csv("~/Documents/Chp 2 Work/NFLD_AZMP_HM_ForMap.csv")
Full_samples_loc <- Full_samples %>% 
  dplyr::select(Latitude, Longitude)
Depth_samples <- read_csv("NFLD_AZMP_HM_Depth.csv")
Samples <- read_csv("~/Documents/Chp 2 Work/NFLD_AZMP_Stations_Trimmed.csv")
HM_Samples <- read_csv("HM_NFLD_AZMP.csv")


summary(NFLD)
par(mar = c(3, 3, 3, 3))
par(oma=c(1,1,1,1)) # all sides have 3 lines of space

plot(NFLD)
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1", "lightsteelblue")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

# Plot the bathymetry
plot.bathy(NFLD, image = TRUE, asp=NA, lwd = 0.4, coast=T, land = TRUE, add=T, shallow=-50, deep=-5000, step=200,
     bpal = list(c(0, max(NFLD), "grey"), c(min(NFLD), 0, blues)))

plot.bathy(NFLD, image = TRUE, asp=NA, land = TRUE, lwd=0.4, add=T, shallow=-50, deep=-5000, step=300,
     xlab="Longitude", ylab="Latitude",
     bpal = list(c(0, max(NFLD), grey(.7), grey(.9), grey(.95)),
                 c(min(NFLD), 0, "darkblue","deepskyblue", "lightblue")))

scaleBathy(NFLD, deg = 2, x = "bottomleft", inset = 5)

points.ba(Full_samples$Latitude, Full_samples$Longitude, pch = 21, col = "black",
       bg = "yellow", cex = 1.3)

SEGB_lines <- read_csv("SEGB_HM.csv")
View(SEGB_lines)
BB_lines <- read_csv("BB_HM.csv")

Full_transect <- read_csv("~/Documents/Chp 2 Work/NFLD_AZMP_Stations_Trimmed.csv")
  
BathPLot <- autoplot(NFLD, geom=c("r", "c"), 
                     coast= TRUE, size=0.1)+
  geom_point(aes(x=Longitude, y=Latitude), shape=21, 
             fill="black", 
             color="red",
             size=3, 
             data = Full_transect)+
  scale_fill_etopo()+
  geom_contour(aes(z=z),
               breaks=c(0, -250, -500, -800, -1000,-1500, -2000, -4000, -4500, -6500),
               colour="black", size=0.2)+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  xlab('Latitude')+
  ylab('Longitude')+
  labs(fill = "Elevation (m)")+
  ggtitle('Newfoundland Shelf Sampling Stations')+
  theme(text = element_text(size=16), 
        plot.title = element_text(hjust = 0.1))

BathPLot
BathPLot <- BathPLot + geom_point(data=Depth_samples, 
                                 aes(x=Longitude, 
                                     y=Latitude),
                                 shape= 21,
                                     fill="white", 
                                     color="red",
                                     size=3)

BathPLot


ggsave(path ="~/Documents/Chp2-Figures", filename = "Bathplot.png", 
       plot = BathPLot, width = 10, height = 10, device='png', dpi=300)

################################ Rasterized plotting approach ################################ 
d <- marmap::as.raster(NFLD)
d <- as.data.frame(d, xy=TRUE)

# upper and lower elevation bounds
z <- max(d$layer)
a <- min(d$layer)

# breaks and labels for color scale
brks <- c(a, 500,100, 0, -200, -1000, -2000, -3000, -4000, -5000, z)
labs <- c("> 1000", "100", "0", "-100", "-1000", "-2000",
          "-3000", "-4000", "-5000", "< -6000")
d$bin <- cut(d$layer, breaks = brks, labels = labs)
d <- d[!is.na(d$bin), ] # filter sneaky NA values

gr   <- colormap(colormaps$greys, nshades = 10)[4:6]
bl   <- colormap(colormaps$velocity_blue, nshades = 13)[3:9]
cols <- c(bl, gr)

# plot
raster<- ggplot(d, aes(x, y, fill = bin)) +
  geom_raster() +
  scale_fill_manual(values = cols, limits = labs, labels = rev(labs)) +
  theme_minimal(20) +
  labs(fill = "Elevation (m)")+
  geom_point(aes(x=Longitude, y=Latitude), shape=21, 
             fill="black", 
             color="red",
             size=3, 
             data = Full_samples)+
  xlab('Latitude')+
  ylab('Longitude')+
  guides(fill = guide_legend(reverse = TRUE))

raster <- raster + geom_point(data=Depth_samples, 
                                  aes(x=Longitude, 
                                      y=Latitude),
                                  shape= 21,
                                  fill="white", 
                                  color="red",
                                  size=3)

ggsave(path ="~/Documents/Chp2-Figures", filename = "Raster_Bathplot_HM-DNA.png", 
       plot = raster, width = 10, height = 10, device='png', dpi=300)

################################# 3-D Plots ################################# 
wireframe(unclass(NFLD), shade = TRUE, aspect = c(1/2, 0.1))

plot(NFLD, xlim = c(-60, -50),image = TRUE, land = TRUE,
     deep = c(-5000, 0), shallow = c(0, 0), step = c(1000, 0),
     col = c("lightgrey", "black"), lwd = c(0.8, 1),
     lty = c(1, 1), draw = c(FALSE, FALSE))

belt <- get.box(NFLD, x1 = -52, x2 = -46.0, y1 = 46.0, y2 = 40.0,
                width = 4, col = "red")
png('SEGB_3D.png', width=12, height=10, units='in', res=300)
par(cex=1.5)
par(oma=c(0.5, 0.5,0.5,0.5))
wireframe(belt, shade = TRUE, zoom = 1.1,
          aspect = c(1/4, 0.1),
          screen = list(z = -60, x = -45),
          par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = rgb(0, 0, 0, 0.1)),
          ylab="Elevation (m)",
          xlab="Longitude",
          zlab="Latitude")
dev.off()
