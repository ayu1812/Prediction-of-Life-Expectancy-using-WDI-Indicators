
# 0. Set-Up:Downloading packages needed

install.packages('WDI')
#install.packages('ggplot2') # install if you do not have it
#install.packages('tidyverse')
#install.packages('dplyr')
install.packages('GGally')
install.packages("plotly")
install.packages('rworldmap')
library(WDI)
library(wbstats)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(corrplot)
library(ggcorrplot)
library("PerformanceAnalytics")
library(rworldmap)
library(RColorBrewer)
library(gridExtra)
library(randomForest)

# 1. searching the dataset from WDI API
#WDIsearch("gdp.*capita.*US\\$", cache = WDIcache())
#WDIsearch("life expectancy at birth.*total", cache = WDIcache())
#WDIsearch("^mortality.*rate.*infant", cache = WDIcache())
#WDIsearch("education.*primary", cache = WDIcache())

# 1.a Getting the dataset 
WDI_dataset <- WDI(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN", "SE.ADT.1524.LT.ZS","SL.IND.EMPL.ZS","SL.TLF.CACT.FM.ZS","
SP.POP.TOTL","NY.GNP.PCAP.CD"), start = 1960, end = 2015, extra = TRUE)
View(WDI_dataset)

# 3. Cleaning the dataset

WDI_dataset <- subset(WDI_dataset, region != "Aggregates")
names(WDI_dataset)[which(names(WDI_dataset) == "SP.POP.TOTL")] <- "Pop_total"
names(WDI_dataset)[which(names(WDI_dataset) == "NY.GDP.PCAP.KD")] <- "GDP"
names(WDI_dataset)[which(names(WDI_dataset) == "SP.DYN.LE00.IN")] <- "life_expectancy"
names(WDI_dataset)[which(names(WDI_dataset) == "SP.DYN.IMRT.IN")] <- "infant_mortality"
names(WDI_dataset)[which(names(WDI_dataset) == "SE.ADT.1524.LT.ZS")] <- "Youth_Literacy_Rate"
names(WDI_dataset)[which(names(WDI_dataset) == "SL.IND.EMPL.ZS")] <- "Employment_in_Industry"
names(WDI_dataset)[which(names(WDI_dataset) == "SL.TLF.CACT.FM.ZS")] <- "Labor_gender"
names(WDI_dataset)[which(names(WDI_dataset) == "NY.GNP.PCAP.CD")] <- "Gross_national_income"
WDI_dataset<-na.omit(WDI_dataset) 

# 4. Dataset Exploration

dim(WDI_dataset)
str(WDI_dataset)
summary(WDI_dataset)

hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  #  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.050,"cm"),
  panel.spacing.y = unit(0.050,"cm"),
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)
# Visualizations

plot_1<-ggplot(subset(WDI_dataset, year == 2015), aes(x = GDP, y = infant_mortality)) + geom_point() + hw +
  labs(x ='Gross Domestic Product (GDP)', y='Infant Mortality',title = 'Infant Mortality v.s Gross Domestic Product: 2015')

plot_2<-ggplot(subset(WDI_dataset, year == 2015), aes(y = GDP, x = life_expectancy)) + geom_point() + hw +
  labs(y ='Gross Domestic Product (GDP)', x='Life Expectancy',title = 'Life Expectancy v.s Gross Domestic Product: 2015')

plot_3<-ggplot(WDI_dataset, aes(y = Employment_in_Industry, x = Youth_Literacy_Rate)) + geom_point() + hw +
  labs(y ='Employment in Industry', x='Youth Literacy Rate',title = 'Does literacy rate affect employment in industry')

pairs(WDI_dataset[4:6], col="black",lower.panel = panel.smooth,pch=19, main="Scatterplot World Bank")

#ggpairs(WDI_dataset,mapping=aes(col=Type),axisLabels="internal")# not necessary

# Creating Correlation matrix

my_data<-WDI_dataset[,c(4,5,6,7,8,9)]
head(my_data,6)

res<-cor(my_data)
round(res,2)

cor(my_data, use = "complete.obs") #handle missing values


corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.cex=1,tl.srt = 45)+hw 

# Create correlogram

ggcorrplot(res, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of WDI Indicators")+hw

#The function chart.Correlation() in the package [PerformanceAnalytics], can be used to display a chart of a correlation matrix.  

my_data <- WDI_dataset[,c(4,6,7,8,9,5)]
chart.Correlation(my_data, histogram=TRUE, pch=19) +hw

# The above graph provides the following information:

#Correlation coefficient (r) - The strength of the relationship.
#p-value - The significance of the relationship. Significance codes 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Histogram with kernel density estimation and rug plot.
#Scatter plot with fitted line.

#create a heatmap
col<- colorRampPalette(c("red", "white", "blue"))(10)
#png("S10.png", width=1000, height=1000)
heatmap(x = res, col = col, symm = TRUE)+hw
dev.off()
#Create world map for WDI indicators with rworldmap

indicator<-"SP.POP.TOTL"

dFpopulation <- WDI( indicator=indicator,
                     start=2015,
                     end=2015 )
sPDFpopulation <-
  joinCountryData2Map( dFpopulation,
                       nameJoinColumn= "iso2c",
                       joinCode='ISO2')
#numCats <- 5
#colourPalette <- rev(brewer.pal(numCats, "PuBuGn"))

mapCountryData( sPDFpopulation,
                nameColumnToPlot=indicator,
                mapTitle="World Population in 2015", catMethod = "pretty", 
                colourPalette = "heat")+hw

mapCountryData( sPDFpopulation,
                nameColumnToPlot=indicator,
                mapTitle="Eurasia Population in 2005", catMethod = "pretty", 
                mapRegion= "Eurasia",colourPalette = "heat")

#Gross National Income\n as per income groups-2015

ggplot(subset(WDI_dataset, year == 2015 & region!="South Asia",
              select=c(region,Gross_national_income,income)), 
  aes(x=region, y=Gross_national_income))+
  geom_bar(stat='identity', fill="magenta")+ 
  scale_x_discrete(labels = abbreviate)+
  facet_wrap(~income)+hw+
  ggtitle("Gross National Income\n as per income groups-2015") +
  xlab("Region") + 
  ylab("GNI (in US$)")

# time series plot

timeseries1<-ggplot(subset(WDI_dataset, region == "Europe & Central Asia"), 
                    aes(year, life_expectancy)) +
  geom_point(na.rm=TRUE,color="purple", size=3, pch=18)+hw+
  ggtitle("Life Expectancy trend in\n Europe and Central Asia 1990-2015") +
  xlab("Year") + 
  ylab("Life Expectancy")
timeseries1

timeseries1_trend <- timeseries1 + stat_smooth(colour="green")

timeseries1_trend

timeseries2<-ggplot(subset(WDI_dataset, region == "Sub-Saharan Africa"), 
                    aes(year, life_expectancy)) +
  geom_point(na.rm=TRUE,color="purple", size=3, pch=18)+hw+
  ggtitle("Life Expectancy trend in\n Sub-Saharan Africa 1990-2015") +
  xlab("Year") + 
  ylab("Life Expectancy")
timeseries2

timeseries2_trend <- timeseries2 + stat_smooth(colour="green")

timeseries2_trend

timeseries3<-ggplot(subset(WDI_dataset, region == "Latin America & Caribbean"), 
                    aes(year, life_expectancy)) +
  geom_point(na.rm=TRUE,color="purple", size=3, pch=18)+hw+
  ggtitle("Life Expectancy trend in\n Latin America & Caribbean 1990-2015") +
  xlab("Year") + 
  ylab("Life Expectancy")
timeseries3

timeseries3_trend <- timeseries3 + stat_smooth(colour="green")

timeseries3_trend

grid.arrange(timeseries1_trend, timeseries2_trend, timeseries3_trend,ncol=1)

# Create Box plot for GNI

g <- ggplot(subset(WDI_dataset, year == 2015 & region!="South Asia"), 
            aes(region, Gross_national_income))
g + geom_boxplot(varwidth=T, fill="plum") + geom_dotplot(binaxis='y', 
                                                         stackdir='center', 
                                                         dotsize = .5, 
                                                         fill="red") +
  labs(title="Gross National Income (per capita) in 2015", 
       subtitle=" Grouped by Region",
       caption="Source: mpg",
       x="Region",
       y="GNI (in US$)")+hw

#Box plot for life expectancy

q <- ggplot(subset(WDI_dataset, year == 2015 & region!="South Asia"), 
            aes(region, life_expectancy))
q + geom_boxplot(varwidth=T, fill="green") + geom_dotplot(binaxis='y', 
                                                         stackdir='center', 
                                                         dotsize = .5, 
                                                         fill="black")+
  labs(title="Life Expectancy in 2015", 
       subtitle=" Grouped by Region",
       x="Region",
       y="Life Expectancy(in years))")+hw

# Box plot for Employment in industry

r <- ggplot(subset(WDI_dataset, year == 2015 & region!="South Asia"), 
            aes(region, Employment_in_Industry))
r + geom_boxplot(varwidth=T, fill="orange") + geom_dotplot(binaxis='y', 
                                                          stackdir='center', 
                                                          dotsize = .5, 
                                                          fill="forest green")+
  labs(title="Industrial Employment in 2015", 
       subtitle=" Grouped by Region",
       x="Region",
       y="Employment(% of total employment))")+hw



