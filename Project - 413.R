#Importing the necessary libraries

library(ggplot2)
library(data.table)
library(haven)
library(ggthemes)
library(pwt9)
library(CBRT)

myCBRTKey <- c("Ql0gFEv8km")
pwt <- read_stata("http://www.rug.nl/ggdc/docs/pwt90.dta")
pwt <- copy(pwt9.1)
setDT(pwt)

data <- pwt[, list(isocode, year, rtfpna, hc, csh_x)]
data <- data[isocode == c("TUR") & year >= 1995]
names(data) <- c("isocode", "year", "tfp", "hc", "csh_x")

world_bank_data <- read.csv("world_bank_data_cleaned.csv")
world_bank_data
names(world_bank_data) <- c("year","inflation_rate", "gini","benins","bensol","bensa","benun","adeqin","adeqsocl","adeqsocsaf","adequn","grosssave","popgrowth","gdp","gdppc","export")

green_data <- read.csv("green_growth_OECD_cleaned.csv")
names(green_data) <- c("year", "co2_pbprod", "co2_dbprod","nonenprod","epgrowth","ncapt", "waterstress","expostopm25","foreststock","envtech","energprod","renrd","renrdgov","natgdp")

data <- data[world_bank_data, on = "year"]
data <- data[green_data, on = "year"]
data$econgrowth <- 1
for(i in c(2:25)){
  data$econgrowth[i] <- ((data$gdp[i] - data$gdp[i-1])/data$gdp[i-1])*100
  
}

data$isocode[23] <- "TUR"
data$isocode[24] <- "TUR"
data$isocode[25] <- "TUR"
data$renrd[15] <- mean(data$renrd[14], data$renrd[19])
data$renrd[16] <- mean(data$renrd[14], data$renrd[19])
data$renrd[17] <- mean(data$renrd[14], data$renrd[19])
data$renrd[18] <- mean(data$renrd[14], data$renrd[19])

tfp_graph <- ggplot(data, aes(x = year, y = tfp)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "TFP at constant national prices (2011 = 1)", x = "Year", title = "Total Factor Productivity", caption = "Source: Penn World Tables 9.1") +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

hc_graph <- ggplot(data, aes(x = year, y = hc)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Index of human capital per person", x = "Year", caption = "Source: Penn World Tables 9.1") + 
  ggtitle("Human Capital", subtitle = "(based on years of schooling and returns to education)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))


csh_x_graph <- ggplot(data, aes(x = year, y = csh_x)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Share of merchandise exports at current PPPs", x = "Year", title = "Share of merchandise exports at current PPPs", caption = "Source: Penn World Tables 9.1")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

inflation_rate_graph <- ggplot(data, aes(x = year, y = inflation_rate)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Inflation, consumer prices (annual %)", x = "Year", title = "Inflation Rate", caption = "Source: World Development Indicators")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))


gini_graph <- ggplot(data, aes(x = year, y = gini)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Gini index (World Bank estimate)", x = "Year", title = "Gini Coefficient", caption = "Source: World Development Indicators")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(2002,2018,2)) + coord_cartesian(xlim =c(2002, 2018), ylim = c(32, 46)) +
  scale_y_continuous(breaks=seq(32,44,2))


benins_graph <- ggplot(data, aes(x = year, y = benins)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social insurance programs to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social insurance programs to poorest quintile",subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(1.7), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 14)) +
  scale_x_continuous(breaks=seq(2004,2018,2)) + coord_cartesian(xlim =c(2004, 2018))


bensol_graph <- ggplot(data, aes(x = year, y = bensol)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social protection and labor programs to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social protection and labor programs to poorest quintile", subtitle = "(% of total SPL benefits)")+
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) + 
  coord_cartesian(xlim =c(2004, 2018), ylim = c(0, 10)) + scale_y_continuous(breaks = seq(0,10,2))


bensa_graph <- ggplot(data, aes(x = year, y = bensa)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social safety net programs to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social safety net programs to poorest quintile", subtitle = " (% of total safety net benefits)")+
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018))


benun_graph <- ggplot(data, aes(x = year, y = benun)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of unemployment benefits and ALMP to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of unemployment benefits and ALMP to poorest quintile", subtitle = "(% of total U/ALMP benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2008,2018,2)) + 
  coord_cartesian(xlim =c(2008, 2016))


adeqin_graph <- ggplot(data, aes(x = year, y = adeqin)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social insurance programs", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social insurance programs", subtitle = "(% of total welfare of beneficiary households)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018))


adeqsocl_graph <- ggplot(data, aes(x = year, y = adeqsocl)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social protection and labor programs ", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social protection and labor programs", subtitle = "(% of total welfare of beneficiary households)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2))  + 
  coord_cartesian(xlim =c(2004, 2018)) + scale_y_continuous(breaks = seq(28,38,2))

adeqsocsaf_graph <- ggplot(data, aes(x = year, y = adeqsocsaf)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social safety net programs", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social safety net program", subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018), ylim = c(0, 8))


adequn_graph <- ggplot(data, aes(x = year, y = adequn)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of unemployment benefits and ALMP", x = "Year", caption = "Source: World Development Indicators")  + 
  ggtitle("Adequacy of unemployment benefits and ALMP", subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2008,2016,2)) +
  coord_cartesian(xlim =c(2008, 2016))

grosssave_graph <- ggplot(data, aes(x = year, y = grosssave)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adjusted savings: gross savings", x = "Year", title = "", caption = "Source: World Development Indicators")+
  ggtitle("Adjusted savings: gross savings", subtitle = "(% of GNI)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))

popgrowth_graph <- ggplot(data, aes(x = year, y = popgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Population Growth", x = "Year", title = "Population Growth", caption = "Source: World Development Indicators") +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


gdp_graph <- ggplot(data, aes(x = year, y = gdp)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Gross Domestic Product (constant 2010 US$)", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Gross Domestic Product", subtitle = "(constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2016,2))


gdppc_graph <- ggplot(data, aes(x = year, y = gdppc)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "GDP per capita (constant 2010 US$)", x = "Year", caption = "Source: World Development Indicators")+
  ggtitle("GDP per capita", subtitle = " (constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


export_graph <- ggplot(data, aes(x = year, y = export)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Exports of goods and services", x = "Year", caption = "Source: World Development Indicators") +
  ggtitle("Exports of goods and services ", subtitle = "(constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


co2_pbprod_graph <- ggplot(data, aes(x = year, y = co2_pbprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Production-based CO2 productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Production-based CO2 productivity", subtitle = "GDP per unit of energy-related CO2 emissions")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


co2_dbprod_graph <- ggplot(data, aes(x = year, y = co2_dbprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Demand-based CO2 productivity", x = "Year", caption = "Source: OECD") + 
  ggtitle("Demand-based CO2 productivity" , subtitle = "GDP per unit of energy-related CO2 emissions")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2004,2016,2)) + 
  coord_cartesian(xlim =c(2004, 2016), ylim= c(4,6))



nonenprod_graph <- ggplot(data, aes(x = year, y = nonenprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Non-energy material productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Non-energy material productivity" , subtitle = "GDP per unit of DMC")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2018,2))


epgrowth_graph <- ggplot(data, aes(x = year, y = epgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Environmentally adjusted multifactor productivity growth", x = "Year", caption = "Source: OECD") + 
  ggtitle("Environmentally adjusted multifactor productivity growth" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15))+ scale_x_continuous(breaks=seq(1996,2014,2)) + 
  coord_cartesian(xlim =c(1996, 2014))



ncapt_graph <- ggplot(data, aes(x = year, y = ncapt)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Contribution of natural capital", x = "Year", caption = "Source: OECD") + 
  ggtitle("Contribution of natural capital" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2012,2)) +
  coord_cartesian(xlim =c(1996, 2012))


waterstress_graph <- ggplot(data, aes(x = year, y = waterstress)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Water stress", x = "Year", caption = "Source: OECD") + 
  ggtitle("Water stress" , subtitle = "(Total freshwater abstraction as % total available renewable resources)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2014,2)) +
  coord_cartesian(xlim =c(2004, 2014))



expostopm25_graph <- ggplot(data, aes(x = year, y = expostopm25)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Mean population exposure to PM2.5", x = "Year", caption = "Source: OECD") +
  ggtitle("Mean population exposure to PM2.5" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2010,2020,2)) +
  coord_cartesian(xlim =c(2010, 2020))



envtech_graph <- ggplot(data, aes(x = year, y = envtech)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Development of environment-related technologies", x = "Year", caption = "Source: OECD") + 
  ggtitle("Development of environment-related technologies" , subtitle = "% all technologies")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2016,2)) +
  coord_cartesian(xlim =c(1996, 2016))



energprod_graph <- ggplot(data, aes(x = year, y = energprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Energy productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Energy productivity" , subtitle = "GDP per unit of TPES")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2020,2))



renrd_graph <- ggplot(data, aes(x = year, y = renrd)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Renewable energy public RD&D budget", x = "Year", caption = "Source: OECD") +
  ggtitle("Renewable energy public RD&D budget" , subtitle = "% total energy public RD&D")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2016,2))



renrdgov_graph <- ggplot(data, aes(x = year, y = renrdgov)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Environmentally related government R&D budget", x = "Year", caption = "Source: OECD") +
  ggtitle("Environmentally related government R&D budget" , subtitle = "% total government R&D")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2008,2020,2)) +
  coord_cartesian(xlim =c(2008, 2020))



natgdp_graph <- ggplot(data, aes(x = year, y = natgdp )) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "National expenditure on environmental protection", x = "Year", caption = "Source: OECD") + 
  ggtitle("National expenditure on environmental protection" , subtitle = "% GDP")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 18),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2014,2018,2)) +
  coord_cartesian(xlim =c(2014, 2018)) + scale_x_continuous(breaks = seq(2014,2018,1))

econgrowth_graph <- ggplot(data, aes(x = year, y = econgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Economic Growth", x = "Year", caption = "Source: World Development Indicators") +
  ggtitle("Economic Growth" , subtitle = "in percentages(%)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2020,2)) +
  scale_y_continuous(breaks = seq(-6,10,4))

econgrowth_graph
tfp_graph 
hc_graph
csh_x_graph
inflation_rate_graph
gini_graph 
benins_graph
bensol_graph
bensa_graph
benun_graph
adeqin_graph
adeqsocl_graph
adeqsocsaf_graph
adequn_graph
grosssave_graph 
popgrowth_graph
gdp_graph
gdppc_graph
export_graph
co2_pbprod_graph
co2_dbprod_graph
nonenprod_graph
epgrowth_graph
ncapt_graph
waterstress_graph 
expostopm25_graph
envtech_graph
energprod_graph
renrd_graph
renrdgov_graph 
natgdp_graph
econgrowth_graph

data$