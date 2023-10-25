#installing libraries
install.packages("WDI")
install.packages("tidyverse")
library(WDI)
library(tidyverse)
options(scipen=999)

wdi_cache <- WDIcache() #creating WDI cache variable

#Creating meain dataset
dataset <- WDI(country="all",
         indicator = c("EN.ATM.PM25.MC.M3","EG.USE.COMM.FO.ZS","IS.SHP.GOOD.TU","EN.ATM.GHGT.KT.CE",
                      "EG.USE.PCAP.KG.OE","SP.POP.TOTL","SP.DYN.LE00.IN","IS.AIR.DPRT",
                      "EN.ATM.CO2E.KT","EG.USE.ELEC.KH.PC","EN.ATM.METH.KT.CE","AG.LND.AGRI.ZS","EG.FEC.RNEW.ZS",
                      "EN.ATM.NOXE.KT.CE"),
         start = 2010 , end = 2015,
         extra=TRUE,
         cache = wdi_cache)

#----------------------------------chart 1 - World Map---------------------------------------------

# create data for world coordinates using 
# map_data() function
world_map <- map_data("world")
countryDataWDI <- dataset %>% mutate(country=recode(str_trim(country),
                                              "United States"="USA",
                                              "United Kingdom"="UK",
                                              "Congo, Dem. Rep."="Democratic Republic of the Congo",
                                              "Congo, Rep."="Republic of Congo",
                                              "Kyrgyz Republic"="Kyrgyzstan",
                                              "Egypt, Arab Rep."="Egypt",
                                              "Russian Federation"="Russia",
                                              "Iran, Islamic Rep."="Iran",
                                              "Venezuela, RB"="Venezuela",
                                              "Yemen, Rep."="Yemen",
                                              "Turkiye"="Turkey",
                                              "Czechia"="Czech Republic",
                                              "Slovak Republic"="Slovakia",
                                              "Cote d'Ivoire"="Ivory Coast",
                                              "Lao PDR"="Laos",
                                              "Korea, Dem. People's Rep."="North Korea",
                                              "Korea, Rep."="South Korea"))
countryDataWDIMap <- left_join(world_map,countryDataWDI,by = c("region"="country"))

ggplot(filter(countryDataWDIMap, year=="2015", !is.na(EN.ATM.PM25.MC.M3)),
       aes(long,lat,group=group))+
  geom_polygon(aes(fill=EN.ATM.PM25.MC.M3),colour="white")+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_void()+
  theme(plot.title = element_text(face="bold",hjust = 0.5,size=14),
        plot.caption = element_text(face = "italic", hjust = 0.6, size=14),
        legend.text = element_text(color = "black", size = 16),
        legend.key.height = unit(6,"line")
        )+
  labs(fill="Pollution", title="World map coloured by POLLUTION in 2015",
       caption="Data source: World Development Indicators (WDI)")


#----------------------------------chart 2 - HEATMAP---------------------------------------------

#print length of each vector
length(dataset$EG.USE.COMM.FO.ZS)

install.packages("reshape")
library(reshape)
y <- data.frame("country" = dataset$country,
                "year" = dataset$year,
                "Air pollution" = dataset$EN.ATM.PM25.MC.M3,
                "Fossil fuel" = dataset$EG.USE.COMM.FO.ZS,
                "Agricultural land" = dataset$AG.LND.AGRI.ZS,
                "Renewable energy consumption" = dataset$EG.FEC.RNEW.ZS)
y <- filter(y, (country=="China" | country=="India" | country=="United States" |
                  country=="Indonesia" | country=="Pakistan") &
              year==2014)
y = subset(y, select = -c(year) )

y<-melt(y)

ggplot(y,aes(country,reorder(variable, value), fill = value)) + geom_tile()+
  scale_fill_viridis_c(option = "A", limits = c(0,100), breaks = c(0,20, 40, 60, 80, 100), 
                       guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(plot.title = element_text(face="bold",hjust = 0.5,size=14),
        plot.caption = element_text(face = "italic", hjust = 0.9, size=10),
        legend.text = element_text(color = "black", size = 16),
        legend.key.height = unit(6,"line") 
  )+
  labs(fill="Pollution", x = "Country", y="Indicators", title="Factors affecting pollution 
       in the five most populous country",
       caption="Data source: World Development Indicators (WDI)")
  


#----------------------------------chart 3 - radar---------------------------------------------


install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)

library(ggradar)
library(scales)
library(tidyverse)

xy <- data.frame("country" = dataset$country,
                 "year" = dataset$year,
                 "Air pollution" = dataset$EN.ATM.PM25.MC.M3,
                 "Fossil fuel" = dataset$EG.USE.COMM.FO.ZS,
                 "Life expectancy at birth" = dataset$SP.DYN.LE00.IN,
                 "Agricultural land" = dataset$AG.LND.AGRI.ZS,
                 "Renewable energy consumption" = dataset$EG.FEC.RNEW.ZS
)

xy<-xy[xy$country!="Aggregates",]
xy<-na.omit(xy)

xy <- filter(xy, (country=="China" | country=="India" | country=="United States" |
                    country=="Indonesia" | country=="Pakistan") &
               year==2014)

xy = subset(xy, select = -c(year) )

xy %>% mutate_if(is.numeric, rescale) %>%
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(new_country) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 3,
          legend.title = "Country",
          legend.position = "bottom",
          legend.text.size = 8,
          plot.title = "Factors affecting pollution\nin top 5 populated country",
          values.radar = c("MIN","AVG","MAX")
          ) +
  labs(caption="Data source:World Development Indicators (WDI)") +
  theme(
        plot.caption = element_text(face = "italic", hjust = 0.9, size=10)
  )


#----------------------------------chart 4 - Lollipop---------------------------------------------


library(dplyr)
library(forecast)

xyz <- dataset %>% filter(region!="Aggregates" & (year==2015 | year==2014))

xyz %>% 
  arrange(SP.DYN.LE00.IN) %>%
  ggplot( aes(x=EN.ATM.PM25.MC.M3, y=SP.DYN.LE00.IN)) +
  geom_segment( aes(xend=EN.ATM.PM25.MC.M3, yend=0 , color = EN.ATM.PM25.MC.M3)) +
  geom_point( aes(color=EN.ATM.PM25.MC.M3), size=4) +
  theme_bw() +
  labs( title = "Pollution related Life Expectancy" ,
        caption="Data source:World Development Indicators (WDI)",
        x = "Pollution" , y = "Life Expectancy\n" , color = "Life Expectancy") +
  scale_fill_viridis_c(aesthetics = "color", option="C", values = c(0,0.05,0.05,0.1,0.2,0.4,0.7,1)) +
  theme(plot.title = element_text(face="bold",hjust = 0.5,size=14),
        plot.caption = element_text(face = "italic", hjust = 0.9, size=10),
        legend.key.height = unit(6,"line")
  )
  