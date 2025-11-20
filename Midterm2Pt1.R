library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(maps)

production <- read.csv("https://github.com/mahsaashouri/STA309-Dataset/raw/refs/heads/main/Milk-Production-Consumption/milk-production-tonnes.csv")
consumption <- read.csv("https://github.com/mahsaashouri/STA309-Dataset/raw/refs/heads/main/Milk-Production-Consumption/per-capita-milk-consumption.csv")

production <- production %>%
  drop_na() %>%
  mutate(prod_scaled = Milk.Production..tonnes. / mean(Milk.Production..tonnes.))
consumption <- consumption %>%
  drop_na() %>%
  mutate(cons_scaled =  Milk.consumption..kilograms.per.year.per.capita./ mean(Milk.consumption..kilograms.per.year.per.capita.))

glimpse(consumption)
glimpse(production)
colnames(consumption)
colnames(production)


#Changes in dairy production/consumption over time
world_prod <- production %>%
  filter(Entity == "World")
world_cons <- consumption %>%
  filter(Entity == "World")

cons <- ggplot(consumption)+
  geom_line(aes(x=Year, y=Milk.consumption..kilograms.per.year.per.capita., group=Entity), color="black", alpha=0.2, size=.2)+
  geom_line(data=world_cons, aes(x=Year, y=Milk.consumption..kilograms.per.year.per.capita.), color="red", size=1)+
  labs(title="Countries' Milk Consumption Over Time", subtitle="In KG Per Year (Per Capita)", y="Milk Consumption")+
  annotate("text", x = 1975, y = 140, label = "World Average", color = "red", fontface="bold", size = 4)+
  theme_minimal()

prod <- ggplot(production)+
  geom_line(aes(x=Year, y=Milk.Production..tonnes., group=Entity), color="black", alpha=0.2, size=.2)+
  geom_line(data=world_cons, aes(x=Year, y=Milk.consumption..kilograms.per.year.per.capita.), color="red", size=1)+
  labs(title="Countries' Milk Production Over Time", subtitle="In Tonnes", y="Milk Production")+
  annotate("text", x = 1970, y = 50000000, label = "World Average", color = "red", fontface="bold", size = 4)+
  theme_minimal()

#A map of dairy production
world_map <- map_data("world")
renamed <- production %>%
  filter(Year==2021)%>%
  mutate(Entity=recode(Entity,
                       "United States"="USA",
                       "Congo"="Democratic Republic of the Congo",
                       "United Kingdom"="UK"))
production_map <- renamed %>%
  right_join(world_map, by=c("Entity" = "region"))
prod.map <- ggplot(production_map, aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill = prod_scaled), color = "black", size=.3) +
  coord_quickmap()+
  labs(title="World Milk Production by Country", subtitle="In Tonnes")+
  theme_map() +
  theme(legend.position="bottom")

#A map of dairy consumption
renamed <- consumption %>%
  filter(Year==2021)%>%
  mutate(Entity=recode(Entity,
                       "United States"="USA",
                       "Congo"="Democratic Republic of the Congo",
                       "United Kingdom"="UK"))
consumption_map <- renamed %>%
  left_join(world_map, by=c("Entity" = "region"))
cons.map <- ggplot(consumption_map, aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill = cons_scaled), color = "black", size=.3) +
  coord_quickmap()+
  labs(title="World Milk Consumption by Country", subtitle="Consumption Per Capita (KG)")+
  theme_map() +
  theme(legend.position="bottom")


#relationship between dairy production and consumption
consumption_2021 <- consumption %>% 
  filter(Year == 2021)

production_2021 <- production %>% 
  filter(Year == 2021)

combined_2021 <- consumption_2021 %>%
  inner_join(production_2021, by = "Entity")%>%
  filter(cons_scaled<10)
        
comp <- ggplot(combined_2021)+
  geom_point(aes(x=cons_scaled, y=prod_scaled, group=Entity), color="black", alpha=.6)+
  geom_smooth(aes(x=cons_scaled, y=prod_scaled))+
  labs(title="Milk Consumption Compared to Production Levels", x="Milk Consumption (Scaled)", y="Milk Production (Scaled)")+
  theme_minimal()

print(comp)
print(cons.map)
print(prod.map)
print(prod)
print(cons)

#DASHBOARDS
dash1 <- cons.map + prod.map
dash2 <- cons + prod 
dash3 <- comp
ggsave(filename="dash1.png", plot=dash1,
       dpi=600, width=15, height=10)
ggsave(filename="dash2.png", plot=dash2,
       dpi=600, width=15, height=10)
ggsave(filename="dash3.png", plot=dash3,
       dpi=600, width=15, height=10)

Part1Dash <- cons.map + prod.map + comp / cons + prod 
ggsave(filename="Part1Dash.png", plot=Part1Dash,
       dpi=600, width=15, height=10)
