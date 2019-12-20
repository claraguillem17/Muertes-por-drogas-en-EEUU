library(readr)
library(tidyverse)
library (ggplot2)
library(extrafont)

#CARGAMOS LOS DATOS

df <- read_csv("DATA/deaths-due-to-illicit-drug-use-by-age.csv")

df1 <- read_csv("DATA/drug_abuse_data.csv")

totalmuertesEUA <- df %>% mutate(total_muert= `under 5 (deaths)`+`70 and older (deaths)` + `50-69 years (deaths)` + 
`15-49 years (deaths)` + `5-14 year (deaths)`) %>% select(total_muert, Year, Entity) %>% filter(Entity=="North America")


#FILTRAMOS PARA CREAR EL PRIMER GRÁFICO

df2 <- df1 %>% filter(year==2016) %>% select(year, state, drug_deaths, population) %>%
  mutate(percent = (drug_deaths/population)*100) %>% top_n(20, drug_deaths)


#REPRESENTAMOS EL PRIMER GRÁFICO

library(ggThemeAssist)

p1 <- ggplot(df2, aes(x = reorder(state,drug_deaths), y = drug_deaths)) +
  geom_col(fill="darkslategray3")+
  coord_flip()  +
  labs(subtitle = "En Valor absoluto") +
  theme_minimal(base_size = 13) +
  theme(axis.title.x=element_blank(), axis.title.y =element_blank())  +
  theme (axis.text.y=element_text(size=rel(1), color="gray21", face="bold"))+
  theme (axis.text.x =element_text(size=rel(1), color="gray21", face="bold"))+
  theme (plot.subtitle=element_text(size=rel(1), color="gray21", face="bold"))

#FILTRAMOS PARA EL SEGUNDO GRÁFICO

df3 <- df2 %>% top_n(20, percent)


#REPRESENTAMOS EL SEGUNDO GRÁFICO

p2 <- ggplot(df3, aes(x = reorder(state, percent), y = percent)) +
  geom_col(fill="deeppink4") +
  coord_flip()  +
  labs(subtitle = "En porcentaje") +
  theme_minimal(base_size = 13) +
  theme(axis.title.x=element_blank(), axis.title.y =element_blank())  +
  theme (axis.text.y=element_text(size=rel(1), color="gray21", face="bold"))+
  theme (axis.text.x =element_text(size=rel(1), color="gray21", face="bold"))+
  theme (plot.subtitle=element_text(size=rel(1), color="gray21", face="bold"))

#UNIMOS AMBOS GRÁFICOS

library(gridExtra)

grid.arrange(p1,p2,ncol = 2, widths = c(5, 5))


#MUERTES SEGÚN CONTINENTE Y GRUPOS DE EDAD

library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
#install.packages("rgeos")
library(rgeos)
world <- ne_countries(returnclass = "sf")

#- Quitamos Antarctica y Groenlandia
world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")
ggplot() + geom_sf(data = world) + theme_void()
world <- world %>% select( iso_a3, geometry) #- nos quedamos solo con las 2/3 variables que me hacen falta

#-Unimos las tablas "df" y "world"

df <- df %>% filter(Year==2017)
df_world <- left_join(df, world, by = c("Code"="iso_a3"))


#graficamos MAYORES DE 70
df_world <- df_world %>% mutate(muerte70_6 = as_factor(ntile(`70 and older (deaths)`, 6)))
df_world <- df_world %>% mutate(muerte70_6 = ntile(`70 and older (deaths)`, 6))


A <- ggplot(data= df_world, aes(geometry=geometry, fill=muerte70_6)) + geom_sf(color = "white", lwd = 0.2) +
  ggtitle("Muertes en mayores de 70 años") + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) +
  labs(fill = NULL) + theme(panel.background = element_rect(fill = NA))


#graficamos ENTRE 50-69

df_world <- df_world %>% mutate(muerte50_6 = as_factor(ntile(`50-69 years (deaths)`, 6)))
df_world <- df_world %>% mutate(muerte50_6 = ntile(`50-69 years (deaths)`, 6))


B <-ggplot(data= df_world, aes(geometry=geometry, fill=muerte50_6)) + geom_sf(color = "white", lwd = 0.2) +
  scale_fill_viridis_c(option = "cividis", trans = "sqrt") +
  ggtitle("Muertes de 50 a 69 años") + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) +
  labs(fill = NULL) + theme(panel.background = element_rect(fill = NA))

#graficamos ENTRE 15-49

df_world <- df_world %>% mutate(muerte15_6 = as_factor(ntile(`15-49 years (deaths)`, 6)))
df_world <- df_world %>% mutate(muerte15_6 = ntile(`15-49 years (deaths)`, 6))


C <- ggplot(data= df_world, aes(geometry=geometry, fill=muerte15_6)) + geom_sf(color = "white", lwd = 0.2) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  ggtitle("Muertes de 15 a 49 años") + theme(plot.subtitle = element_text(vjust = 1),plot.caption = element_text(vjust = 1)) +
  labs(fill = NULL) + theme(panel.background = element_rect(fill = NA))


#graficamos ENTRE 5-14

df_world <- df_world %>% mutate(muerte5_6 = as_factor(ntile(`5-14 year (deaths)`, 6)))
df_world <- df_world %>% mutate(muerte5_6 = ntile(`5-14 year (deaths)`,6))


D <- ggplot(data= df_world, aes(geometry=geometry, fill=muerte5_6)) + geom_sf(color = "white", lwd = 0.2) +
  scale_fill_viridis_c(option = "rainbow", trans = "sqrt") +
  ggtitle("Muertes de 5 a 14 años")  +  theme(plot.subtitle = element_text(vjust = 1),plot.caption = element_text(vjust = 1)) +
  labs(fill = NULL) + theme(panel.background = element_rect(fill = NA))

#scatterplot

grid.arrange(A,B,C,D,ncol = 2, nrow= 2,widths = c(5, 5))


#EVOLUCIÓN DE LA MUERTE POR DROGAS SEGÚN AÑOS
library(ggplot2)
#install.packages("gganimate")
library(gganimate)
#install.packages("png")
#install.packages("gifski")
library(gifski)
library(hrbrthemes)

PLOT <- df1 %>% group_by(state, year)%>% select(drug_deaths, state,year)

PLOT <- PLOT %>%arrange(desc(drug_deaths))%>% arrange(desc(year))

PLOT <-  PLOT %>% group_by(year)%>%top_n(8, drug_deaths) 


myPlot <-PLOT %>%
  ggplot( aes(x=year, y=drug_deaths, group=state, color=state)) +
  geom_line() + scale_color_manual(values = c("darkred","darkgreen","darkorange1", "gray35","seagreen2", "coral","darkolivegreen2", "chocolate4","hotpink", "yellow1", "springgreen", "darkred","palevioletred4","navyblue","chocolate4","seagreen2")) +
  geom_point(size=1.5)  + theme_ipsum()+
  ggtitle("Evolución de las muertes en EUA") +     
  theme(plot.title = element_text(size=14))+
  transition_reveal(year)


animate(myPlot, renderer = gifski_renderer())

anim_save("MUERTESDROGASEUA.gif")

