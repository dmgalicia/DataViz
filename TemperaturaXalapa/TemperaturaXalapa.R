library(dplyr)
library(scales)
library(ggplot2)
library(reshape2)
library(MetBrewer)

setwd("D:/ProyectosRandom/Dataviz/TemperaturaXalapa/")

df <- read.csv("30135-m.csv")
df$FECHA <- as.Date(df$FECHA, format = "%d/%m/%Y")
df$FECHA2 <- as.Date(format(df$FECHA,"2000-%m-%d"))
df$AÑO <- format(df$FECHA,"%Y")
df$MES <- format(df$FECHA,"%m")
df$TMAX <- as.numeric(df$TMAX)
df$TMIN <- as.numeric(df$TMIN)
df$TMEAN <- (df$TMAX + df$TMIN)/2

ggplot() + coord_polar() +
  geom_segment(data = subset(df, AÑO != 2018), 
               aes(x = FECHA2, xend = FECHA2, y = TMIN, yend = TMAX, alpha = 0.01),  
               color = "#e8e8e8", lineend = "round") +
  geom_segment(data = subset(df, AÑO == 2018), 
               aes(x = FECHA2, xend = FECHA2, y = TMIN, yend = TMAX, col = TMEAN), 
               lineend = "round") +
  geom_segment(aes(x = as.Date("2000-03-20"), xend = as.Date("2000-06-20"), y = 40, yend = 40))+
  geom_segment(aes(x = as.Date("2000-06-21"), xend = as.Date("2000-09-22"), y = 40, yend = 40))+
  geom_segment(aes(x = as.Date("2000-09-23"), xend = as.Date("2000-12-20"), y = 40, yend = 40))+
  geom_segment(aes(x = as.Date("2000-01-27"), xend = as.Date("2000-01-27"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-02-12"), xend = as.Date("2000-02-12"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-03-28"), xend = as.Date("2000-03-28"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-04-22"), xend = as.Date("2000-04-22"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-05-03"), xend = as.Date("2000-05-03"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-09-09"), xend = as.Date("2000-09-09"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-10-17"), xend = as.Date("2000-10-17"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-11-13"), xend = as.Date("2000-11-13"), y = 35, yend = 30))+
  geom_segment(aes(x = as.Date("2000-12-09"), xend = as.Date("2000-12-09"), y = 35, yend = 30))+
  theme(legend.position = "none", panel.grid.major.y = element_line(color = "#dddddd"), 
        panel.grid.minor.x = element_line(color = "#dddddd"), axis.title = element_blank(), 
        panel.background = element_rect(fill= "white"), axis.ticks = element_blank()) +
  scale_color_gradientn(colors = met.brewer("Hokusai3", direction = -1, n = 12)[c(1:12,12,12)]) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), 
                     labels = c("0°C", "", "20°C", "", "40°C", ""), limits = c(-20,50)) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

df %>% group_by(AÑO)%>% summarise(TMEAN = mean(TMEAN, na.rm = T), TMAX = mean(TMAX, na.rm = T),
                                   TMIN = mean(TMIN, na.rm = T))





  df2 <- df %>% select(FECHA, FECHA2, TMEAN, AÑO) %>% subset(AÑO == 2006 | AÑO == 2018) %>%
  group_by(FECHA2) %>% summarise(TMAX = max(TMEAN), TMIN = min(TMEAN))

df %>% group_by(AÑO, MES) %>% summarise(TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>% 
  mutate(FECHA = as.Date(paste(AÑO,"-",MES,"-","01",sep = ""))) %>% ggplot() +
  geom_ribbon(aes(x = FECHA, ymin = TMIN, ymax = TMAX, fill = AÑO))

ggplot() + geom_line(data = subset(df, AÑO == 2006), aes(x = FECHA2, y = TMEAN, group = AÑO, col = AÑO)) + 
  geom_line(data = subset(df, AÑO == 2018), aes(x = FECHA2, y = TMEAN, group = AÑO, col = AÑO)) + 
  geom_ribbon(data = df2, aes(x = FECHA2, ymin = TMIN, ymax = TMAX))


df %>% ggplot(aes(x = FECHA2, y = TMAX, group = AÑO, fill = AÑO)) + 
  geom_ribbon(aes(ymin = TMIN, ymax = TMAX, x = FECHA2)) + scale_fill_manual(values = met.brewer("Hokusai1", 14))
