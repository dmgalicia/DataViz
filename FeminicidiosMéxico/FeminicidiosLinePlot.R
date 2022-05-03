remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::loadfonts(device = "win")
extrafont::font_import(pattern = "PT")
extrafont::fonts()
library(dplyr)
library(stringi)
library(ggplot2)
library(svglite)
library(reshape2)
library(MetBrewer)
library(viridisLite)

setwd("D:/ProyectosRandom/Dataviz/FeminicidiosMéxico/")

datos22 <- read.csv("IDEFC_NM_mar22.csv", encoding = "latin1")

datos22 <- datos22 %>% filter(Tipo.de.delito == "Feminicidio")

datos22 <- datos22 %>% select(Año, Entidad, Tipo.de.delito, Enero, Febrero, Marzo, Abril, Mayo,
                              Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre) %>%
  melt(c("Año", "Entidad", "Tipo.de.delito"), value.name = "Valor", variable.name = "Mes") %>% 
  group_by(Año, Entidad, Tipo.de.delito) %>% summarise(Valor = sum(Valor, na.rm = T)) %>% 
  na.omit() %>% as.data.frame()

names(datos22)[names(datos22) == "Tipo.de.delito"] <- "Delito"

datos22 %>% group_by(x = ceiling(row_number()/32), Delito) %>% 
  summarise(Valor = sum(Valor), Año = mean(Año)) %>% ggplot() + 
  geom_segment(aes(x = 2015, xend = 2022, y = 0, yend = 0), color = "#f5f5f5") +
  geom_segment(aes(x = 2015, xend = 2022, y = 200, yend = 200), color = "#f5f5f5") +
  geom_segment(aes(x = 2015, xend = 2022, y = 400, yend = 400), color = "#f5f5f5") +
  geom_segment(aes(x = 2015, xend = 2022, y = 600, yend = 600), color = "#f5f5f5") +
  geom_segment(aes(x = 2015, xend = 2022, y = 800, yend = 800), color = "#f5f5f5") +
  geom_segment(aes(x = 2015, xend = 2022, y = 1000, yend = 1000), color = "#f5f5f5") +
  geom_vline(aes(xintercept = 2015), color = "#ededed") + geom_vline(aes(xintercept = 2016), color = "#ededed") + 
  geom_vline(aes(xintercept = 2017), color = "#ededed") + geom_vline(aes(xintercept = 2018), color = "#ededed") + 
  geom_vline(aes(xintercept = 2019), color = "#ededed") + geom_vline(aes(xintercept = 2020), color = "#ededed") + 
  geom_vline(aes(xintercept = 2021), color = "#ededed") + geom_vline(aes(xintercept = 2022), color = "#ededed") + 
  scale_y_continuous(limits = c(0,1000), breaks = c(0, 200, 400, 600, 800, 1000)) +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), 
                     expand = expansion(0, 0.2), position = "bottom") +
  geom_line(aes(x = Año, y = Valor, group = Delito, color = Delito), size = 0.75) +
  geom_point(aes(x = Año, y = Valor, color = Delito), size = 3) +
  geom_text(aes(x = Año, y = Valor, label = Valor), size = 3, vjust = 3) + 
  scale_color_manual(values = rocket(10)[4]) +
  theme(panel.background = element_blank(), axis.title = element_blank(), legend.position = "none",
        axis.ticks = element_blank(), axis.text = element_text(family = "PT Sans", colour = "black"),
        panel.grid = element_blank())

ggsave("FeminicidiosLineScatterPlot.svg", width = 8.5, height = 2, units = "in")
