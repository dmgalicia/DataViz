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
library(patchwork)
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

datos22$Entidad <- stri_replace_all_regex(datos22$Entidad,
                                          pattern = c("Baja California Sur"),
                                          replacement = c("BS"),
                                          vectorize = F)

datos22$Entidad2 <- stri_replace_all_regex(datos22$Entidad,
                                          pattern = sort(unique(datos22$Entidad)),
                                          replacement = c("AG", "BC", "BS", "CM", "CS", "CH", "CX", "CO",
                                                          "CL", "DG", "GT", "GR", "HG", "JC", "EM", "MI",
                                                          "MO", "NY", "NL", "OA", "PU", "QT", "QR", "SL",
                                                          "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA"),
                                          vectorize = F)

posiciones <- read.csv("Entidades.csv")

datos22 <- datos22 %>%  inner_join(posiciones, by = "Entidad2")

datos22 %>% ggplot(aes(x = XE, y = YE, fill = Valor)) + 
  geom_tile(aes(width = 0.9, height = 0.9))  +
  scale_fill_gradientn(colors = rocket(8)[7:1], limits = c(0, 150)) +
  geom_text(aes(x= XE, y = YE, label = Entidad2), color = "#ffffff", family = "PT Sans", size = 3.5) + 
  #geom_text(aes(x= XE, y = YE-0.2, label = Valor), color = "#ffffff", family = "PT") +
  theme(panel.background =  element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(), 
        plot.margin=grid::unit(c(0,0,0,0), "mm"), panel.grid = element_blank()) +
  facet_wrap(.~Año) 

ggsave("FeminicidiosTileMaps.svg", width = 8.5, height = 6, units = "in")
