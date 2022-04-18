library(dplyr)
library(stringi)
library(ggplot2)
library(svglite)
library(reshape2)
library(MetBrewer)
library(patchwork)

windowsFonts(PT=windowsFont("PT Sans"))
setwd("D:/ProyectosRandom/DelitosMéxico")

datos17 <- read.csv("IDEFC_jun2021.csv") %>% filter(AÑO >= 1998 &AÑO <= 2015)

datos17$MODALIDAD[datos17$MODALIDAD == "HOMICIDIOS" 
                  & datos17$TIPO == "CULPOSOS"] <- "Homicidios culposos"
datos17$MODALIDAD[datos17$MODALIDAD == "HOMICIDIOS" & 
                    datos17$TIPO == "DOLOSOS"] <- "Homicidios dolosos"

datos17 <- datos17 %>% filter(MODALIDAD == "Homicidios culposos" | MODALIDAD == "Homicidios dolosos")

datos17 <- datos17 %>% select(AÑO, ENTIDAD, MODALIDAD, ENERO, FEBRERO, MARZO, ABRIL, MAYO, JUNIO, 
                              JULIO, AGOSTO, SEPTIEMBRE, OCTUBRE, NOVIEMBRE, DICIEMBRE)  %>%
  melt(c("AÑO", "ENTIDAD", "MODALIDAD"), value.name = "Valor", variable.name = "Mes") %>% 
  group_by(AÑO, ENTIDAD, MODALIDAD) %>% summarise(Valor = sum(Valor)) %>% as.data.frame()

datos17$MODALIDAD <- factor(datos17$MODALIDAD, c("Homicidios culposos", "Homicidios dolosos"))

datos22 <- read.csv("IDEFC_NM_feb22.csv") %>% filter(Año >= 2016)

datos22$Tipo.de.delito[datos22$Subtipo.de.delito == "Homicidio culposo"] <- "Homicidios culposos"
datos22$Tipo.de.delito[datos22$Subtipo.de.delito == "Homicidio doloso"] <- "Homicidios dolosos"

datos22 <- datos22 %>% filter(Tipo.de.delito == "Homicidios culposos" | Tipo.de.delito == "Homicidios dolosos")

datos22 <- datos22 %>% select(Año, Entidad, Tipo.de.delito, Enero, Febrero, Marzo, Abril, Mayo,
                              Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre) %>%
  melt(c("Año", "Entidad", "Tipo.de.delito"), value.name = "Valor", variable.name = "Mes" )%>% 
  group_by(Año, Entidad, Tipo.de.delito) %>% summarise(Valor = sum(Valor)) %>% na.omit() %>% as.data.frame()

names(datos22)[names(datos22) == "Tipo.de.delito"] <- "Delito"
names(datos17) <- names(datos22)

datos17$Entidad <- stri_replace_all_regex(datos17$Entidad,
                                          pattern = c("BAJA CALIFORNIA SUR"),
                                          replacement = c("BS"),
                                          vectorize = F)
datos22$Entidad <- stri_replace_all_regex(datos22$Entidad,
                                          pattern = c("Baja California Sur"),
                                          replacement = c("BS"),
                                          vectorize = F)

datos17$Entidad2 <- stri_replace_all_regex(datos17$Entidad,
                                          pattern = sort(unique(datos17$Entidad)),
                                          replacement = c("AG", "BC", "BS", "CM", "CS", "CH", "CX", "CO",
                                                          "CL", "DG", "GT", "GR", "HG", "JC", "EM", "MI",
                                                          "MO", "NY", "NL", "OA", "PU", "QT", "QR", "SL",
                                                          "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA"),
                                          vectorize = F)

datos22$Entidad2 <- stri_replace_all_regex(datos22$Entidad,
                                          pattern = sort(unique(datos22$Entidad)),
                                          replacement = c("AG", "BC", "BS", "CM", "CS", "CH", "CX", "CO",
                                                          "CL", "DG", "GT", "GR", "HG", "JC", "EM", "MI",
                                                          "MO", "NY", "NL", "OA", "PU", "QT", "QR", "SL",
                                                          "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA"),
                                          vectorize = F)

datos <- rbind(datos17, datos22)
posiciones <- read.csv("Entidades.csv")

datos2 <- datos %>% group_by(Año, Entidad2) %>% summarise(Valor = sum(Valor)) %>% 
  arrange(Entidad2) %>% group_by(i = ceiling(row_number()/3), Entidad2) %>% 
  summarise(Valor = sum(Valor), Periodo = paste(min(Año),"/", max(Año), sep = "")) %>%
  as.data.frame() %>%  inner_join(posiciones, by = "Entidad2")

entidades <- unique(datos$Entidad2)

datos2 %>% ggplot(aes(x = XE, y = YE, fill = Valor)) + 
  geom_tile(aes(width = 0.9, height = 0.9))  +
  scale_fill_gradientn(colors = met.brewer("Hokusai1", 5)[4:1], limits = c(0, 20000)) +
  geom_text(aes(x= XE, y = YE, label = Entidad2), color = "#ffffff", family = "PT", size = 3.5) + 
  #geom_text(aes(x= XE, y = YE-0.2, label = Valor), color = "#ffffff", family = "PT") +
  theme(panel.background =  element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(), 
        plot.margin=grid::unit(c(0,0,0,0), "mm"), panel.grid = element_blank()) +
  facet_wrap(.~Periodo) 

ggsave("HomicidiosTileMaps.svg", width = 8.5, height = 6, units = "in")
