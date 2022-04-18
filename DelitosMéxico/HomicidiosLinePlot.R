library(dplyr)
library(stringi)
library(ggplot2)
library(svglite)
library(reshape2)
library(MetBrewer)

windowsFonts(PT=windowsFont("PT Sans"))
setwd("D:/ProyectosRandom/DelitosMéxico")

datos17 <- read.csv("IDEFC_jun2021.csv") %>% filter(AÑO >= 1998 &AÑO <= 2015)

datos17$MODALIDAD[datos17$MODALIDAD == "HOMICIDIOS" 
                  & datos17$TIPO == "CULPOSOS"] <- "Homicidios culposos"
datos17$MODALIDAD[datos17$MODALIDAD == "HOMICIDIOS" & 
                    datos17$TIPO == "DOLOSOS"] <- "Homicidios dolosos"

datos17 <- datos17 %>% filter(MODALIDAD == "Homicidios culposos" | MODALIDAD == "Homicidios dolosos")

datos17 <- datos17 %>% select(AÑO, MODALIDAD, ENERO, FEBRERO, MARZO, ABRIL, MAYO, JUNIO, 
                              JULIO, AGOSTO, SEPTIEMBRE, OCTUBRE, NOVIEMBRE, DICIEMBRE)  %>%
  melt(c("AÑO", "MODALIDAD"), value.name = "Valor", variable.name = "Mes") %>% 
  group_by(AÑO, MODALIDAD) %>% summarise(Valor = sum(Valor)) %>% as.data.frame()

datos17$MODALIDAD <- factor(datos17$MODALIDAD, c("Homicidios culposos", "Homicidios dolosos"))

datos22 <- read.csv("IDEFC_NM_feb22.csv") %>% filter(Año >= 2016)

datos22$Tipo.de.delito[datos22$Subtipo.de.delito == "Homicidio culposo"] <- "Homicidios culposos"
datos22$Tipo.de.delito[datos22$Subtipo.de.delito == "Homicidio doloso"] <- "Homicidios dolosos"

datos22 <- datos22 %>% filter(Tipo.de.delito == "Homicidios culposos" | Tipo.de.delito == "Homicidios dolosos")

datos22 <- datos22 %>% select(Año, Tipo.de.delito, Enero, Febrero, Marzo, Abril, Mayo,
                              Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre) %>%
  melt(c("Año", "Tipo.de.delito"), value.name = "Valor", variable.name = "Mes" )%>% 
  group_by(Año, Tipo.de.delito) %>% summarise(Valor = sum(Valor)) %>% na.omit() %>% as.data.frame()

names(datos22)[names(datos22) == "Tipo.de.delito"] <- "Delito"
names(datos17) <- names(datos22)

datos <- rbind(datos17, datos22)

datos %>% group_by(x = ceiling(row_number()/6), Delito) %>% 
  summarise(Valor = sum(Valor), Periodo = paste(min(Año),"\n", max(Año), sep = "")) %>% ggplot() + 
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 0, yend = 0), color = "#f5f5f5") +
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 20000, yend = 20000), color = "#f5f5f5") +
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 40000, yend = 40000), color = "#f5f5f5") +
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 60000, yend = 60000), color = "#f5f5f5") +
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 80000, yend = 80000), color = "#f5f5f5") +
  geom_segment(aes(x = "1998\n2000", xend = "2019\n2021", y = 100000, yend = 100000), color = "#f5f5f5") +
  geom_vline(aes(xintercept = c("1998\n2000")), color = "#ededed") + geom_vline(aes(xintercept = "2001\n2003"), color = "#ededed") +
  geom_vline(aes(xintercept = "2004\n2006"), color = "#ededed") + geom_vline(aes(xintercept = "2007\n2009"), color = "#ededed") +
  geom_vline(aes(xintercept = "2010\n2012"), color = "#ededed") + geom_vline(aes(xintercept = "2013\n2015"), color = "#ededed") +
  geom_vline(aes(xintercept = "2016\n2018"), color = "#ededed") + geom_vline(aes(xintercept = "2019\n2021"), color = "#ededed") +
  scale_y_continuous(limits = c(0,100000), breaks = c(0, 20000, 40000, 60000, 80000, 100000), 
                     labels = c("0", "20K", "40K", "60K", "80K", "100K")) +
  scale_x_discrete(expand = expansion(0, 0.1), position = "bottom") +
  geom_line(aes(x = Periodo, y = Valor, group = Delito, color = Delito), size = 0.75) +
  geom_point(aes(x = Periodo, y = Valor, color = Delito), size = 3) +
  geom_text(aes(x = Periodo, y = Valor, label = Valor)) + 
  scale_color_manual(values = met.brewer("Hokusai1", 5)[c(4,2)]) +
  theme(panel.background = element_blank(), axis.title = element_blank(), legend.position = "none",
        axis.ticks = element_blank(), axis.text = element_text(family = "PT"),
        panel.grid = element_blank())

ggsave("HomicidiosLineScatterPlot.svg", width = 8.5, height = 2, units = "in")
