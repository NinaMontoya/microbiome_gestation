
#Verificar ruta de trabajo
getwd()

#Seleccionar carpeta de trabajo
setwd("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/microbiome_gestation/")

#########DIVERSIDAD ALFA######

#otros otu tables
#gestacion.total<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_filtered_bacteria/table_filtered_bacteria.csv", row.names = 1, check.names = FALSE)
#gestacion.sin.estomago<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_filtered_bacteria/otu.sin.estomago.csv", row.names = 1, check.names = FALSE)
#metadata completo
#mapa.gestacion <- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/metadata.gestacion.txt", check.names = F)

library(tidyverse)
library(hillR)
library(hilldiv)
library(ggplot2)
library(ggpubr)

#RECTO
#otu de recto
gestacion.recto<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("gestacion/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("gestacion/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl")))


#INDICES DE DIVERSIDAD

#Calcular orden de diversidad q0, q1, q2
hill_div(gestacion.recto.filtrada, 0)
hill_div(gestacion.recto.filtrada, 1)
hill_div(gestacion.recto.filtrada, 2)

#Graficar indices de diversidad 
#Obtener indices de diversidad por cada orden q de la otu completa
q0.gestacion.recto<- hill_div(gestacion.recto.filtrada,0) %>% as.data.frame() %>% mutate(orden="q=0") %>% rownames_to_column(var = "OTUID")
q1.gestacion.recto<- hill_div(gestacion.recto.filtrada,1) %>% as.data.frame() %>% mutate(orden="q=1") %>% rownames_to_column(var = "OTUID")
q2.gestacion.recto<- hill_div(gestacion.recto.filtrada,2) %>% as.data.frame() %>% mutate(orden="q=2") %>% rownames_to_column(var = "OTUID")

#Unir indices de diversidad con el metadata
alfa_gestacion.recto <- rbind(q0.gestacion.recto,q1.gestacion.recto,q2.gestacion.recto) %>% inner_join(metadata.gestacion.recto, joining, by = "OTUID")
colnames(alfa_gestacion.recto)[2]<- "Numero efectivo de ASV´s"

#Subset para obtener los datos por orden 
alfa.recto.q0 <- subset(alfa_gestacion.recto, orden=="q=0")
alfa.recto.q1 <- subset(alfa_gestacion.recto, orden=="q=1")
alfa.recto.q2 <- subset(alfa_gestacion.recto, orden=="q=2")


#Especificar el orden en el que quiero que grafique 
alfa.recto.q0$orden<- factor(alfa.recto.q0$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


#Especificar el orden en el que quiero que grafique 
alfa.recto.q1$orden<- factor(alfa.recto.q1$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


#Especificar el orden en el que quiero que grafique 
alfa.recto.q2$orden<- factor(alfa.recto.q2$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


colores.alfa1 <- c("#673dff", "#af6502","#b38bff","#e49007")
colores.alfa2 <- c("#9265ff", "#ca7a04","#cfb1ff","#fea509")
# "#673dff", lila oscuro++++
# "#9265ff", lila oscuro+++
# "#af6502", orange brown++++
# "#ca7a04", orange brown+++
# "#b38bff", lila claro++++
# "#cfb1ff", lila claro+++
# "#e49007", amarillo claro++++
# "#fea509", amarillo claro+++


#Generar gráfico 
#figura por seccion a orden q0, si quiero graficar sexo, cambiar fill
recto.alfa.q0<- ggboxplot(data = alfa.recto.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007"))+
 theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=0 (Riqueza de especies)")

recto.alfa.q0
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#figura por seccion a orden q1
recto.alfa.q1<- ggboxplot(data = alfa.recto.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (Especies típicas)")

recto.alfa.q1

ggsave("recto.alfa.q1.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q1, device = "pdf")


#figura por seccion a orden q2
recto.alfa.q2<- ggboxplot(data = alfa.recto.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (Especies dominantes)")

recto.alfa.q2
ggsave("recto.alfa.q2.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q2 , device = "pdf")



#GRAFICA POR ALTITUD 
#figura a orden q0 por poblacion (2600 y 4150 msnm)
#q0
recto.pob.alfa.q0<- ggboxplot(data = alfa.recto.q0, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

recto.pob.alfa.q0
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#Gráfica por altitud 
#figura a orden q1 por poblacion (2600 y 4150 msnm)
#q1
recto.pob.alfa.q1<- ggboxplot(data = alfa.recto.q1, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

recto.pob.alfa.q1
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#Gráfica por altitud 
#figura a orden q2 por poblacion (2600 y 4150 msnm)
#q2
recto.pob.alfa.q2<- ggboxplot(data = alfa.recto.q2, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

recto.pob.alfa.q2
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.recto.poblacion<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  recto.pob.alfa.q0+theme(legend.title = element_blank(),
                    legend.text = element_blank(),
                    legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 7),
                    #legend.box.spacing = unit(0, "pt"),
                    #legend.spacing.x = unit(0, "pt"),
                    plot.margin =unit(c(0,0,0.4,-0.25), "cm"))+ylab(""),
  recto.pob.alfa.q1+theme(legend.title = element_blank(),
                    legend.text = element_blank(),
                    legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 7),
                    plot.margin =unit(c(0,0,0.4,-0.2), "cm"), 
                    axis.title.y = element_text(vjust = 0.3, size = 9)),
  recto.pob.alfa.q2+theme(legend.title = element_blank(),
                    legend.position = "bottom",
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 7),
                    legend.text = element_text(size = 7),
                    legend.box.spacing = unit(0.5, "pt"),
                    plot.margin =unit(c(0,0,-0.4,-0.1), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.recto.poblacion

ggsave("boxplot.recto.poblacion.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.recto.poblacion , device = "pdf")


#GRAFICA POR TEMPORADA
#figura a orden q0 por temporada (reproductiva y no reproductiva)
#q0
recto.tem.alfa.q0<- ggboxplot(data = alfa.recto.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

recto.tem.alfa.q0
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#q1
recto.tem.alfa.q1<- ggboxplot(data = alfa.recto.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+ #"#0E6251"
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

recto.tem.alfa.q1
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#q2
recto.tem.alfa.q2<- ggboxplot(data = alfa.recto.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

recto.tem.alfa.q2
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.recto.temporada<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  recto.tem.alfa.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,0), "cm"))+ylab(""),
  recto.tem.alfa.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0.1), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 9)),
  recto.tem.alfa.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0.15), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.recto.temporada

ggsave("boxplot.recto.temporada.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.recto.temporada , device = "pdf")


#GRAFICA POR SEXO
#figura a sexo q0 por sexo (hembra y macho)
#q0
recto.sex.alfa.q0<- ggboxplot(data = alfa.recto.q0, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

recto.sex.alfa.q0
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#q1
recto.sex.alfa.q1<- ggboxplot(data = alfa.recto.q1, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+ #"#0E6251"
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

recto.sex.alfa.q1
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#q2
recto.sex.alfa.q2<- ggboxplot(data = alfa.recto.q2, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

recto.sex.alfa.q2
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.recto.sexo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  recto.sex.alfa.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,0), "cm"))+ylab(""),
  recto.sex.alfa.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0.1), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 9)),
  recto.sex.alfa.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0.15), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.recto.sexo

ggsave("boxplot.recto.sexo.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.recto.sexo, device = "pdf")



#FIGURA POR TEMPORADA-SEXO
#figura a orden q0
recto.temsex.q0<- ggboxplot(data = alfa.recto.q0, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        axis.title = element_blank(),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  xlab("Población") +
  ggtitle("q=0 (ASVs richness)")

recto.temsex.q0
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#figura a orden q1
recto.temsex.q1<- ggboxplot(data = alfa.recto.q1, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (typical ASVs)")

recto.temsex.q1
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#figura a orden q2
recto.temsex.q2<- ggboxplot(data = alfa.recto.q2, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  facet_grid2(~"q=2 (dominant ASVs)")+
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        axis.title = element_blank(),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (dominant ASVs)")

recto.temsex.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.recto.temsex<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  recto.temsex.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          title = element_text(size = 9),
                          strip.text.x = element_text(size = 11),
                          #strip.background = element_rect(fill = "#808000","#1B5E20"),
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,0.48), "cm")), #ylab(""),
  recto.temsex.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          title = element_text(size = 9),
                          strip.text.x = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 11)),
  recto.temsex.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          title = element_text(size = 9),
                          strip.text.x = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 9),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0.55), "cm")), #+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.recto.temsex

ggsave("boxplot.recto.temsex.pdf", width = 4, height = 5.5, dpi = 600, plot = boxplot.recto.temsex, device = "pdf")


##DIVERSIDAD ALFA SOLO MACHOS 
#archivos para hacer subset
alfa.recto.q0 
alfa.recto.q1 
alfa.recto.q2 

#Subset por orden de machos 
machos.recto.q0 <- subset(alfa.recto.q0, sexo=="Male")
machos.recto.q1 <- subset(alfa.recto.q1, sexo=="Male")
machos.recto.q2 <- subset(alfa.recto.q2, sexo=="Male")


#figura a orden q0
recto.machos.q0<- ggboxplot(data = machos.recto.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
    title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
    axis.title = element_blank(),
    legend.position = "top", 
    strip.text.x = element_text(size=10, colour = "black"),
    legend.title = element_blank(), 
    axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  xlab("Población") +
  ggtitle("q=0 (ASVs richness)")

recto.machos.q0

#figura a orden q1
recto.machos.q1<- ggboxplot(data = machos.recto.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (typical ASVs)")

recto.machos.q1

#figura a orden q2
recto.machos.q2<- ggboxplot(data = machos.recto.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_grid2(~"q=2 (dominant ASVs)")+
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
    title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
    axis.title = element_blank(),
    legend.position = "top", 
    strip.text.x = element_text(size=10, colour = "black"),
    legend.title = element_blank(), 
    axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (dominant ASVs)")

recto.machos.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.recto.machos<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  recto.machos.q0+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 9),
                        strip.text.x = element_text(size = 11),
                        #strip.background = element_rect(fill = "#808000","#1B5E20"),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        #legend.box.spacing = unit(0, "pt"),
                        #legend.spacing.x = unit(0, "pt"),
                        plot.margin =unit(c(0,0,0.4,0.48), "cm")), #ylab(""),
  recto.machos.q1+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 9),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        plot.margin =unit(c(0,0,0.4,0), "cm"), 
                        axis.title.y = element_text(vjust = 0.3, size = 11)),
  recto.machos.q2+theme(legend.title = element_blank(),
                        legend.position = "bottom",
                        title = element_text(size = 9),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        legend.text = element_text(size = 9),
                        legend.box.spacing = unit(0, "pt"),
                        plot.margin =unit(c(0,0,-0.4,0.55), "cm")), #+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.recto.machos

ggsave("boxplot.recto.machos.pdf", width = 4, height = 5.5, dpi = 600, plot = boxplot.recto.machos, device = "pdf")

#Analisis estadistico
library(coin)
library(Matrix)
library(lme4)
library(nlme)
#cambiar nombre de columna
colnames(machos.recto.q0)[2]<- "q0"
colnames(machos.recto.q1)[2]<- "q1"
colnames(machos.recto.q2)[2]<- "q2"
#test
#wilcox_test(q0 ~ poblacion, data= machos.recto.q0)

#Calcular normalidad de q0 tomando en cuenta solo el Ileon 
plot(density(machos.recto.q0$q0))
shapiro.test(machos.recto.q0$q0)

#Subset para obtner machos por poblacion a diferente orden 
machos26.recto.q0 <- subset(machos.recto.q0, poblacion=="2600 masl")
machos26.recto.q1 <- subset(machos.recto.q1, poblacion=="2600 masl")
machos26.recto.q2 <- subset(machos.recto.q2, poblacion=="2600 masl")

#Subset para obtner machos por poblacion a diferente orden 
machos41.recto.q0 <- subset(machos.recto.q0, poblacion=="4150 masl")
machos41.recto.q1 <- subset(machos.recto.q1, poblacion=="4150 masl")
machos41.recto.q2 <- subset(machos.recto.q2, poblacion=="4150 masl")

#MODELOS POR POBLACION (2600) comparando machos reproductivos vs no reproductivos 
#modelo a q0
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q0.lme.machos26<-lme(q0 ~ temporada, random=~1 |ano, data = machos26.recto.q0) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q0<-anova(q0.lme.machos26) 
anova_machos26.q0

#modelo a q1
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q1.lme.machos26<-lme(q1 ~ temporada, random=~1 |ano, data = machos26.recto.q1) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q1<-anova(q1.lme.machos26) 
anova_machos26.q1

#modelo a q2
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q2.lme.machos26<-lme(q2~ temporada, random=~1 |ano, data = machos26.recto.q2) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q2<-anova(q2.lme.machos26) 
anova_machos26.q2



#MODELOS POR POBLACION (4150) comparando machos reproductivos vs no reproductivos 
#modelo a q0
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q0.lme.machos41<-lme(q0 ~ temporada, random=~1 |ano, data = machos41.recto.q0) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q0<-anova(q0.lme.machos41) 
anova_machos41.q0

#modelo a q1
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q1.lme.machos41<-lme(q1 ~ temporada, random=~1 |ano, data = machos41.recto.q1) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q1<-anova(q1.lme.machos41) 
anova_machos41.q1

#modelo a q2
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q2.lme.machos41<-lme(q2~ temporada, random=~1 |ano, data = machos41.recto.q2) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q2<-anova(q2.lme.machos41) 
anova_machos41.q2



###ILEON
#otu de ILEON
gestacion.ileon<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("gestacion/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl")))


#INDICES DE DIVERSIDAD

#Calcular orden de diversidad q0, q1, q2
hill_div(gestacion.ileon.filtrada, 0)
hill_div(gestacion.ileon.filtrada, 1)
hill_div(gestacion.ileon.filtrada, 2)

#Graficar indices de diversidad 

#Obtener indices de diversidad por cada orden q de la otu completa
q0.gestacion.ileon<- hill_div(gestacion.ileon.filtrada,0) %>% as.data.frame() %>% mutate(orden="q=0") %>% rownames_to_column(var = "OTUID")
q1.gestacion.ileon<- hill_div(gestacion.ileon.filtrada,1) %>% as.data.frame() %>% mutate(orden="q=1") %>% rownames_to_column(var = "OTUID")
q2.gestacion.ileon<- hill_div(gestacion.ileon.filtrada,2) %>% as.data.frame() %>% mutate(orden="q=2") %>% rownames_to_column(var = "OTUID")

#Unir indices de diversidad con el metadata
alfa_gestacion.ileon <- rbind(q0.gestacion.ileon,q1.gestacion.ileon,q2.gestacion.ileon) %>% inner_join(metadata.gestacion.ileon, joining, by = "OTUID")
colnames(alfa_gestacion.ileon)[2]<- "Numero efectivo de ASV´s"

#Subset para obtener los datos por orden 
alfa.ileon.q0 <- subset(alfa_gestacion.ileon, orden=="q=0")
alfa.ileon.q1 <- subset(alfa_gestacion.ileon, orden=="q=1")
alfa.ileon.q2 <- subset(alfa_gestacion.ileon, orden=="q=2")

#Especificar el orden en el que quiero que grafique 
alfa.ileon.q0$orden<- factor(alfa.ileon.q0$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


#Especificar el orden en el que quiero que grafique 
alfa.ileon.q1$orden<- factor(alfa.ileon.q1$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


#Especificar el orden en el que quiero que grafique 
alfa.ileon.q2$orden<- factor(alfa.ileon.q2$Sexo_temporada, levels=c("Hembra Reproductiva","Macho Reproductivo",
                                                                    "Hembra No Reproductiva","Macho No Reproductivo"))


#Generar gráfico 
#figura por seccion a orden q0, si quiero graficar sexo, cambiar fill
ileon.alfa.q0<- ggboxplot(data = alfa.ileon.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=0 (Riqueza de especies)")

ileon.alfa.q0
#ggsave("ileon.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = ileon.alfa.q0, device = "pdf")


#figura por seccion a orden q1
ileon.alfa.q1<- ggboxplot(data = alfa.ileon.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (Especies típicas)")

ileon.alfa.q1
ggsave("ileon.alfa.q1.pdf", width = 8, height = 4, dpi = 600, plot = ileon.alfa.q1, device = "pdf")


#figura por seccion a orden q2
ileon.alfa.q2 <- ggboxplot(data = alfa.ileon.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "orden", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#5d3277","#af6502","#b38bff","#e49007")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), 
        legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (Especies dominantes)")

ileon.alfa.q2
ggsave("ileon.alfa.q2.pdf", width = 8, height = 4, dpi = 600, plot = ileon.alfa.q2, device = "pdf")



#GRAFICA POR ALTITUD
#figura a orden q0 por poblacion (2600 y 4150 msnm)
#q0
ileon.pob.alfa.q0<- ggboxplot(data = alfa.ileon.q0, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

ileon.pob.alfa.q0
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#figura a orden q1 por poblacion (2600 y 4150 msnm)
#q1
ileon.pob.alfa.q1<- ggboxplot(data = alfa.ileon.q1, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

ileon.pob.alfa.q1
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#figura a orden q2 por poblacion (2600 y 4150 msnm)
#q2
ileon.pob.alfa.q2<- ggboxplot(data = alfa.ileon.q2, x= "poblacion", y="Numero efectivo de ASV´s", fill = "poblacion") + 
  scale_fill_manual(values = c("#676778","#D9D9C2"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

ileon.pob.alfa.q2
ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.ileon.poblacion<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  ileon.pob.alfa.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,-0.1), "cm"))+ylab(""),
  ileon.pob.alfa.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0.1), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 9)),
  ileon.pob.alfa.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.ileon.poblacion

ggsave("boxplot.ileon.poblacion.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.ileon.poblacion , device = "pdf")



#GRAFICA POR TEMPORADA
#figura a orden q0 por temporada (reproductiva y no reproductiva)
#q0
ileon.tem.alfa.q0<- ggboxplot(data = alfa.ileon.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

ileon.tem.alfa.q0
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#Gráfica por altitud 
#figura a orden q1 por poblacion (2600 y 4150 msnm)
#q1
ileon.tem.alfa.q1<- ggboxplot(data = alfa.ileon.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+ #"#0E6251"
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

ileon.tem.alfa.q1
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#GRÁFICA POR TEMPORADA 
#figura a orden q2 por temporada
#q2
ileon.tem.alfa.q2<- ggboxplot(data = alfa.ileon.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada") + 
  scale_fill_manual(values = c("#808000","#1B5E20"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

ileon.tem.alfa.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.ileon.temporada<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  ileon.tem.alfa.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,-0.1), "cm"))+ylab(""),
  ileon.tem.alfa.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0.1), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 9)),
  ileon.tem.alfa.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.ileon.temporada

ggsave("boxplot.ileon.temporada.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.ileon.temporada, device = "pdf")


#GRAFICA POR SEXO
#figura a sexo q0 por sexo (hembra y macho)
#q0
ileon.sex.alfa.q0<- ggboxplot(data = alfa.ileon.q0, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_bw() +
  facet_wrap(~"q=0 (ASVs richness)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's")

ileon.sex.alfa.q0
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#q1
ileon.sex.alfa.q1<- ggboxplot(data = alfa.ileon.q1, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+ #"#0E6251"
  theme_bw() +
  facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Effective number of ASV's")

ileon.sex.alfa.q1
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#q2
ileon.sex.alfa.q2<- ggboxplot(data = alfa.ileon.q2, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_bw() +
  facet_wrap(~"q=2 (dominant ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 9, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") 

recto.sex.alfa.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.ileon.sexo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  ileon.sex.alfa.q0+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          #legend.box.spacing = unit(0, "pt"),
                          #legend.spacing.x = unit(0, "pt"),
                          plot.margin =unit(c(0,0,0.4,0), "cm"))+ylab(""),
  ileon.sex.alfa.q1+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0.4,0.25), "cm"), 
                          axis.title.y = element_text(vjust = 0.3, size = 9)),
  ileon.sex.alfa.q2+theme(legend.title = element_blank(),
                          legend.position = "bottom",
                          axis.text.x = element_blank(),
                          axis.text.y = element_text(size = 7),
                          legend.text = element_text(size = 7),
                          legend.box.spacing = unit(0, "pt"),
                          plot.margin =unit(c(0,0,-0.4,0.15), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.ileon.sexo

ggsave("boxplot.ileon.sexo.pdf", width = 3, height = 5, dpi = 600, plot = boxplot.ileon.sexo, device = "pdf")


#FIGURA POR TEMPORADA-SEXO
#figura a orden q0
ileon.temsex.q0<- ggboxplot(data = alfa.ileon.q0, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        axis.title = element_blank(),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=0 (ASVs richness)")

ileon.temsex.q0
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#figura a orden q1
ileon.temsex.q1<- ggboxplot(data = alfa.ileon.q1, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (typical ASVs)")

ileon.temsex.q1
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")

#figura a orden q2
ileon.temsex.q2<- ggboxplot(data = alfa.ileon.q2, x= "sexo", y="Numero efectivo de ASV´s", fill = "sexo", facet.by = "temporada") + 
  scale_fill_manual(values = c("#5D3277","#AF6502"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        axis.title = element_blank(),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (dominant ASVs)")

ileon.temsex.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.ileon.temsex<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  ileon.temsex.q0+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 10),
                        #strip.background = element_rect(fill = "#808000","#1B5E20"),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        #legend.box.spacing = unit(0, "pt"),
                        #legend.spacing.x = unit(0, "pt"),
                        plot.margin =unit(c(0,0,0.2,0.39), "cm"))+ylab(""),
  ileon.temsex.q1+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 10),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        plot.margin =unit(c(0,0,0.4,0), "cm"), 
                        axis.title.y = element_text(vjust = 0.3, size = 11)),
  ileon.temsex.q2+theme(legend.title = element_blank(),
                        legend.position = "bottom",
                        title = element_text(size = 10),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        legend.text = element_text(size = 9),
                        legend.box.spacing = unit(0, "pt"),
                        plot.margin =unit(c(0,0,-0.4,0.51), "cm"))+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.ileon.temsex

ggsave("boxplot.ileon.temsex.pdf", width = 4, height = 5.5, dpi = 600, plot = boxplot.ileon.temsex, device = "pdf")



##DIVERSIDAD ALFA SOLO MACHOS 
#archivos para hacer subset
alfa.ileon.q0 
alfa.ileon.q1 
alfa.ileon.q2 

#Subset por orden de machos 
machos.ileon.q0 <- subset(alfa.ileon.q0, sexo=="Male")
machos.ileon.q1 <- subset(alfa.ileon.q1, sexo=="Male")
machos.ileon.q2 <- subset(alfa.ileon.q2, sexo=="Male")


#figura a orden q0
ileon.machos.q0<- ggboxplot(data = machos.ileon.q0, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
    title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
    axis.title = element_blank(),
    legend.position = "top", 
    strip.text.x = element_text(size=10, colour = "black"),
    legend.title = element_blank(), 
    axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  xlab("Población") +
  ggtitle("q=0 (ASVs richness)")

ileon.machos.q0

#figura a orden q1
ileon.machos.q1<- ggboxplot(data = machos.ileon.q1, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_wrap(~"q=1 (typical ASVs)", scales = "free", dir = "v")+
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
        legend.position = "top", 
        strip.text.x = element_text(size=10, colour = "black"),
        legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=1 (typical ASVs)")

ileon.machos.q1

#figura a orden q2
ileon.machos.q2<- ggboxplot(data = machos.ileon.q2, x= "temporada", y="Numero efectivo de ASV´s", fill = "temporada", facet.by = "poblacion") + 
  scale_fill_manual(values = c("#AF6502","#FEA509"))+
  theme_grey() +
  ggtitle("grey()") +
  #facet_grid2(~"q=2 (dominant ASVs)")+
  theme(#axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
    title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"),
    axis.title = element_blank(),
    legend.position = "top", 
    strip.text.x = element_text(size=10, colour = "black"),
    legend.title = element_blank(), 
    axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número efectivo de ASV's") +
  #xlab("Población") +
  ggtitle("q=2 (dominant ASVs)")

ileon.machos.q2
#ggsave("recto.alfa.q0.pdf", width = 8, height = 4, dpi = 600, plot = recto.alfa.q0 , device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
boxplot.ileon.machos<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  ileon.machos.q0+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 9),
                        strip.text.x = element_text(size = 11),
                        #strip.background = element_rect(fill = "#808000","#1B5E20"),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        #legend.box.spacing = unit(0, "pt"),
                        #legend.spacing.x = unit(0, "pt"),
                        plot.margin =unit(c(0,0,0.4,0.48), "cm")), #ylab(""),
  ileon.machos.q1+theme(legend.title = element_blank(),
                        legend.text = element_blank(),
                        legend.position = "none",
                        title = element_text(size = 9),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        plot.margin =unit(c(0,0,0.4,0), "cm"), 
                        axis.title.y = element_text(vjust = 0.3, size = 11)),
  ileon.machos.q2+theme(legend.title = element_blank(),
                        legend.position = "bottom",
                        title = element_text(size = 9),
                        strip.text.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_text(size = 7),
                        legend.text = element_text(size = 9),
                        legend.box.spacing = unit(0, "pt"),
                        plot.margin =unit(c(0,0,-0.4,0.55), "cm")), #+ylab(""),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

boxplot.ileon.machos

#ggsave("boxplot.ileon.machos.pdf", width = 4, height = 5.5, dpi = 600, plot = boxplot.ileon.machos, device = "pdf")

#Analisis estadistico
library(coin)
library(Matrix)
library(lme4)
library(nlme)
#cambiar nombre de columna
colnames(machos.ileon.q0)[2]<- "q0"
colnames(machos.ileon.q1)[2]<- "q1"
colnames(machos.ileon.q2)[2]<- "q2"
#test
#wilcox_test(q0 ~ poblacion, data= machos.recto.q0)

#Calcular normalidad de q0 tomando en cuenta solo el Ileon 
plot(density(machos.ileon.q0$q0))
shapiro.test(machos.ileon.q0$q0)

#Subset para obtner machos por poblacion a diferente orden 
machos26.ileon.q0 <- subset(machos.ileon.q0, poblacion=="2600 masl")
machos26.ileon.q1 <- subset(machos.ileon.q1, poblacion=="2600 masl")
machos26.ileon.q2 <- subset(machos.ileon.q2, poblacion=="2600 masl")

#Subset para obtner machos por poblacion a diferente orden 
machos41.ileon.q0 <- subset(machos.ileon.q0, poblacion=="4150 masl")
machos41.ileon.q1 <- subset(machos.ileon.q1, poblacion=="4150 masl")
machos41.ileon.q2 <- subset(machos.ileon.q2, poblacion=="4150 masl")

#MODELOS POR POBLACION (2600) comparando machos reproductivos vs no reproductivos 
#modelo a q0
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q0.lme.machos26.ileon<-lme(q0 ~ temporada, random=~1 |ano, data = machos26.ileon.q0) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q0.ileon<-anova(q0.lme.machos26.ileon) 
anova_machos26.q0.ileon

#modelo a q1
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q1.lme.machos26.ileon<-lme(q1 ~ temporada, random=~1 |ano, data = machos26.ileon.q1) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q1.ileon<-anova(q1.lme.machos26.ileon) 
anova_machos26.q1.ileon

#modelo a q2
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q2.lme.machos26.ileon<-lme(q2~ temporada, random=~1 |ano, data = machos26.ileon.q2) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos26.q2.ileon<-anova(q2.lme.machos26.ileon) 
anova_machos26.q2.ileon



#MODELOS POR POBLACION (4150) comparando machos reproductivos vs no reproductivos 
#modelo a q0
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q0.lme.machos41.ileon<-lme(q0 ~ temporada, random=~1 |ano, data = machos41.ileon.q0) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q0.ileon<-anova(q0.lme.machos41.ileon) 
anova_machos41.q0.ileon

#modelo a q1
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q1.lme.machos41.ileon<-lme(q1 ~ temporada, random=~1 |ano, data = machos41.ileon.q1) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q1.ileon<-anova(q1.lme.machos41.ileon) 
anova_machos41.q1.ileon

#modelo a q2
#modelo lineal comparando temporada reproductiva vs no reproductiva, bloqueando por año
q2.lme.machos41.ileon<-lme(q2~ temporada, random=~1 |ano, data = machos41.ileon.q2) 

#Obtener anova del modelo (estadístico y valor p (no permutado))
anova_machos41.q2.ileon<-anova(q2.lme.machos41.ileon) 
anova_machos41.q2.ileon





#######DIVERSIDAD BETA#####
library(tidyverse)
library(ggpubr)
library(hillR)

#otu de recto
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#transponer
otu.gestacion.recto.filtrada <- as.data.frame(t(gestacion.recto.filtrada))

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto")


#Correr diversidad beta en términos de números de Hill
gestacion.beta.q0 <- hill_taxa_parti_pairwise(comm = otu.gestacion.recto.filtrada, q = 0)
gestacion.beta.q1 <- hill_taxa_parti_pairwise(comm = otu.gestacion.recto.filtrada, q = 1)
gestacion.beta.q2 <- hill_taxa_parti_pairwise(comm = otu.gestacion.recto.filtrada, q = 2)

#Agregar columna para obtener recambio del archivo anterior (TD_beta-1)
gestacion.beta.q0.rec <- gestacion.beta.q0 %>% mutate(Recambio = gestacion.beta.q0$TD_beta-1)
gestacion.beta.q1.rec <- gestacion.beta.q1 %>% mutate(Recambio = gestacion.beta.q1$TD_beta-1)
gestacion.beta.q2.rec <- gestacion.beta.q2 %>% mutate(Recambio = gestacion.beta.q2$TD_beta-1)
#metadata<- read.csv("metadata.gestacion.csv")

#Guardar el archivo y exportarlo a tu escritorio 
write_csv(gestacion.beta.q0.rec, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gest.beta.q0.csv")
write_csv(gestacion.beta.q1.rec, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gest.beta.q1.csv")
write_csv(gestacion.beta.q2.rec, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gest.beta.q2.csv")


#FILTRAR TABLA ANTERIOR PARA OBTENER RECAMBIO POR HEMBRAS, MACHOS Y POR CADA POBLACION 

#RECAMBIO A q0 
#Unir tabla con metadata
beta.gest.q0.format<- gestacion.beta.q0.rec %>% inner_join(metadata_gestacion, by = c("site1"="ID")) %>% inner_join(metadata_gestacion, by = c("site2"="ID")) 

#Vector de sexo para filtrar despues
comparacion_sexo<- c("Hembra_vs_Hembra", "Macho_vs_Macho")

#Subset para obtener solo comparacion ileon vs iloen y comparacion macho vs macho y hembra vs hembra.
beta2.gest.ileon.q0 <- beta.gest.q0.format %>% filter(seccion.x=="Ileon", seccion.y=="Ileon") %>%
  unite("compar_sexo", c("sexo.x", "sexo.y"), sep="_vs_", remove=F) %>% 
  unite("compar_temporada", c("temporada.x", "temporada.y"), sep="_vs_", remove=F) %>% 
  unite("compar_poblacion", c("poblacion.x", "poblacion.y"), sep="_vs_", remove=F) %>% 
  filter(compar_sexo %in% comparacion_sexo ) %>% 
  mutate(compar_condiciones = case_when(compar_temporada=="No Reproductiva_vs_Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_No Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_Reproductiva" ~ "Intra",
                                        compar_temporada=="No Reproductiva_vs_No Reproductiva" ~ "Intra"))


#En esta tabla filtrada tenemos las comparaciones de ileon y de macho-macho, hembra-hembra,
#con esta realizar las dos graficas correspondientes a cada prediccion


#PREDICCION 2
#Filtrar solo las comparaciones entre hembras
beta.P2.q0 <- beta2.gest.ileon.q0 %>% filter(compar_sexo=="Hembra_vs_Hembra")

#Graficar recambio de hembras intercondiciones e intracondiciones
#Intracondiciones, tome las comparaciones Reproductiva-Reproductiva, No Reproductiva-No Reproductiva
#Intercondiciones, tome las comparaciones Reproductiva-No Reproductiva, No Reproductiva-Reproductiva
ggboxplot(data = beta.P2.q0, x= "compar_condiciones", y="Recambio", fill = "compar_condiciones", order = c("Inter", "Intra")) + 
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Tasa de recambio relativo de especies") +
  xlab("Condiciones") +
  ggtitle("q=0")


#PREDICCION 3 a orden q0
#Crear columna nueva con las tres condiciones (sexo, temporada, poblacion)
beta.P3.q0<-beta2.gest.ileon.q0 %>% unite("comparacion", c("compar_sexo", "compar_temporada", "compar_poblacion"), remove=F)

#Elegir de esa columna, las comparaciones que necesito (comparaciones intracondiciones, es decir, entre machos y hembras de la misma temporada y poblacion)
comparaciones.interes <- c("Macho_vs_Macho_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm" ,
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm"         )

#Filtrar de la tabla inicial las comparaciones que necesito
beta.P3.q0.figura <- beta.P3.q0 %>% filter(comparacion %in% comparaciones.interes)

#Figura a q0
ggboxplot(data = beta.P3.q0.figura, x= "temporada.x", y="Recambio", fill = "sexo.x", facet.by = "poblacion.x") + 
  scale_fill_manual(values = c("#9CCED9","#BF9445")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Tasa de recambio relativo de especies") +
  xlab("Temporada") +
  ggtitle("q=0")


#RECAMBIO A q1 
#Unir tabla con metadata
beta.gest.q1.format<- gestacion.beta.q1.rec %>% inner_join(metadata_gestacion, by = c("site1"="ID")) %>% inner_join(metadata_gestacion, by = c("site2"="ID")) 

#Vector de sexo para filtrar despues
comparacion_sexo<- c("Hembra_vs_Hembra", "Macho_vs_Macho")

#Subset para obtener solo comparacion ileon vs iloen y comparacion macho vs macho y hembra vs hembra.
beta2.gest.ileon.q1 <- beta.gest.q1.format %>% filter(seccion.x=="Ileon", seccion.y=="Ileon") %>%
  unite("compar_sexo", c("sexo.x", "sexo.y"), sep="_vs_", remove=F) %>% 
  unite("compar_temporada", c("temporada.x", "temporada.y"), sep="_vs_", remove=F) %>% 
  unite("compar_poblacion", c("poblacion.x", "poblacion.y"), sep="_vs_", remove=F) %>% 
  filter(compar_sexo %in% comparacion_sexo ) %>% 
  mutate(compar_condiciones = case_when(compar_temporada=="No Reproductiva_vs_Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_No Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_Reproductiva" ~ "Intra",
                                        compar_temporada=="No Reproductiva_vs_No Reproductiva" ~ "Intra"))


#En esta tabla filtrada tenemos las comparaciones de ileon y de macho-macho, hembra-hembra,
#con esta realizar las dos graficas correspondientes a cada prediccion

#PREDICCION 3 a orden q1
#Crear columna nueva con las tres condiciones (sexo, temporada, poblacion)
beta.P3.q1<-beta2.gest.ileon.q1 %>% unite("comparacion", c("compar_sexo", "compar_temporada", "compar_poblacion"), remove=F)

#Elegir de esa columna, las comparaciones que necesito (comparaciones intracondiciones, es decir, entre machos y hembras de la misma temporada y poblacion)
comparaciones.interes <- c("Macho_vs_Macho_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm" ,
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm"         )

#Filtrar de la tabla inicial las comparaciones que necesito
beta.P3.q1.figura <- beta.P3.q1 %>% filter(comparacion %in% comparaciones.interes)

#Figura a q1
ggboxplot(data = beta.P3.q1.figura, x= "temporada.x", y="Recambio", fill = "sexo.x", facet.by = "poblacion.x") + 
  scale_fill_manual(values = c("#9CCED9","#BF9445")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Tasa de recambio relativo de especies") +
  xlab("Temporada") +
  ggtitle("q=1")


#RECAMBIO A q2 
#Unir tabla con metadata
beta.gest.q2.format<- gestacion.beta.q2.rec %>% inner_join(metadata_gestacion, by = c("site1"="ID")) %>% inner_join(metadata_gestacion, by = c("site2"="ID")) 

#Vector de sexo para filtrar despues
comparacion_sexo<- c("Hembra_vs_Hembra", "Macho_vs_Macho")

#Subset para obtener solo comparacion ileon vs iloen y comparacion macho vs macho y hembra vs hembra.
beta2.gest.ileon.q2 <- beta.gest.q2.format %>% filter(seccion.x=="Ileon", seccion.y=="Ileon") %>%
  unite("compar_sexo", c("sexo.x", "sexo.y"), sep="_vs_", remove=F) %>% 
  unite("compar_temporada", c("temporada.x", "temporada.y"), sep="_vs_", remove=F) %>% 
  unite("compar_poblacion", c("poblacion.x", "poblacion.y"), sep="_vs_", remove=F) %>% 
  filter(compar_sexo %in% comparacion_sexo ) %>% 
  mutate(compar_condiciones = case_when(compar_temporada=="No Reproductiva_vs_Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_No Reproductiva" ~ "Inter",
                                        compar_temporada=="Reproductiva_vs_Reproductiva" ~ "Intra",
                                        compar_temporada=="No Reproductiva_vs_No Reproductiva" ~ "Intra"))


#En esta tabla filtrada tenemos las comparaciones de ileon y de macho-macho, hembra-hembra,
#con esta realizar las dos graficas correspondientes a cada prediccion

#PREDICCION 3 a orden q2
#Crear columna nueva con las tres condiciones (sexo, temporada, poblacion)
beta.P3.q2<-beta2.gest.ileon.q2 %>% unite("comparacion", c("compar_sexo", "compar_temporada", "compar_poblacion"), remove=F)

#Elegir de esa columna, las comparaciones que necesito (comparaciones intracondiciones, es decir, entre machos y hembras de la misma temporada y poblacion)
comparaciones.interes <- c("Macho_vs_Macho_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
                           "Hembra_vs_Hembra_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm",
                           "Macho_vs_Macho_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm" ,
                           "Hembra_vs_Hembra_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm"         )

#Filtrar de la tabla inicial las comparaciones que necesito
beta.P3.q2.figura <- beta.P3.q2 %>% filter(comparacion %in% comparaciones.interes)

#Figura a q2
ggboxplot(data = beta.P3.q2.figura, x= "temporada.x", y="Recambio", fill = "sexo.x", facet.by = "poblacion.x") + 
  scale_fill_manual(values = c("#9CCED9","#BF9445")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Tasa de recambio relativo de especies") +
  xlab("Temporada") +
  ggtitle("q=2")



#OTRA OPCION 
#Dar formato al archivo, filtrando por seccion, sexo, poblacion y temporada
com_sex<-  c("Female_vs_Female", "Male_vs_Male")
gestacion.beta.q0.rec.format<- gestacion.beta.q0.rec %>% mutate(seccion1 = case_when(
  str_detect(.$site1, "I") ~ "Ileon",
  str_detect(.$site1, "R") ~ "Rectum",
  str_detect(.$site1, "S") ~ "Stomach"))%>% mutate(seccion2 = case_when(
    str_detect(.$site2, "I") ~ "Ileon",
    str_detect(.$site2, "R") ~ "Rectum",
    str_detect(.$site2, "S") ~ "Stomach")) %>% mutate(sexo1 = case_when(
      str_detect(.$site1, "F") ~ "Female",
      str_detect(.$site1, "H") ~ "Female",
      str_detect(.$site1, "M") ~ "Male"))%>% mutate(sexo2 = case_when(
        str_detect(.$site2, "F") ~ "Female",
        str_detect(.$site2, "H") ~ "Female",
        str_detect(.$site2, "M") ~ "Male")) %>% unite(
          "compar_seccion",seccion1:seccion2, sep="_vs_") %>% filter( #escoger comparaciones entre secciones
            compar_seccion=="Ileon_vs_Ileon") %>% unite(
              "compar_sexo", sexo1:sexo2, sep="_vs_") %>% filter( #escoger comparaciones entre sexos
              compar_sexo %in% com_sex )%>% inner_join(metadata_gestacion, by = c("site1"="ID")) %>% rename(
                poblacion1=poblacion) %>%   inner_join(metadata_gestacion, by = c("site2"="ID")) %>% rename(
                  poblacion2=poblacion) %>% unite(
                    "compar_poblacion", c("poblacion1", "poblacion2"), sep="_vs_", remove=F)%>% mutate(
                      poblacion = case_when(
                      str_detect(.$compar_poblacion, "2600 msnm_vs_2600 msnm") ~ "baja_vs_baja",
                      str_detect(.$compar_poblacion, "4150 msnm_vs_4150 msnm") ~ "alta_vs_alta",
                      str_detect(.$compar_poblacion, "4150 msnm_vs_2600 msnm") ~ "alta_vs_baja",
                      str_detect(.$compar_poblacion, "2600 msnm_vs_4150 msnm") ~ "baja_vs_alta")) %>% unite(
                        "compar_temp", c("temporada.x", "temporada.y"), 
                        sep="_vs_", remove=F) %>%  mutate(
                          compar_cond = case_when(
                           compar_temp=="No Reproductiva_vs_Reproductiva" ~ "inter",
                           compar_temp== "Reproductiva_vs_No Reproductiva" ~ "inter",
                           compar_temp== "Reproductiva_vs_Reproductiva" ~ "intra",
                           compar_temp=="No Reproductiva_vs_No Reproductiva" ~ "intra"))


#Exportar tabla
write_csv(gestacion.beta.q0.rec.format, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion.beta.q0.rec.formato2.csv")

#Graficar
library(ggpubr)

#PREDICCION 2 
#Filtrar solo las comparaciones entre hembras y posteriormente graficar 
#Graficar recambio de hembras intercondiciones e intracondiciones
#Intracondiciones, tome las comparaciones Reproductiva-Reproductiva, No Reproductiva-No Reproductiva
#Intercondiciones, tome las comparaciones Reproductiva-No Reproductiva, No Reproductiva-Reproductiva
gestacion.beta.q0.rec.format %>% filter(
  compar_sexo=="Female_vs_Female") %>%  ggboxplot(
    x="compar_cond", y="Recambio", add="jitter", fill="compar_cond", 
    order = c("inter", "intra"))


#PREDICCION 3 
#Crear columna nueva con las tres condiciones (sexo, temporada, poblacion)
tab<-gestacion.beta.q0.rec.format %>% unite("compara", c("compar_sexo", "compar_temp", "compar_poblacion"), remove=F)

#Elegir de esa columna, las comparaciones que necesito (comparaciones intracondiciones, es decir, entre machos y hembras de la misma temporada y poblacion)
quiero<- c("Male_vs_Male_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
           "Female_vs_Female_Reproductiva_vs_Reproductiva_2600 msnm_vs_2600 msnm",
           "Female_vs_Female_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
           "Male_vs_Male_Reproductiva_vs_Reproductiva_4150 msnm_vs_4150 msnm",
           "Female_vs_Female_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm",
           "Male_vs_Male_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm",
           "Male_vs_Male_No Reproductiva_vs_No Reproductiva_2600 msnm_vs_2600 msnm" ,
           "Female_vs_Female_No Reproductiva_vs_No Reproductiva_4150 msnm_vs_4150 msnm"         )

#Filtrar de la tabla inicial las comparaciones que necesito
tab_fig<- tab %>% filter(compara %in% quiero)

#Grafica de Recambio a q0
ggboxplot(data = tab_fig, x= "temporada.x", y="Recambio", fill = "sexo.x", facet.by = "poblacion1") + 
  scale_fill_manual(values = c("#9CCED9","#BF9445")) + 
  theme_grey() +
  ggtitle("grey()") +
  theme(axis.title = element_text(size = 12, hjust=0.5, face= "bold", colour = "black"),
        title = element_text(size = 13, hjust= 0.5, face = "bold", colour = "black"), legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Tasa de recambio relativo de especies") +
  xlab("Época") +
  ggtitle("q=0")


######PCA##### 

#Cargar paquetes 
library(tidyverse)
library(MASS)
library(zCompositions)
library(Rfast)
library(ALDEx2)
library(vegan)
library(compositions)
library(FactoMineR)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(dplyr)

#PCA DE TODOS LOS DATOS (8O datos, 10 hembras reproductivas a 2600 y 4150 (20),
                                  #10 hembras no reproductivas a 2600 y 4150 (20),
                                  #10 machos reproductivas a 2600 y 4150 (20),
                                  #10 machoss no reproductivas a 2600 y 4150 (20)


#PCA DE ILEON
#otu de ILEON
gestacion.ileon<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon")

#Cambiar a factor, la variable comparacion
metadata.gestacion.ileon$comparacion <- factor(metadata.gestacion.ileon$comparacion)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable sexo_poblacion
metadata.gestacion.ileon$Sexo_poblacion <- factor(metadata.gestacion.ileon$Sexo_poblacion)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable sexo_temporada
metadata.gestacion.ileon$Sexo_temporada <- factor(metadata.gestacion.ileon$Sexo_temporada)
str(metadata.gestacion.ileon)


#Transformar datos 
#METODO 1
transfor.aldex.gestacion.ileon <- aldex.clr(gestacion.ileon.filtrada, mc.samples = 999, denom ="all", verbose = FALSE, useMC = FALSE)
transfor.aldex.gestacion.ileon.data <- t(getMonteCarloSample(transfor.aldex.gestacion.ileon,1))

#PCA
pca.gestacion.ileon <- prcomp(transfor.aldex.gestacion.ileon.data)

#Definir niveles
PC1.gestacion.ileon <- paste("PC1", round(sum(pca.gestacion.ileon$sdev[1] ^ 2) / mvar(transfor.aldex.gestacion.ileon.data) * 100, 1), "%")
PC2.gestacion.ileon <- paste("PC2", round(sum(pca.gestacion.ileon$sdev[2] ^ 2) / mvar(transfor.aldex.gestacion.ileon.data) * 100, 1), "%")

#Especificar el orden en el que quiero que grafique 
puntos_comparacion.ileon <- data.frame(pca.gestacion.ileon$x) %>% rownames_to_column(var = "OTUID") %>%
  left_join(metadata.gestacion.ileon, by = "OTUID")
puntos_comparacion.ileon$orden<- factor(puntos_comparacion.ileon$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                           "Hembra2600NoRep","Hembra4150NoRep",
                                                                           "Macho2600Rep", "Macho4150Rep",
                                                                           "Macho2600NoRep", "Macho4150NoRep"))

#definir colores
color.orden <- c("#673dff", "#9265ff", "#b38bff", "#cfb1ff","#af6502","#ca7a04","#e49007","#fea509")


# Figura por seccion 
PCA_gestacion.ileon.aldex.clr <- ggplot() +
  theme_bw() +
  xlab(PC1.gestacion.ileon) +
  ylab(PC2.gestacion.ileon) +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 12),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(colour = "black"),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text.x.top = element_text(colour = "black"),
        legend.position = "right", 
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.01, 'cm'),
        #axis.line.x.top = element_line(color = "red", size = 2), 
        #axis.line.y.right =  element_line(color = "black", size = 2),
        panel.border = element_rect(color = "black", size = 1.5),
        legend.title = element_blank()) + 
  geom_point(data = puntos_comparacion.ileon,
             aes(x=PC1, y=PC2, fill=orden), shape=21, size= 4) +
  #geom_text(data = data.frame(pca.transferencia$x) %>% rownames_to_column(var = "ID") %>%
  #left_join(metadata.transf.2600.filter, by = "ID"),
  #aes(x=PC1, y=PC2, fill=Seccion,label= ID)) +
  geom_vline(xintercept = 0, linetype = 3) + #lines-cross 
  geom_hline(yintercept = 0, linetype = 3) +
  scale_fill_manual(values = color.orden)+
  scale_x_continuous(limits = c(-40,40))+
  scale_y_continuous(limits = c(-50,90))
#theme(legend.text = element_text(size = 9))+guides(fill=guide_legend(nrow = 1))

#Observar PCA 
print(PCA_gestacion.ileon.aldex.clr)


# perMANOVA por seccion
permanova.gestacion.ileon <- adonis2(transfor.aldex.gestacion.ileon.data ~ comparacion, data = metadata.gestacion.ileon,
                                 method = "euclidean", permutations = 999)

print(permanova.gestacion.ileon)


#PCA SEXO_TEMPORADA
#Cambiar a factor, la variable sexo_temporada
metadata.gestacion.ileon$Sexo_temporada<- factor(metadata.gestacion.ileon$Sexo_temporada)
str(metadata.gestacion.ileon)

#Transformar datos 
#METODO 1
transfor.aldex.sex_temp.ileon <- aldex.clr(gestacion.ileon.filtrada, mc.samples = 999, denom ="all", verbose = FALSE, useMC = FALSE)
transfor.aldex.sex_temp.ileon.data <- t(getMonteCarloSample(transfor.aldex.sex_temp.ileon,1))

#PCA
pca.sex_temp.ileon <- prcomp(transfor.aldex.sex_temp.ileon.data)

#Definir niveles
PC1.sex_temp.ileon <- paste("PC1", round(sum(pca.sex_temp.ileon$sdev[1] ^ 2) / mvar(transfor.aldex.sex_temp.ileon.data) * 100, 1), "%")
PC2.sex_temp.ileon <- paste("PC2", round(sum(pca.sex_temp.ileon$sdev[2] ^ 2) / mvar(transfor.aldex.sex_temp.ileon.data) * 100, 1), "%")

#definir colores 
color.myorden <- c("#F26B1D", "#F2913D", "#49798C", "#9CCED9")

#Especificar el orden en el que quiero que grafique 
puntos_sex_temp.ileon <- data.frame(pca.sex_temp.ileon$x) %>% rownames_to_column(var = "OTUID") %>%
  left_join(metadata.gestacion.ileon, by = "OTUID")
puntos_sex_temp.ileon$sex.tem<- factor(puntos_sex_temp.ileon$Sexo_temporada, levels=c("Hembra Reproductiva","Hembra No Reproductiva",
                                                                                           "Macho Reproductivo", "Macho No Reproductivo"))

# Figura por seccion 
PCA_sex_temp.ileon.aldex.clr <- ggplot() +
  theme_bw() +
  xlab(PC1.sex_temp.ileon) +
  ylab(PC2.sex_temp.ileon) +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 12),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(colour = "black"),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text.x.top = element_text(colour = "black"),
        legend.position = "right", 
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.01, 'cm'),
        #axis.line.x.top = element_line(color = "red", size = 2), 
        #axis.line.y.right =  element_line(color = "black", size = 2),
        panel.border = element_rect(color = "black", size = 1.5),
        legend.title = element_blank()) + 
  geom_point(data = puntos_sex_temp.ileon,
             aes(x=PC1, y=PC2, color=sex.tem, shape=poblacion), size= 6) +
                  scale_shape_manual(values = c(16,17))+
  #geom_text(data = data.frame(pca.transferencia$x) %>% rownames_to_column(var = "ID") %>%
  #left_join(metadata.transf.2600.filter, by = "ID"),
  #aes(x=PC1, y=PC2, fill=Seccion,label= ID)) +
  #geom_vline(xintercept = 0, linetype = 3) + #lines-cross 
  #geom_hline(yintercept = 0, linetype = 3) +
  scale_color_manual(values = color.myorden)+
  scale_x_continuous(limits = c(-40,40))+
  scale_y_continuous(limits = c(-50,50))
#theme(legend.text = element_text(size = 9))+guides(fill=guide_legend(nrow = 1))

#Observar PCA 
print(PCA_sex_temp.ileon.aldex.clr)


# perMANOVA por seccion
permanova_sex_temp_ileon <- adonis2(transfor.aldex.sex_temp.ileon.data ~ Sexo_temporada, data = metadata.gestacion.ileon,
                                          method = "euclidean", permutations = 999)

print(permanova_sex_temp_ileon)




#PCA DE RECTO

#otu de recto
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto")


#Cambiar a factor, la variable sexo_poblacion
metadata.gestacion.recto$comparacion <- factor(metadata.gestacion.recto$comparacion)
str(metadata.gestacion.recto)

#Transformar datos 
#METODO 1
transfor.aldex.gestacion.recto <- aldex.clr(gestacion.recto.filtrada, mc.samples = 999, denom ="all", verbose = FALSE, useMC = FALSE)
transfor.aldex.gestacion.recto.data <- t(getMonteCarloSample(transfor.aldex.gestacion.recto,1))

#PCA
pca.gestacion.recto <- prcomp(transfor.aldex.gestacion.recto.data)

#Definir niveles
PC1.gestacion.recto <- paste("PC1", round(sum(pca.gestacion.recto$sdev[1] ^ 2) / mvar(transfor.aldex.gestacion.recto.data) * 100, 1), "%")
PC2.gestacion.recto <- paste("PC2", round(sum(pca.gestacion.recto$sdev[2] ^ 2) / mvar(transfor.aldex.gestacion.recto.data) * 100, 1), "%")

#definir colores 
color.orden <- c("#673dff", "#9265ff", "#b38bff", "#cfb1ff","#af6502","#ca7a04","#e49007","#fea509")
#"#9CCED9","#094293","#BF9445", "#f9ec3c"

#Especificar el orden en el que quiero que grafique 
puntos<- data.frame(pca.gestacion.recto$x) %>% rownames_to_column(var = "OTUID") %>%
  left_join(metadata.gestacion.recto, by = "OTUID")
puntos$orden<- factor(puntos$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                   "Hembra2600NoRep","Hembra4150NoRep",
                                                   "Macho2600Rep", "Macho4150Rep",
                                                   "Macho2600NoRep", "Macho4150NoRep"))


# Figura por seccion 
PCA_gestacion.aldex.clr.recto <- ggplot() +
  theme_bw() +
  xlab(PC1.gestacion.recto) +
  ylab(PC2.gestacion.recto) +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 12),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(colour = "black"),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text.x.top = element_text(colour = "black"),
        legend.position = "right", 
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.01, 'cm'),
        #axis.line.x.top = element_line(color = "red", size = 2), 
        #axis.line.y.right =  element_line(color = "black", size = 2),
        panel.border = element_rect(color = "black", size = 1.5),
        legend.title = element_blank()) + 
  geom_point(data = puntos,
             aes(x=PC1, y=PC2, fill=orden), shape=21, size= 4) +
  #geom_text(data = data.frame(pca.transferencia$x) %>% rownames_to_column(var = "ID") %>%
  #left_join(metadata.transf.2600.filter, by = "ID"),
  #aes(x=PC1, y=PC2, fill=Seccion,label= ID)) +
  geom_vline(xintercept = 0, linetype = 3) + #lines-cross 
  geom_hline(yintercept = 0, linetype = 3) +
  scale_fill_manual(values = color.orden)+
  scale_x_continuous(limits = c(-40,40))+
  scale_y_continuous(limits = c(-50,50))
#theme(legend.text = element_text(size = 9))+guides(fill=guide_legend(nrow = 1))

#Observar PCA por recto
print(PCA_gestacion.aldex.clr.recto)

# perMANOVA por seccion
permanova.comparacion.recto<- adonis2(transfor.aldex.gestacion.recto.data ~ comparacion, data = metadata.gestacion.recto,
                                          method = "euclidean", permutations = 999)

print(permanova.comparacion.recto)


#PCA SEXO_TEMPORADA
#Cambiar a factor, la variable sexo_temporada
metadata.gestacion.recto$Sexo_temporada <- factor(metadata.gestacion.recto$Sexo_temporada)
str(metadata.gestacion.recto)

#Transformar datos 
#METODO 1
transfor.aldex.sex_temp.recto <- aldex.clr(gestacion.recto.filtrada, mc.samples = 999, denom ="all", verbose = FALSE, useMC = FALSE)
transfor.aldex.sex_temp.recto.data <- t(getMonteCarloSample(transfor.aldex.sex_temp.recto,1))

#PCA
pca.sex_temp.recto <- prcomp(transfor.aldex.sex_temp.recto.data)

#Definir niveles
PC1.sex_temp.recto <- paste("PC1", round(sum(pca.sex_temp.recto$sdev[1] ^ 2) / mvar(transfor.aldex.sex_temp.recto.data) * 100, 1), "%")
PC2.sex_temp.recto <- paste("PC2", round(sum(pca.sex_temp.recto$sdev[2] ^ 2) / mvar(transfor.aldex.sex_temp.recto.data) * 100, 1), "%")

#definir colores 
#color.myorden <- c("#F26B1D", "#F2913D", "#49798C", "#9CCED9")
#"#094293","#9CCED9","#BF9445", "#f9ec3c"

#Especificar el orden en el que quiero que grafique 
puntos_sex_temp <- data.frame(pca.sex_temp.recto$x) %>% rownames_to_column(var = "OTUID") %>%
  left_join(metadata.gestacion.recto, by = "OTUID")
puntos_sex_temp$se.tem<- factor(puntos_sex_temp$Sexo_temporada, levels=c("Hembra Reproductiva","Hembra No Reproductiva",
                                                                         "Macho Reproductivo", "Macho No Reproductivo"))


# Figura por seccion 
PCA_sex_temp.recto.aldex.clr <- ggplot() +
  theme_bw() +
  xlab(PC1.sex_temp.recto) +
  ylab(PC2.sex_temp.recto) +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 12),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(colour = "black"),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text.x.top = element_text(colour = "black"),
        legend.position = "right", 
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.01, 'cm'),
        #axis.line.x.top = element_line(color = "red", size = 2), 
        #axis.line.y.right =  element_line(color = "black", size = 2),
        panel.border = element_rect(color = "black", size = 1.5),
        legend.title = element_blank()) + 
  geom_point(data = puntos_sex_temp,
             aes(x=PC1, y=PC2, color=se.tem, shape=poblacion), size= 6) +
                scale_shape_manual(values = c(16,17))+
  #geom_text(data = data.frame(pca.transferencia$x) %>% rownames_to_column(var = "ID") %>%
  #left_join(metadata.transf.2600.filter, by = "ID"),
  #aes(x=PC1, y=PC2, fill=Seccion,label= ID)) +
  #geom_vline(xintercept = 0, linetype = 3) + #lines-cross 
  #geom_hline(yintercept = 0, linetype = 3) +
  scale_colour_manual(values = c("#F26B1D", "#F2913D", "#49798C", "#9CCED9"))+
  scale_x_continuous(limits = c(-40,40))+
  scale_y_continuous(limits = c(-50,50))
#theme(legend.text = element_text(size = 9))+guides(fill=guide_legend(nrow = 1))

#Observar PCA 
print(PCA_sex_temp.recto.aldex.clr)
print(PCA_sex_temp.ileon.aldex.clr)

# perMANOVA por seccion
permanova.sexo.temp.recto<- adonis2(transfor.aldex.sex_temp.recto.data ~ Sexo_temporada, data = metadata.gestacion.recto,
                                          method = "euclidean", permutations = 999)

print(permanova.sexo.temp.recto)


####NMDS####

library(permute)
library(lattice)
library(MASS)
library(cluster)
library(nlme)
library(mgcv)
library(parallel)
library(tcltk)
library(knitr)
library(markdown)
library(vegan)
library(tidyverse)
library(dplyr)
library(tibble)
library(hilldiv)
library(ggplot2)
library(ggpubr)

#NMDS DE RECTO
#otu de recto
gestacion.recto<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("gestacion/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("gestacion/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl"))) %>%
  mutate(Sexo_temporada=factor(Sexo_temporada, levels = c("Hembra No Reproductiva","Hembra Reproductiva", "Macho No Reproductivo", "Macho Reproductivo"),
                                               labels = c("Non-reproductive female","Reproductive female", "Non-reproductive male", "Reproductive male")))


#Cambiar a factor, la variable sexo_poblacion
metadata.gestacion.recto$Sexo_poblacion <- factor(metadata.gestacion.recto$Sexo_poblacion)
str(metadata.gestacion.recto)

#Cambiar a factor, la variable sexo_temporada
metadata.gestacion.recto$Sexo_temporada <- factor(metadata.gestacion.recto$Sexo_temporada)
str(metadata.gestacion.recto)

#Cambiar a factor, la variable poblacion
metadata.gestacion.recto$poblacion <- factor(metadata.gestacion.recto$poblacion)
str(metadata.gestacion.recto)

#Cambiar a factor, la variable temporada
metadata.gestacion.recto$temporada <- factor(metadata.gestacion.recto$temporada)
str(metadata.gestacion.recto)

#Cambiar a factor, la variable sexo
metadata.gestacion.recto$sexo <- factor(metadata.gestacion.recto$sexo)
str(metadata.gestacion.recto)


#transponer otu
gestacion.recto.trans <- as.data.frame(t(gestacion.recto.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion <- vegdist(gestacion.recto.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto <- metaMDS(matriz.gestacion, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.recto <- nmds.recto %>% 
              left_join(metadata.gestacion.recto, by="OTUID") %>%
              group_by(Sexo_temporada) %>%
              mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
              mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
              ungroup()


#Especificar el orden en el que quiero que grafique 
nmds.recto$se.tem<- factor(nmds.recto$Sexo_temporada, levels=c("Reproductive female","Non-reproductive female",
                                                               "Reproductive male","Non-reproductive male"))


##FIGURA SEXO-TEMPORADA
NMDS_plot.recto.sextem <- ggplot(nmds.recto, aes(x=NMDS1, y=NMDS2, colour=se.tem))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= se.tem), size=3) + #shape=poblacion
  #scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#5D3277", "#CFB1FF", "#AF6502", "#FEA509"))
  #guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.recto.sextem)


##FIGURA POR POBLACION
NMDS_plot.recto.pob <- ggplot(nmds.recto, aes(x=NMDS1, y=NMDS2, fill=poblacion))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= poblacion, shape=poblacion), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.y = unit(0.01, 'cm'),
        #panel.border = element_rect(color = "black", size = 1.5),
        axis.ticks.x=element_blank())+
  #scale_x_continuous(limits = c(-20,40))+
  #scale_y_continuous(limits = c(-20,40))+
  scale_fill_manual(values = c("#676778","#D9D9C2"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

print(NMDS_plot.recto.pob)
#ggsave("nmds.recto.poblacion.pdf", width = 8, height = 4, dpi = 600, plot = NMDS_plot.recto.pob, device = "pdf")


#Especificar el orden en el que quiero que grafique 
#nmds.recto$orden<- factor(nmds.recto$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                           #"Hembra2600NoRep","Hembra4150NoRep",
                                                           #"Macho2600Rep", "Macho4150Rep",  
                                                           #"Macho2600NoRep", "Macho4150NoRep"))



#figura
#colors<- c("#5d46e2", "#766ec5", "#b38bff", "#cfb1ff","#ff8000","#ff963e","#ffc08c","#ffd5b1")


#NMDS_plot.recto <- ggplot(nmds.recto, aes(x=NMDS1, y=NMDS2, color=comparacion))+
  #geom_point(aes(x=NMDS1, y=NMDS2, color= comparacion, shape=sexo, shape=21), size=9) +
  #geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  #theme_classic()+
  #theme(legend.position = "right",
        #legend.text = element_text(face = "bold"),
        #axis.ticks.x=element_blank())+
  #scale_color_manual(values = c("#673dff", "#9265ff", "#b38bff", "#cfb1ff","#af6502","#ca7a04","#e49007","#fea509"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

#print(NMDS_plot.recto)

#otra forma 
#NMDS_Plot <- ggplot(nmds.recto, aes(x = NMDS1, y = NMDS2, color = comparacion)) +
  #geom_point(aes(x=NMDS1, y=NMDS2, color= comparacion, shape=temporada), size = 8) +
  #geom_segment(aes(x = x_cen, y = y_cen, xend = NMDS1, yend = NMDS2), alpha = 0.2) +
  #theme_classic() +
  #theme(legend.position = "right",
        #axis.ticks.x = element_blank()) +
  #scale_colour_manual(values = c("#673dff", "#9265ff", "#b38bff", "#cfb1ff","#af6502","#ca7a04","#e49007","#fea509"))

#NMDS_Plot


##FIGURA POR TEMPORADA

#transponer otu
gestacion.recto.trans <- as.data.frame(t(gestacion.recto.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion <- vegdist(gestacion.recto.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto.tem <- metaMDS(matriz.gestacion, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.recto.tem <- nmds.recto.tem %>% 
  left_join(metadata.gestacion.recto, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA POR TEMPORADA
NMDS_plot.recto.temp <- ggplot(nmds.recto.tem, aes(x=NMDS1, y=NMDS2, fill=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= temporada, shape=temporada), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.y = unit(0.01, 'cm'),
        #panel.border = element_rect(color = "black", size = 1.5),
        axis.ticks.x=element_blank())+
  #scale_x_continuous(limits = c(-20,40))+
  #scale_y_continuous(limits = c(-20,40))+
  scale_fill_manual(values = c("#808000","#1B5E20"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

print(NMDS_plot.recto.temp)


##FIGURA POR SEXO

#transponer otu
gestacion.recto.trans <- as.data.frame(t(gestacion.recto.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion <- vegdist(gestacion.recto.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto.sex <- metaMDS(matriz.gestacion, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.recto.sex <- nmds.recto.sex %>% 
  left_join(metadata.gestacion.recto, by="OTUID") %>%
  group_by(sexo) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA POR SEXO
NMDS_plot.recto.sexo <- ggplot(nmds.recto.sex, aes(x=NMDS1, y=NMDS2, fill=sexo))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= sexo, shape=sexo), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.y = unit(0.01, 'cm'),
        #panel.border = element_rect(color = "black", size = 1.5),
        axis.ticks.x=element_blank())+
  #scale_x_continuous(limits = c(-20,40))+
  #scale_y_continuous(limits = c(-20,40))+
  scale_fill_manual(values = c("#5D3277","#AF6502"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

print(NMDS_plot.recto.sexo)


#FIGURA DE MACHOS 

##FIGURA DE MACHOS
#otu completa (machos)
#otu de machos en el recto
gestacion.recto.machos<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos.recto.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.recto.machos.sin0 <- gestacion.recto.machos%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.recto<- read.delim("gestacion/asvs.filtrar.recto2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.recto.machos.filtrada <- gestacion.recto.machos.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.machos.filtrada <- gestacion.recto.machos.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.recto.machos.filtrada.trans <- as.data.frame(t(gestacion.recto.machos.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos.recto <- vegdist(gestacion.recto.machos.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto.machos <- metaMDS(matriz.machos.recto, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrar metadata 
metadata.machos.recto <- metadata.gestacion.recto %>% filter(sexo=="Male") 

#Unir con metadata
nmds.recto.machos <- nmds.recto.machos %>% 
  left_join(metadata.machos.recto, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR TEMPORADA-POBLACION
NMDS_plot.recto.machos <- ggplot(nmds.recto.machos, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada, shape=poblacion), size=3) + #
  scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.recto.machos)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos.recto <-with(metadata.machos.recto, adonis2(matriz.machos.recto ~ temporada, data = metadata.machos.recto,
                                                             method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos.recto)


#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#otu machos 2600 (recto)
#otu de machos en el recto
gestacion.recto.machos26<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos26.recto.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu anterior
gestacion.recto.machos26.sin0 <- gestacion.recto.machos26 %>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.recto<- read.delim("gestacion/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.recto.machos26.filtrada <- gestacion.recto.machos26.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.machos26.filtrada <- gestacion.recto.machos26.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.recto.machos26.filtrada.trans <- as.data.frame(t(gestacion.recto.machos26.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos26.recto <- vegdist(gestacion.recto.machos26.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto.machos26 <- metaMDS(matriz.machos26.recto, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrat metadata
metadata.machos26.recto <- metadata.machos.recto %>% filter(poblacion=="2600 masl") 

#Unir con metadata
nmds.recto.machos26 <- nmds.recto.machos26 %>% 
  left_join(metadata.machos26.recto, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR POBLACION (2600)
NMDS_plot.recto.machos26 <- ggplot(nmds.recto.machos26, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada), size=3) + #shape=poblacion
  #scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum 2600 masl", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.recto.machos26)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos26.recto <-with(metadata.machos26.recto, adonis2(matriz.machos26.recto ~ temporada, data = metadata.machos26.recto,
                                                                 method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos26.recto)

#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#otu machos 4150 (ileon)
#otu de machos en el ileon
gestacion.recto.machos41<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos41.recto.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu anterior
gestacion.recto.machos41.sin0 <- gestacion.recto.machos41 %>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.recto<- read.delim("gestacion/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.recto.machos41.filtrada <- gestacion.recto.machos41.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.machos41.filtrada <- gestacion.recto.machos41.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.recto.machos41.filtrada.trans <- as.data.frame(t(gestacion.recto.machos41.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos41.recto <- vegdist(gestacion.recto.machos41.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.recto.machos41 <- metaMDS(matriz.machos41.recto, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrat metadata
metadata.machos41.recto <- metadata.machos.recto %>% filter(poblacion=="4150 masl") 

#Unir con metadata
nmds.recto.machos41 <- nmds.recto.machos41 %>% 
  left_join(metadata.machos41.recto, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR POBLACION (4150)
NMDS_plot.recto.machos41 <- ggplot(nmds.recto.machos41, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada), size=3) + #shape=poblacion
  #scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Rectum 4150 masl", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.recto.machos41)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos41.recto <-with(metadata.machos41.recto, adonis2(matriz.machos41.recto ~ temporada, data = metadata.machos41.recto,
                                                                 method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos41.recto)


#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#UNIR LAS 2 FIGURAS EN 1, MACHOS POR POBLACION COMPARADO POR TEMPORADA
library(cowplot)
nmds.machos26.41.recto<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.recto.machos26+theme(legend.title = element_blank(),
                                 legend.text = element_blank(),
                                 legend.position = "none",
                                 axis.text.x = element_text(size = 7),
                                 axis.text.y = element_text(size = 7),
                                 strip.background = element_rect(fill = "#8D176F"),
                                 strip.text.x = element_text(size = 10, face = "bold"),
                                 axis.title.x = element_text(vjust = 0.3, size = 8),
                                 axis.title.y = element_text(vjust = 0.3, size = 8),
                                 plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.recto.machos41+theme(legend.title = element_blank(),
                                 legend.text = element_blank(),
                                 legend.position = "none",
                                 legend.box.spacing = unit(0.5, "pt"),
                                 axis.text.x = element_text(size = 7),
                                 strip.background = element_rect(fill = "#8D176F"),
                                 axis.text.y = element_text(size = 7),
                                 plot.margin =unit(c(0,0,0,0), "cm"), 
                                 strip.text.x = element_text(size = 10, face = "bold"),
                                 axis.title.x = element_text(vjust = 0.3, size = 8), 
                                 axis.title.y = element_text(vjust = 0.3, size = 8)),
  #theme(guides(fill= guide_legend(nrow = 2)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.machos26.41.recto

#ggsave("nmds.machos26.41.recto.pdf", width = 3, height = 5, dpi = 600, plot = nmds.machos26.41.recto, device = "pdf")

  
  
  
  
  
  
  
  
  

#NMDS DE ILEON
#otu de ILEON
gestacion.ileon<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("gestacion/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl"))) %>%
  mutate(Sexo_temporada=factor(Sexo_temporada, levels = c("Hembra No Reproductiva","Hembra Reproductiva", "Macho No Reproductivo", "Macho Reproductivo"),
                                               labels = c("Non-reproductive female","Reproductive female", "Non-reproductive male", "Reproductive male")))



#Cambiar a factor, la variable compar_sexo_poblacion
metadata.gestacion.ileon$Sexo_poblacion <- factor(metadata.gestacion.ileon$Sexo_poblacion)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable compar_sexo_temporada
metadata.gestacion.ileon$Sexo_temporada <- factor(metadata.gestacion.ileon$Sexo_temporada)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable poblacion
metadata.gestacion.ileon$poblacion <- factor(metadata.gestacion.ileon$poblacion)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable temporada
metadata.gestacion.ileon$temporada <- factor(metadata.gestacion.ileon$temporada)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable sexo
metadata.gestacion.ileon$sexo <- factor(metadata.gestacion.ileon$sexo)
str(metadata.gestacion.ileon)

#transponer otu
gestacion.ileon.trans <- as.data.frame(t(gestacion.ileon.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion.ileon <- vegdist(gestacion.ileon.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon <- metaMDS(matriz.gestacion.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.ileon <- nmds.ileon %>% 
  left_join(metadata.gestacion.ileon, by="OTUID") %>%
  group_by(Sexo_temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


#Especificar el orden en el que quiero que grafique 
nmds.ileon$se.tem<- factor(nmds.ileon$Sexo_temporada, levels=c("Reproductive female","Non-reproductive female",
                                                               "Reproductive male","Non-reproductive male"))


##FIGURA SEXO-TEMPORADA
NMDS_plot.ileon.sextem <- ggplot(nmds.ileon, aes(x=NMDS1, y=NMDS2, color=se.tem))+
  geom_point(aes(x=NMDS1, y=NMDS2, color= se.tem), size=3) +
  #scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#5D3277", "#CFB1FF", "#AF6502", "#FEA509"))  
  

print(NMDS_plot.ileon.sextem)

#ggsave("nmds.plot.ileon.sextem.pdf", width = 5, height = 5, dpi = 600, plot = NMDS_plot.ileon.sextem, device = "pdf")


##FIGURA POR POBLACION

#Unir con metadata
nmds.ileon <- nmds.ileon %>% 
  left_join(metadata.gestacion.ileon, by="OTUID") %>%
  group_by(poblacion) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


NMDS_plot.ileon.pob <- ggplot(nmds.ileon, aes(x=NMDS1, y=NMDS2, fill=poblacion))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= poblacion, shape=poblacion), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = c("#676778","#D9D9C2"))

print(NMDS_plot.ileon.pob)


##FIGURA POR TEMPORADA
#transponer otu
gestacion.ileon.trans <- as.data.frame(t(gestacion.ileon.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion.ileon <- vegdist(gestacion.ileon.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon.temp <- metaMDS(matriz.gestacion.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.ileon.temp <- nmds.ileon.temp %>% 
  left_join(metadata.gestacion.ileon, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA POR TEMPORADA
NMDS_plot.ileon.temp <- ggplot(nmds.ileon.temp, aes(x=NMDS1, y=NMDS2, fill=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= temporada, shape=temporada), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.y = unit(0.01, 'cm'),
        #panel.border = element_rect(color = "black", size = 1.5),
        axis.ticks.x=element_blank())+
  #scale_x_continuous(limits = c(-20,40))+
  #scale_y_continuous(limits = c(-20,40))+
  scale_fill_manual(values = c("#808000","#1B5E20"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

print(NMDS_plot.ileon.temp)


##FIGURA POR SEXO
#transponer otu
gestacion.ileon.trans <- as.data.frame(t(gestacion.ileon.filtrada))

#matriz de distancia con otu transpuesta
matriz.gestacion.ileon <- vegdist(gestacion.ileon.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon.sex <- metaMDS(matriz.gestacion.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#Unir con metadata
nmds.ileon.sex <- nmds.ileon.sex %>% 
  left_join(metadata.gestacion.ileon, by="OTUID") %>%
  group_by(sexo) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA POR SEXO
NMDS_plot.ileon.sex <- ggplot(nmds.ileon.sex, aes(x=NMDS1, y=NMDS2, fill=sexo))+
  geom_point(aes(x=NMDS1, y=NMDS2, fill= sexo, shape=sexo), size=3) +
  scale_shape_manual(values = c(21,24))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine", scales = "free", dir = "v")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.y = unit(0.01, 'cm'),
        #panel.border = element_rect(color = "black", size = 1.5),
        axis.ticks.x=element_blank())+
  #scale_x_continuous(limits = c(-20,40))+
  #scale_y_continuous(limits = c(-20,40))+
  scale_fill_manual(values = c("#5D3277","#AF6502"))

##673dff, #9265ff azules
##af6502, #ca7a04,#e49007,#fea509

print(NMDS_plot.ileon.sex)


##FIGURA DE MACHOS
#otu completa (machos)
#otu de machos en el ileon
gestacion.ileon.machos<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos.ileon.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.machos.sin0 <- gestacion.ileon.machos%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.machos.filtrada <- gestacion.ileon.machos.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.machos.filtrada <- gestacion.ileon.machos.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.ileon.machos.filtrada.trans <- as.data.frame(t(gestacion.ileon.machos.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos.ileon <- vegdist(gestacion.ileon.machos.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon.machos <- metaMDS(matriz.machos.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrar metadata 
metadata.machos.ileon <- metadata.gestacion.ileon %>% filter(sexo=="Male") 

#Unir con metadata
nmds.ileon.machos <- nmds.ileon.machos %>% 
  left_join(metadata.machos.ileon, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR TEMPORADA-POBLACION
NMDS_plot.ileon.machos <- ggplot(nmds.ileon.machos, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada, shape=poblacion), size=3) + #
  scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.ileon.machos)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos.ileon <-with(metadata.machos.ileon, adonis2(matriz.machos.ileon ~ temporada, data = metadata.machos.ileon,
                                                            method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos.ileon)


#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#otu machos 2600 (ileon)
#otu de machos en el ileon
gestacion.ileon.machos26<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos26.ileon.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu anterior
gestacion.ileon.machos26.sin0 <- gestacion.ileon.machos26 %>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.machos26.filtrada <- gestacion.ileon.machos26.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.machos26.filtrada <- gestacion.ileon.machos26.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.ileon.machos26.filtrada.trans <- as.data.frame(t(gestacion.ileon.machos26.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos26.ileon <- vegdist(gestacion.ileon.machos26.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon.machos26 <- metaMDS(matriz.machos26.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrat metadata
metadata.machos26.ileon <- metadata.machos.ileon %>% filter(poblacion=="2600 masl") 

#Unir con metadata
nmds.ileon.machos26 <- nmds.ileon.machos26 %>% 
  left_join(metadata.machos26.ileon, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR POBLACION (2600)
NMDS_plot.ileon.machos26 <- ggplot(nmds.ileon.machos26, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada), size=3) + #shape=poblacion
  #scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine 2600 masl", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.ileon.machos26)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos26.ileon <-with(metadata.machos26.ileon, adonis2(matriz.machos26.ileon ~ temporada, data = metadata.machos26.ileon,
                                                             method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos26.ileon)

#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#otu machos 4150 (ileon)
#otu de machos en el ileon
gestacion.ileon.machos41<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/otu.machos41.ileon.0.99.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu anterior
gestacion.ileon.machos41.sin0 <- gestacion.ileon.machos41 %>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.machos41.filtrada <- gestacion.ileon.machos41.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.machos41.filtrada <- gestacion.ileon.machos41.filtrada %>% column_to_rownames(var = "OTUID")

#transponer otu
gestacion.ileon.machos41.filtrada.trans <- as.data.frame(t(gestacion.ileon.machos41.filtrada))

#matriz de distancia con otu transpuesta
matriz.machos41.ileon <- vegdist(gestacion.ileon.machos41.filtrada.trans, method = "robust.aitchison")

#correr nmds con matriz transpuesta
nmds.ileon.machos41 <- metaMDS(matriz.machos41.ileon, trymax = 500, k=2, verbosity=FALSE) %>%
  vegan:::scores.metaMDS() %>%
  as_tibble(., rownames= "OTUID")

#filtrat metadata
metadata.machos41.ileon <- metadata.machos.ileon %>% filter(poblacion=="4150 masl") 

#Unir con metadata
nmds.ileon.machos41 <- nmds.ileon.machos41 %>% 
  left_join(metadata.machos41.ileon, by="OTUID") %>%
  group_by(temporada) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup()


##FIGURA de MACHOS POR POBLACION (4150)
NMDS_plot.ileon.machos41 <- ggplot(nmds.ileon.machos41, aes(x=NMDS1, y=NMDS2, colour=temporada))+
  geom_point(aes(x=NMDS1, y=NMDS2, colour= temporada), size=3) + #shape=poblacion
  #scale_shape_manual(values = c(16,17))+
  geom_segment(aes(x = x_cen, y = y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2)+
  theme_bw()+
  facet_wrap(~"Small intestine 4150 masl", scales = "free", dir = "v")+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values = c("#AF6502", "#FEA509"))
#guides(fill= guide_legend(nrow = 2))

print(NMDS_plot.ileon.machos41)

# perMANOVA por seccion bloqueando por año 
library(vegan)
permanova.machos41.ileon <-with(metadata.machos41.ileon, adonis2(matriz.machos41.ileon ~ temporada, data = metadata.machos41.ileon,
                                                                 method = "euclidean", permutations = 999, strata = ano))

print(permanova.machos41.ileon)


#ggsave("nmds.ileon.machos.pdf", width = 3, height = 5, dpi = 600, plot = NMDS_plot.ileon.machos, device = "pdf")


#UNIR LAS 2 FIGURAS EN 1, MACHOS POR POBLACION COMPARADO POR TEMPORADA
library(cowplot)
nmds.machos26.41<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.machos26+theme(legend.title = element_blank(),
                               legend.text = element_blank(),
                               legend.position = "none",
                               axis.text.x = element_text(size = 7),
                               axis.text.y = element_text(size = 7),
                               strip.background = element_rect(fill = "#7258A0"),
                               strip.text.x = element_text(size = 10, face = "bold"),
                               axis.title.x = element_text(vjust = 0.3, size = 8),
                               axis.title.y = element_text(vjust = 0.3, size = 8),
                               plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.ileon.machos41+theme(legend.title = element_blank(),
                               legend.text = element_blank(),
                               legend.position = "none",
                               legend.box.spacing = unit(0.5, "pt"),
                               axis.text.x = element_text(size = 7),
                               strip.background = element_rect(fill = "#7258A0"),
                               axis.text.y = element_text(size = 7),
                               plot.margin =unit(c(0,0,0,0), "cm"), 
                               strip.text.x = element_text(size = 10, face = "bold"),
                               axis.title.x = element_text(vjust = 0.3, size = 8), 
                               axis.title.y = element_text(vjust = 0.3, size = 8)),
  #theme(guides(fill= guide_legend(nrow = 2)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.machos26.41

#ggsave("nmds.machos26.41.pdf", width = 3, height = 5, dpi = 600, plot = nmds.machos26.41, device = "pdf")



#UNIR LAS 2 FIGURAS EN 1, POBLACION
library(cowplot)
nmds.poblacion.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.pob+theme(legend.title = element_blank(),
                          legend.text = element_blank(),
                          legend.position = "none",
                          axis.text.x = element_text(size = 7),
                          axis.text.y = element_text(size = 7),
                          strip.text.x = element_text(size = 10),
                          axis.title.x = element_text(vjust = 0.3, size = 8),
                          axis.title.y = element_text(vjust = 0.3, size = 8),
                          plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.recto.pob+theme(legend.title = element_blank(),
                          legend.text = element_text(size = 8),
                          legend.position = "bottom",
                          legend.box.spacing = unit(0.5, "pt"),
                          axis.text.x = element_text(size = 7),
                          axis.text.y = element_text(size = 7),
                          plot.margin =unit(c(0,0,0,0), "cm"), 
                          strip.text.x = element_text(size = 10),
                          axis.title.x = element_text(vjust = 0.3, size = 8), 
                          axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.poblacion.completo

#ggsave("nmds.poblacion.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.poblacion.completo, device = "pdf")


#UNIR LAS 2 FIGURAS EN 1, TEMPORADA
library(cowplot)
nmds.temporada.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.temp+theme(legend.title = element_blank(),
                            legend.text = element_blank(),
                            legend.position = "none",
                            axis.text.x = element_text(size = 7),
                            axis.text.y = element_text(size = 7),
                            strip.text.x = element_text(size = 10),
                            axis.title.x = element_text(vjust = 0.3, size = 8),
                            axis.title.y = element_text(vjust = 0.3, size = 8),
                            plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.recto.temp+theme(legend.title = element_blank(),
                            legend.text = element_text(size = 8),
                            legend.position = "bottom",
                            legend.box.spacing = unit(0.5, "pt"),
                            axis.text.x = element_text(size = 7),
                            axis.text.y = element_text(size = 7),
                            plot.margin =unit(c(0,0,0,0), "cm"), 
                            strip.text.x = element_text(size = 10),
                            axis.title.x = element_text(vjust = 0.3, size = 8), 
                            axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.temporada.completo

#ggsave("nmds.temporada.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.temporada.completo, device = "pdf")

#UNIR LAS 2 FIGURAS EN 1, SEXO
library(cowplot)
nmds.sexo.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.sex+theme(legend.title = element_blank(),
                             legend.text = element_blank(),
                             legend.position = "none",
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8),
                             axis.title.y = element_text(vjust = 0.3, size = 8),
                             plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.recto.sexo+theme(legend.title = element_blank(),
                             legend.text = element_text(size = 8),
                             legend.position = "bottom",
                             legend.box.spacing = unit(0.5, "pt"),
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             plot.margin =unit(c(0,0,0,0), "cm"), 
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8), 
                             axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.sexo.completo

#ggsave("nmds.sexo.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.sexo.completo, device = "pdf")


#UNIR LAS 2 FIGURAS EN 1, SEXO-TEMPORADA
library(cowplot)
nmds.sextem.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.sextem+theme(legend.title = element_blank(),
                            legend.text = element_blank(),
                            legend.position = "none",
                            axis.text.x = element_text(size = 7),
                            axis.text.y = element_text(size = 7),
                            strip.background = element_rect(fill = "#7258A0"),
                            strip.text.x = element_text(size = 10, face = "bold"),
                            axis.title.x = element_text(vjust = 0.3, size = 8),
                            axis.title.y = element_text(vjust = 0.3, size = 8),
                            plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  NMDS_plot.recto.sextem+theme(legend.title = element_blank(),
                             legend.text = element_blank(),
                             legend.position = "none",
                             legend.box.spacing = unit(0.5, "pt"),
                             axis.text.x = element_text(size = 7),
                             strip.background = element_rect(fill = "#8D176F"),
                             axis.text.y = element_text(size = 7),
                             plot.margin =unit(c(0,0,0,0), "cm"), 
                             strip.text.x = element_text(size = 10, face = "bold"),
                             axis.title.x = element_text(vjust = 0.3, size = 8), 
                             axis.title.y = element_text(vjust = 0.3, size = 8)),
                             #theme(guides(fill= guide_legend(nrow = 2)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.sextem.completo

ggsave("nmds.sextem.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.sextem.completo, device = "pdf")


#UNIR LAS 2 FIGURAS EN 1, MACHOS POR TEMPORADA EN AMBAS POBLACIONES
library(cowplot)
nmds.machos.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  NMDS_plot.ileon.machos+theme(legend.title = element_blank(),
                               legend.text = element_blank(),
                               legend.position = "none",
                               axis.text.x = element_text(size = 7),
                               axis.text.y = element_text(size = 7),
                               strip.background = element_rect(fill = "#7258A0"),
                               strip.text.x = element_text(size = 10, face = "bold"),
                               axis.title.x = element_text(vjust = 0.3, size = 8),
                               axis.title.y = element_text(vjust = 0.3, size = 8),
                               plot.margin =unit(c(0,0,0.2,0), "cm")),      
  NMDS_plot.recto.machos+theme(legend.title = element_blank(),
                               legend.text = element_blank(),
                               legend.position = "none",
                               legend.box.spacing = unit(0.5, "pt"),
                               axis.text.x = element_text(size = 7),
                               strip.background = element_rect(fill = "#8D176F"),
                               axis.text.y = element_text(size = 7),
                               plot.margin =unit(c(0,0,0,0), "cm"), 
                               strip.text.x = element_text(size = 10, face = "bold"),
                               axis.title.x = element_text(vjust = 0.3, size = 8), 
                               axis.title.y = element_text(vjust = 0.3, size = 8)),
  #theme(guides(fill= guide_legend(nrow = 2)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

nmds.machos.completo

ggsave("nmds.machos.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.machos.completo, device = "pdf")









######BARPLOT############ 

library(tidyverse)
library(dplyr)
library(pals)
library(RColorBrewer)
library(readr)
library(cowplot)


#####BARPLOT DE RECTO#########
#otu de recto
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto")


#metadata
#metadata.transferencia.2600.completo <- read.delim("//home/yendi//Downloads//mapa.capII.transferencia.completo (1).txt", check.names = F) %>% 
  #mutate(Type=factor(Origen, levels = c("Mother","Control of aseptic technique","Embryo"),
                     #labels = c("Mother", "Asepsia control", "Embryo")))


#taxonomia
taxonomia<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#funcion para obtener abundancia relativa 
relabunda<- function(x){(as.data.frame(t(t(x)/colSums(x)))*100)}


#NIVEL PHYLUM
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
                        rownames_to_column(var = "OTUID") %>% 
                        inner_join(metadata.gestacion.recto) %>% 
                        filter(sexo=="Hembra") %>% 
                        filter(poblacion=="2600 msnm") %>% 
                        filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras <- c("26AFR","26BFR","26CFR","26DFR","26EFR","26FP12R","26FP16R","26FP3R","26FP7R","26FP8R")

HEM.2600.REP.phylum_10 <- otutable.HEM.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:3)


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.REP.41 <- c("41AFR","41DFR","41FFR","41FR","41JFR","41FP16R","41FP17R","41FP18R","41FP19R","41FP20R")


HEM.4150.REP.phylum_10 <- otutable.HEM.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26 <- c("261MR","262MR","263MR","264MR","265MR","26M14R","26M19R","26M2ScR","26M4ScR","26M6ScR")


MAC.2600.REP.phylum_10 <- otutable.MAC.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MAC.REP.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41 <- c("4110MR","41EMR","41GMR","41IMR","41JMR","41M11R","41M12R","41M13R","41M14R","41M15R")


MAC.4150.REP.phylum_10 <- otutable.MAC.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MAC.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:3)


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26 <- c("26FV1R","26FV2R","26FV3R","26FV4R","26FV5R","26H2R","26H3R","26H4R","26H7R","26H8R")

HEM.2600.NREP.phylum_10 <- otutable.HEM.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:3)


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41 <- c("41FV1R","41FV2R","41FV3R","41FV4R","41FV5R","H31R","H32R","H33R","H34R","H35R")

HEM.4150.NREP.phylum_10 <- otutable.HEM.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26 <- c("26MVAR","26MVBR","26MVDR","26MVER","26M2R","26M5R","26M6R","26M9R")

MAC.2600.NREP.phylum_10 <- otutable.MAC.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41 <- c("41MVAR","41MVBR","41MVCR","41MVDR","41MVER","M1R","M2R","M3R","M4R","M5R")

MAC.4150.NREP.phylum_10 <- otutable.MAC.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#unir todo
phylum_recto.all<- HEM.2600.REP.phylum_10 %>% rownames_to_column(var = "id") %>%
              full_join(HEM.4150.REP.phylum_10 %>% rownames_to_column(var="id")) %>% 
              full_join(MAC.2600.REP.phylum_10 %>% rownames_to_column(var="id")) %>% 
              full_join(MAC.4150.REP.phylum_10 %>% rownames_to_column(var="id")) %>%
              full_join(HEM.2600.NREP.phylum_10 %>% rownames_to_column(var="id")) %>%
              full_join(HEM.4150.NREP.phylum_10 %>% rownames_to_column(var="id")) %>% 
              full_join(MAC.2600.NREP.phylum_10 %>% rownames_to_column(var="id")) %>% 
              full_join(MAC.4150.NREP.phylum_10 %>% rownames_to_column(var="id"))

lista.phylum.recto<- unique(phylum_recto.all$id)

phylum_recto.all_final<- gestacion.recto.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(p) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "p") %>%
  relabunda() %>%
  rownames_to_column(var = "phylum") %>%
  filter(phylum %in% lista.phylum.recto)

#preparar archivo
phylum_recto_plot<- phylum_recto.all_final %>% 
  pivot_longer(cols=-phylum, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.recto) %>%
  mutate(phylum=str_replace(phylum,"p__", "")) 
  #mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
                                  #labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%
  
  
#Especificar el orden en el que quiero que grafique 
phylum_recto_plot$orden<- factor(phylum_recto_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                         "Macho2600Rep", "Macho4150Rep",
                                                                         "Hembra2600NoRep","Hembra4150NoRep",
                                                                         "Macho2600NoRep", "Macho4150NoRep"))



#FIGURA PHYLUM
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.p <- 10
mycolors.p = colorRampPalette(brewer.pal(8, "Dark2")) (colors.p) 
mycolors.p[1]<- "#1b9e77"
mycolors.p[2]<- "#a6761d"
mycolors.p[3]<- "#7570B3"
mycolors.p[4]<- "#E7298A" 
 
##AE6D1C verde
#mycolors[1]<- "#D95F02" anaranjado
#mycolors[2]<- "#7570b3" lila
#mycolors[3]<- "#66a61e" verde pasto
#mycolors[4]<- "#E7298A" rosa fiucsia

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)


pp.recto<- phylum_recto_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=phylum))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Phylum")) +
  scale_fill_manual(values = mycolors.p) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    axis.text.y = element_text(size=8, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 8, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(colour="black", size = 10, face = "bold"),
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica", color = "white", face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+guides(fill=guide_legend(title = "Phylum"))

pp.recto



#Especificar el orden en el que quiero que grafique 
metadata.gestacion.recto$temporada2<- factor(metadata.gestacion.recto$temporada, levels=c("Reproductiva","No Reproductiva"))


###ANOTACION
h1<- metadata.gestacion.recto%>%arrange(temporada2) %>% 
  mutate(OTUID=factor(OTUID, levels=OTUID)) %>% 
  ggplot(aes(OTUID, y = 1, fill= temporada2))+
  geom_tile()+
  theme_void()+theme(legend.position = "top")+
  annotate("text", x = 20, y = 1, label = "REPRODUCTIVA", size=3, color = "gray1",fontface="bold")+
  annotate("text", x = 50, y = 1, label = "NO REPRODUCTIVA", size=3, color = "gray1",fontface="bold")+
  #annotate("text", x = 61.5 , y = 1.1, label = "ASEPTIC CONTROL", size=3, color = "gray1",fontface="bold")+
  scale_fill_manual(values = c("#7d4741","#3d4761"))

h1

h1 <- h1 +  theme(legend.position = "none")+theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

h1



h2<-phylum_recto_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=phylum))+
  geom_col()+
  facet_nested(~ orden + temporada, nest_line = element_line(linetype = 2), scales = "free", space = "free") +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Phylum")) +
  scale_fill_manual(values = mycolors) +
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    #axis.text.y = element_text(color="black", size=9, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 7, family = "Helvetica"),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.box = "vertical",
    legend.position = "right",
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica"),
    legend.title = element_text(color="black", size=10, family = "Helvetica"), 
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())

h2


















#NIVEL CLASE
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras <- c("26AFR","26BFR","26CFR","26DFR","26EFR","26FP12R","26FP16R","26FP3R","26FP7R","26FP8R")

HEM.2600.REP.clase_10 <- otutable.HEM.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.REP.41 <- c("41AFR","41DFR","41FFR","41FR","41JFR","41FP16R","41FP17R","41FP18R","41FP19R","41FP20R")


HEM.4150.REP.clase_10 <- otutable.HEM.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26 <- c("261MR","262MR","263MR","264MR","265MR","26M14R","26M19R","26M2ScR","26M4ScR","26M6ScR")


MAC.2600.REP.clase_10 <- otutable.MAC.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MAC.REP.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)


#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41 <- c("4110MR","41EMR","41GMR","41IMR","41JMR","41M11R","41M12R","41M13R","41M14R","41M15R")


MAC.4150.REP.clase_10 <- otutable.MAC.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MAC.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26 <- c("26FV1R","26FV2R","26FV3R","26FV4R","26FV5R","26H2R","26H3R","26H4R","26H7R","26H8R")

HEM.2600.NREP.clase_10 <- otutable.HEM.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:4)


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41 <- c("41FV1R","41FV2R","41FV3R","41FV4R","41FV5R","H31R","H32R","H33R","H34R","H35R")

HEM.4150.NREP.clase_10 <- otutable.HEM.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26 <- c("26MVAR","26MVBR","26MVDR","26MVER","26M2R","26M5R","26M6R","26M9R")

MAC.2600.NREP.clase_10 <- otutable.MAC.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41 <- c("41MVAR","41MVBR","41MVCR","41MVDR","41MVER","M1R","M2R","M3R","M4R","M5R")

MAC.4150.NREP.clase_10 <- otutable.MAC.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)


#unir todo
clase_recto.all<- HEM.2600.REP.clase_10 %>% rownames_to_column(var = "id") %>%
  full_join(HEM.4150.REP.clase_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.REP.clase_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.REP.clase_10 %>% rownames_to_column(var="id")) %>%
  full_join(HEM.2600.NREP.clase_10 %>% rownames_to_column(var="id")) %>%
  full_join(HEM.4150.NREP.clase_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.NREP.clase_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.NREP.clase_10 %>% rownames_to_column(var="id"))

lista.clase.recto<- unique(clase_recto.all$id)

clase_recto.all_final<- gestacion.recto.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(c) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "c") %>%
  relabunda() %>%
  rownames_to_column(var = "clase") %>%
  filter(clase %in% lista.clase.recto)

#preparar archivo
clase_recto_plot<- clase_recto.all_final %>% 
  pivot_longer(cols=-clase, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.recto) %>%
  mutate(clase=str_replace(clase,"c__", "")) 
#mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
#labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%


#Especificar el orden en el que quiero que grafique 
clase_recto_plot$orden<- factor(clase_recto_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                         "Macho2600Rep", "Macho4150Rep",
                                                                         "Hembra2600NoRep","Hembra4150NoRep",
                                                                         "Macho2600NoRep", "Macho4150NoRep"))

#FIGURA CLASE
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.c <- 10
mycolors.c = colorRampPalette(brewer.pal(8, "Dark2")) (colors.c) 
mycolors.c[3]<- "#e6ab02" 
mycolors.c[4]<- "#7570b3" 

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)


cc.recto<- clase_recto_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=clase))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Clase")) +
  scale_fill_manual(values = mycolors.c) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    axis.text.y = element_text(size=8, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 8, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(color="black", size=10, face = "bold"),
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+theme(strip.background = element_blank(),
                                         strip.text.x = element_blank())+guides(fill=guide_legend(title = "Clase"))


cc.recto


#NIVEL FAMILIA
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras <- c("26AFR","26BFR","26CFR","26DFR","26EFR","26FP12R","26FP16R","26FP3R","26FP7R","26FP8R")

HEM.2600.REP.familia_10 <- otutable.HEM.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:9)


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.REP.41 <- c("41AFR","41DFR","41FFR","41FR","41JFR","41FP16R","41FP17R","41FP18R","41FP19R","41FP20R")


HEM.4150.REP.familia_10 <- otutable.HEM.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:8)


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26 <- c("261MR","262MR","263MR","264MR","265MR","26M14R","26M19R","26M2ScR","26M4ScR","26M6ScR")


MAC.2600.REP.familia_10 <- otutable.MAC.2600.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MAC.REP.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:9)



#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41 <- c("4110MR","41EMR","41GMR","41IMR","41JMR","41M11R","41M12R","41M13R","41M14R","41M15R")


MAC.4150.REP.familia_10 <- otutable.MAC.4150.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MAC.REP.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:8)


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26 <- c("26FV1R","26FV2R","26FV3R","26FV4R","26FV5R","26H2R","26H3R","26H4R","26H7R","26H8R")

HEM.2600.NREP.familia_10 <- otutable.HEM.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:11)


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41 <- c("41FV1R","41FV2R","41FV3R","41FV4R","41FV5R","H31R","H32R","H33R","H34R","H35R")

HEM.4150.NREP.familia_10 <- otutable.HEM.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:9)


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26 <- c("26MVAR","26MVBR","26MVDR","26MVER","26M2R","26M5R","26M6R","26M9R")

MAC.2600.NREP.familia_10 <- otutable.MAC.2600.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MNR.26, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:11)

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP <-data.frame(t(gestacion.recto.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.recto) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41 <- c("41MVAR","41MVBR","41MVCR","41MVDR","41MVER","M1R","M2R","M3R","M4R","M5R")

MAC.4150.NREP.familia_10 <- otutable.MAC.4150.NO.REP %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MNR.41, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>% 
  slice(1:10)


#unir todo
familia_recto.all<- HEM.2600.REP.familia_10 %>% rownames_to_column(var = "id") %>%
  full_join(HEM.4150.REP.familia_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.REP.familia_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.REP.familia_10 %>% rownames_to_column(var="id")) %>%
  full_join(HEM.2600.NREP.familia_10 %>% rownames_to_column(var="id")) %>%
  full_join(HEM.4150.NREP.familia_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.NREP.familia_10 %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.NREP.familia_10 %>% rownames_to_column(var="id"))

lista.familia.recto<- unique(familia_recto.all$id)

familia_recto.all_final<- gestacion.recto.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(f) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "f") %>%
  relabunda() %>%
  rownames_to_column(var = "familia") %>%
  filter(familia %in% lista.familia.recto)

#preparar archivo
familia_recto_plot<- familia_recto.all_final %>% 
  pivot_longer(cols=-familia, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.recto) %>%
  mutate(familia=str_replace(familia,"f__", "")) 
#mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
#labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%


#Especificar el orden en el que quiero que grafique 
familia_recto_plot$orden<- factor(familia_recto_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                       "Macho2600Rep", "Macho4150Rep",
                                                                       "Hembra2600NoRep","Hembra4150NoRep",
                                                                       "Macho2600NoRep", "Macho4150NoRep"))



#FIGURA FAMILIA
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.f <- 20
mycolors.f = colorRampPalette(brewer.pal(8, "Dark2")) (colors.f) 
#mycolors[11]<- "#a6761d" 
#mycolors[9]<- "#c22776" 
##f2a687 salmon
#e7298a rosa fosforescente

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)

library(ggpubr)

ff.recto<- familia_recto_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=familia))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Familia")) +
  #guides(fill= guide_legend(nrow = 3)) +
  scale_fill_manual(values = mycolors.f) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    axis.text.y = element_text(size=8, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 8, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(color="black", size=15, face = "bold"), 
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+
  theme(legend.text = element_text(size = 7))+theme(strip.background = element_blank(),
                                                    strip.text.x = element_blank())+guides(fill=guide_legend(title = "Familia"))+
   guides(fill= guide_legend(nrow = 3))

ff.recto




####BUENA
c
#barplot.final.ileon

#guardar
#ggsave("barplot.recto.tiff", width = 10, height = 6, dpi = 600, plot = barplot.final.recto, device = "tiff")




#####BARPLOT DE ILEON#######

#otu de ILEON
gestacion.ileon<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")


#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon")

#taxonomia
taxonomia<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#funcion para obtener abundancia relativa 
relabunda<- function(x){(as.data.frame(t(t(x)/colSums(x)))*100)}


#NIVEL PHYLUM
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.26.ileon <- c("26AFI","26BFI","26CFI","26DFI","26EFI","26FP12I","26FP16I","26FP3I","26FP7I","26FP8I")

HEM.2600.REP.phylum_ileon <- otutable.HEM.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HEM.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:4) 


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.41.ileon <- c("41AFI","41DFI","41FFI","41FI","41JFI","41FP16I","41FP17I","41FP18I","41FP19I","41FP20I")


HEM.4150.REP.phylum_ileon <- otutable.HEM.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HEM.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:3) 


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26.ileon <- c("261MI","262MI","263MI","264MI","265MI","26M14I","26M19I","26M2ScI","26M4ScI","26M6ScI")


MAC.2600.REP.phylum_ileon <- otutable.MAC.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MAC.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:3) 


#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41.ileon <- c("4110MI","41EMI","41GMI","41IMI","41JMI","41M11I","41M12I","41M13I","41M14I","41M15I")


MAC.4150.REP.phylum_ileon <- otutable.MAC.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MAC.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:3) 


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26.ileon <- c("26FV1I","26FV2I","26FV3I","26FV4I","26FV5I","26H2I","26H3I","26H4I","26H7I","26H8I")

HEM.2600.NREP.phylum_ileon <- otutable.HEM.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:2) 


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41.ileon <- c("41FV1I","41FV2I","41FV3I","41FV4I","41FV5I","H31I","H32I","H33I","H34I","H35I")

HEM.4150.NREP.phylum_ileon <- otutable.HEM.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.HNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:3) 


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26.ileon <- c("26MVAI","26MVBI","26MVDI","26MVEI","26M2I","26M5I","26M6I","26M9I")

MAC.2600.NREP.phylum_ileon <- otutable.MAC.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:3) 

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41.ileon <- c("41MVAI","41MVBI","41MVCI","41MVDI","41MVEI","M1I","M2I","M3I","M4I","M5I")

MAC.4150.NREP.phylum_ileon <- otutable.MAC.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(p) %>% 
  mutate_at(muestras.MNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "p") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean))  %>%
  slice(1:2) 


#unir todo
phylum_ileon.all<- HEM.2600.REP.phylum_ileon %>% rownames_to_column(var = "id") %>%
  full_join(HEM.4150.REP.phylum_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.REP.phylum_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.REP.phylum_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.2600.NREP.phylum_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.4150.NREP.phylum_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.NREP.phylum_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.NREP.phylum_ileon %>% rownames_to_column(var="id"))

lista.phylum.ileon<- unique(phylum_ileon.all$id)

phylum_ileon.all_final<- gestacion.ileon.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(p) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "p") %>%
  relabunda() %>%
  rownames_to_column(var = "phylum") %>%
  filter(phylum %in% lista.phylum.ileon)

#preparar archivo
phylum_ileon_plot<- phylum_ileon.all_final %>% 
  pivot_longer(cols=-phylum, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.ileon) %>%
  mutate(phylum=str_replace(phylum,"p__", "")) 
#mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
#labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%


#Especificar el orden en el que quiero que grafique 
phylum_ileon_plot$orden<- factor(phylum_ileon_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                         "Macho2600Rep", "Macho4150Rep",
                                                                         "Hembra2600NoRep","Hembra4150NoRep",
                                                                         "Macho2600NoRep", "Macho4150NoRep"))



#FIGURA PHYLUM
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.p.ile <- 10
mycolors.p.ile = colorRampPalette(brewer.pal(8, "Dark2")) (colors.p.ile) 
mycolors.p.ile[1]<- "#1b9e77"
mycolors.p.ile[2]<- "#a6761d"
mycolors.p.ile[3]<- "#7570B3"
mycolors.p.ile[4]<- "#E7298A"  

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)


pp.ileon<- phylum_ileon_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=phylum))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Phylum")) +
  scale_fill_manual(values = mycolors.p.ile) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    #axis.text.y = element_text(color="black", size=9, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 8, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(color="black", size=8, face = "bold"), 
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica", color = "white", face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+guides(fill=guide_legend(title = "Phylum"))

pp.ileon


#NIVEL CLASE
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.26.ileon <- c("26AFI","26BFI","26CFI","26DFI","26EFI","26FP12I","26FP16I","26FP3I","26FP7I","26FP8I")

HEM.2600.REP.clase_ileon <- otutable.HEM.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HEM.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:6)


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.41.ileon <- c("41AFI","41DFI","41FFI","41FI","41JFI","41FP16I","41FP17I","41FP18I","41FP19I","41FP20I")


HEM.4150.REP.clase_ileon <- otutable.HEM.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HEM.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:5)


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26.ileon <- c("261MI","262MI","263MI","264MI","265MI","26M14I","26M19I","26M2ScI","26M4ScI","26M6ScI")


MAC.2600.REP.clase_ileon <- otutable.MAC.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MAC.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:4)


#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41.ileon <- c("4110MI","41EMI","41GMI","41IMI","41JMI","41M11I","41M12I","41M13I","41M14I","41M15I")


MAC.4150.REP.clase_ileon <- otutable.MAC.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MAC.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:4)


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26.ileon <- c("26FV1I","26FV2I","26FV3I","26FV4I","26FV5I","26H2I","26H3I","26H4I","26H7I","26H8I")

HEM.2600.NREP.clase_ileon <- otutable.HEM.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:3) 


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41.ileon <- c("41FV1I","41FV2I","41FV3I","41FV4I","41FV5I","H31I","H32I","H33I","H34I","H35I")

HEM.4150.NREP.clase_ileon <- otutable.HEM.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.HNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:4)


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26.ileon <- c("26MVAI","26MVBI","26MVDI","26MVEI","26M2I","26M5I","26M6I","26M9I")

MAC.2600.NREP.clase_ileon <- otutable.MAC.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:4)

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41.ileon <- c("41MVAI","41MVBI","41MVCI","41MVDI","41MVEI","M1I","M2I","M3I","M4I","M5I")

MAC.4150.NREP.clase_ileon <- otutable.MAC.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(c) %>% 
  mutate_at(muestras.MNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "c") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:3)


#unir todo
clase_ileon.all<- HEM.2600.REP.clase_ileon %>% rownames_to_column(var = "id") %>%
  full_join(HEM.4150.REP.clase_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.REP.clase_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.REP.clase_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.2600.NREP.clase_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.4150.NREP.clase_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.NREP.clase_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.NREP.clase_ileon %>% rownames_to_column(var="id"))

lista.clase.ileon<- unique(clase_ileon.all$id)

clase_ileon.all_final<- gestacion.ileon.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(c) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "c") %>%
  relabunda() %>%
  rownames_to_column(var = "clase") %>%
  filter(clase %in% lista.clase.ileon)

#preparar archivo
clase_ileon_plot<- clase_ileon.all_final %>% 
  pivot_longer(cols=-clase, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.ileon) %>%
  mutate(clase=str_replace(clase,"c__", "")) 
#mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
#labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%


#Especificar el orden en el que quiero que grafique 
clase_ileon_plot$orden<- factor(clase_ileon_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                         "Macho2600Rep", "Macho4150Rep",
                                                                         "Hembra2600NoRep","Hembra4150NoRep",
                                                                         "Macho2600NoRep", "Macho4150NoRep"))



#FIGURA CLASE
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.c.ile <- 10
mycolors.c.ile = colorRampPalette(brewer.pal(8, "Dark2")) (colors.c.ile) 
mycolors.c.ile[3]<- "#e6ab02"
mycolors.c.ile[4]<- "#66a61e"
mycolors.c.ile[5]<- "#7570b3" 
mycolors.c.ile[6]<- "#e7298a"

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)


cc.ileon<- clase_ileon_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=clase))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Clase")) +
  scale_fill_manual(values = mycolors.c.ile) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    #axis.text.y = element_text(color="black", size=9, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 8, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(color="black", size=8, face = "bold"), 
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=7, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+theme(strip.background = element_blank(),
                                         strip.text.x = element_blank())+guides(fill=guide_legend(nrow = 1, title = "Clase"))


cc.ileon


#NIVEL FAMILIA
# separar tablas y escoger los 10 mas abundates de cada seccion.

#HEMBRA REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva") 

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.26.ileon <- c("26AFI","26BFI","26CFI","26DFI","26EFI","26FP12I","26FP16I","26FP3I","26FP7I","26FP8I")

HEM.2600.REP.familia_ileon <- otutable.HEM.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HEM.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:10)


#HEMBRA REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HEM.REP.41.ileon <- c("41AFI","41DFI","41FFI","41FI","41JFI","41FP16I","41FP17I","41FP18I","41FP19I","41FP20I")


HEM.4150.REP.familia_ileon <- otutable.HEM.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HEM.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:10)


#MACHO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.26.ileon <- c("261MI","262MI","263MI","264MI","265MI","26M14I","26M19I","26M2ScI","26M4ScI","26M6ScI")


MAC.2600.REP.familia_ileon <- otutable.MAC.2600.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MAC.REP.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:6)


#MACHO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MAC.REP.41.ileon <- c("4110MI","41EMI","41GMI","41IMI","41JMI","41M11I","41M12I","41M13I","41M14I","41M15I")


MAC.4150.REP.familia_ileon <- otutable.MAC.4150.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MAC.REP.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:8)


#HEMBRA NO REPRODUCTIVA A 2600 MSNM
otutable.HEM.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.26.ileon <- c("26FV1I","26FV2I","26FV3I","26FV4I","26FV5I","26H2I","26H3I","26H4I","26H7I","26H8I")

HEM.2600.NREP.familia_ileon <- otutable.HEM.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:5)


#HEMBRA NO REPRODUCTIVA A 4150 MSNM
otutable.HEM.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Hembra") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.HNR.41.ileon <- c("41FV1I","41FV2I","41FV3I","41FV4I","41FV5I","H31I","H32I","H33I","H34I","H35I")

HEM.4150.NREP.familia_ileon <- otutable.HEM.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.HNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:11)


#MACHO NO REPRODUCTIVO A 2600 MSNM
otutable.MAC.2600.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="2600 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.26.ileon <- c("26MVAI","26MVBI","26MVDI","26MVEI","26M2I","26M5I","26M6I","26M9I")

MAC.2600.NREP.familia_ileon <- otutable.MAC.2600.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MNR.26.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:9)

#MACHO NO REPRODUCTIVO A 4150 MSNM
otutable.MAC.4150.NO.REP.ileon <-data.frame(t(gestacion.ileon.filtrada), check.names = F, check.rows = F) %>% 
  rownames_to_column(var = "OTUID") %>% 
  inner_join(metadata.gestacion.ileon) %>% 
  filter(sexo=="Macho") %>% 
  filter(poblacion=="4150 msnm") %>% 
  filter(temporada=="No Reproductiva")

#Vector para especificar que todas estas columnas las haga datos numericos con mutate_at
muestras.MNR.41.ileon <- c("41MVAI","41MVBI","41MVCI","41MVDI","41MVEI","M1I","M2I","M3I","M4I","M5I")

MAC.4150.NREP.familia_ileon <- otutable.MAC.4150.NO.REP.ileon %>% dplyr::select(-seccion:-corrida) %>%
  column_to_rownames(var = "OTUID") %>% t() %>% 
  as.data.frame() %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>% 
  group_by(f) %>% 
  mutate_at(muestras.MNR.41.ileon, as.numeric) %>%
  summarise_if(is.numeric,sum) %>% 
  dplyr::select(-Confidence) %>% 
  column_to_rownames(var = "f") %>%
  relabunda() %>% 
  mutate(mean=rowMeans(.)) %>% 
  arrange(desc(mean)) %>%
  slice(1:8)


#unir todo
familia_ileon.all<- HEM.2600.REP.familia_ileon %>% rownames_to_column(var = "id") %>%
  full_join(HEM.4150.REP.familia_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.REP.familia_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.REP.familia_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.2600.NREP.familia_ileon %>% rownames_to_column(var="id")) %>%
  full_join(HEM.4150.NREP.familia_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.2600.NREP.familia_ileon %>% rownames_to_column(var="id")) %>% 
  full_join(MAC.4150.NREP.familia_ileon %>% rownames_to_column(var="id"))

lista.familia.ileon<- unique(familia_ileon.all$id)

familia_ileon.all_final<- gestacion.ileon.filtrada %>%
  rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia) %>% 
  separate(taxonomy, c("k", "p", "c", "o", "f", "g", "s"),sep=";") %>%
  group_by(f) %>% 
  summarise_if(is.numeric,sum) %>%
  dplyr::select(-Confidence) %>%
  column_to_rownames(var = "f") %>%
  relabunda() %>%
  rownames_to_column(var = "familia") %>%
  filter(familia %in% lista.familia.ileon) 

#preparar archivo
familia_ileon_plot<- familia_ileon.all_final %>% 
  pivot_longer(cols=-familia, names_to = "OTUID", values_to="realbunda") %>%
  inner_join(metadata.gestacion.ileon) %>%
  mutate(familia=str_replace(familia,"f__", "")) 
#mutate(Seccion2=factor(Seccion, levels = c("L.amniotico","Tracto.embrionario","Membrana","Yema","Boca","Cloaca","Ileon","Dorso"),
#labels = c("Amniotic fluid","Embryonic tract","Membrane","Yolk","Mouth","Cloaca","Small \n intestine","Aseptic ventral \n skin")))%>%


#Especificar el orden en el que quiero que grafique 
familia_ileon_plot$orden<- factor(familia_ileon_plot$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                       "Macho2600Rep", "Macho4150Rep",
                                                                       "Hembra2600NoRep","Hembra4150NoRep",
                                                                       "Macho2600NoRep", "Macho4150NoRep"))



#FIGURA CLASE
#definir colores para aumentar la n 
library(RColorBrewer)
library(ggh4x)

colors.f.ile <- 30
mycolors.f.ile = colorRampPalette(brewer.pal(8, "Dark2")) (colors.f.ile) 
#mycolors[8]<- "#a6761d" 
#mycolors[10]<- "#c22776" 
##f2a687 salmon
#e7298a rosa fosforescente

#figura
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")), by_layer_x = FALSE)


ff.ileon<- familia_ileon_plot %>% ggplot(aes(x=`OTUID`, y=realbunda, fill=familia))+
  geom_col()+ 
  facet_grid2(.~orden, scales = "free", space = "free",  strip = ridiculous_strips)+
  labs(y="Relative abundance (%)") +
  geom_bar(stat = "identity", position="stack", color="black", lwd=0.15) +    
  guides(fill= guide_legend(title = "Familia")) +
  scale_fill_manual(values = mycolors.f.ile) +
  theme_classic()+
  theme(## títutlos de los ejes
    ## texto en los ejes "x" y nùmeros en el eje "y":
    #axis.text.y = element_text(color="black", size=9, family = "Helvetica"),
    #axis.text.x = element_text(angle = 90, hjust = 0.5, colour = "black", size = 5.5, family = "Helvetica"),
    ## font de los géneros: legenda
    legend.text = element_text(colour = "black", size = 6, family = "Helvetica"),
    legend.key.size = unit(0.35, "cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(color="black", size=8, face = "bold"), 
    ## títutlos de los ejes
    axis.title.y = element_text (color="black", size=9, family = "Helvetica", hjust = 0.5, face = "bold",
                                 margin =margin(t=0, r=10, b=0, l=0)),
    #axis.title.x = element_text (color="black", size=9, family = "Helvetica", vjust = 0, hjust = 0.5, face = "bold"), 
    #titulo de facet
    strip.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.5, size = 7, family = "Helvetica"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x =element_blank())+theme(strip.background = element_blank(),
                                         strip.text.x = element_blank())+guides(fill=guide_legend(nrow=4, title = "Familia"))


ff.ileon

####BUENA
#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
#tiff(filename = "barplots_final.tiff", width=3750, height = 1870, res = 250)
barplot.final.ileon<-plot_grid(#h2+theme(plot.margin =unit(c(0,0,0,2), "cm")),
                               pp.ileon+theme(legend.title = element_text(size = 8),
                                              legend.text = element_text(size = 7),
                                              legend.box.spacing = unit(0, "pt"),
                                              plot.margin =unit(c(0,0,0,-0.1), "cm"),
                                              strip.text.x = element_blank()) + ylab(""),
                               cc.ileon+theme(legend.title = element_text(size = 8),
                                              legend.text = element_text(size = 7),
                                              legend.box.spacing = unit(0, "pt"),
                                              plot.margin =unit(c(0,0,0,-0.1), "cm"), 
                                              axis.title.y = element_text(vjust = 3, size = 12)),
                               ff.ileon+theme(legend.title = element_text(size = 8),
                                              legend.text = element_text(size = 7),
                                              legend.box.spacing = unit(0, "pt"),
                                              plot.margin =unit(c(0,0,-0.1,-0.1), "cm"))+ylab(""), 
                               ncol = 1, axis="r", align = "v",
                               #rel_heights = c(0.1,1.2,0.9,1),
                               scale = 0.95, hjust = -2.8, vjust = -0.3, label_size = 10)

barplot.final.ileon
barplot.final.recto

getwd()
ggsave("barplot.ileon2.pdf", width = 8, height = 5.5, dpi = 600, plot = barplot.final.ileon, device = "pdf")
ggsave("barplot.recto.pdf", width = 8, height = 5.5, dpi = 600, plot = barplot.final.recto, device = "pdf")



###########HEATMAP#############

###HEATMAP TAXÓNOMICO 

library(tidyverse)
library(pheatmap)
library(ALDEx2)
library(ComplexHeatmap)
library(grid)
library(gridExtra)
library(dplyr)
library(colorRamps)
library(circlize)
library(RColorBrewer)

#otu de recto
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#tabla inicial
phy.ra.completa.recto <- t(t(gestacion.recto.filtrada)/colSums(gestacion.recto.filtrada)*100) %>% as.data.frame()

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                        labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                       labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl")))


#data de features importantes
#feature.important.taxa <- read_tsv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/feature.important.taxa2.txt") 

#taxonomia
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#tabla 2
tabla_abundantes.recto<- phy.ra.completa.recto %>%
  mutate(abun= rowMeans(.)) %>% 
  rownames_to_column(var="OTUID") %>% 
  arrange(-abun) %>%
  slice(1:50) %>%
  inner_join(taxonomia.nueva) %>%
  separate(taxonomy, into = c("dominio", "phylum", "clase", 
                                               "orden", "familia",
                                               "genero", "especie"), sep = ";") %>% 
  mutate(taxa=str_extract(genero, "[^_]+$"))

#guardar y exportar para editar nombres
write.csv(tabla_abundantes.recto, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.recto.csv", row.names = F)


#modifiqué tabla en excel y luego volví a importarla con los nombres modificados
tabla_modificada.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.recto.csv", check.names = F) %>%  
  arrange(-abun) %>%
  slice(1:50) %>% 
  mutate(asv=paste0("ASV", rownames(.))) %>%
  mutate(phylum=str_replace(phylum,"p__", "")) %>%
  mutate(genero=str_replace(genero,"g__", "")) %>%
  mutate_if(is.character, str_trim) %>%
  unite("nombres", c("asv", "taxa"), remove = F)
  #left_join(feature.important.taxa) %>% 
  #mutate_at(c("importance"), ~replace(., is.na(.), 0)) %>% 
  #mutate(score=importance*100)


#escogí los nombres de las taxas
taxa.names.recto<- tabla_modificada.recto$taxa

#ordenar la tabla por origen y seccion uniendo con la metadata
heat.recto<- tabla_modificada.recto  %>% 
  column_to_rownames(var = "nombres") %>%
  select_if(is.numeric) %>%
  dplyr::select(-abun, -Confidence) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "OTUID") %>%
  inner_join(metadata.gestacion.recto, joining, by="OTUID") 


#Especificar el orden en el que quiero que grafique 
#heat.recto$orden<- factor(heat.recto$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
                                                                         #"Macho2600Rep", "Macho4150Rep",
                                                                         #"Hembra2600NoRep","Hembra4150NoRep",
                                                                         #"Macho2600NoRep", "Macho4150NoRep"))


heat.recto<- metadata.gestacion.recto %>% inner_join(heat.recto)%>%
  arrange(#factor(poblacion, levels = c("2600 masl","4150 masl")), 
          #factor(temporada, levels = c("Reproductive","No Reproductive")),
          factor(comparacion, levels = c("Hembra2600Rep","Hembra4150Rep",
                                          "Macho2600Rep", "Macho4150Rep",
                                         "Hembra2600NoRep","Hembra4150NoRep",
                                         "Macho2600NoRep", "Macho4150NoRep")))
          #factor(sexo, levels = c("Female","Male")))

#transformación de rangos
heatmap.recto<- heat.recto %>%
  column_to_rownames(var = "OTUID") %>%
  dplyr::select(`ASV1_Hafnia paralvei`:`ASV50_Akkermansia`) %>% 
  t() %>%
  as.data.frame() %>%
  mutate_all(., funs(R = case_when(
    . <= 0.001 ~ 0,
    . >  0.001 & .  <= 0.005 ~ 1,
    . >  0.005 & .  <= 0.01 ~ 2,
    . >  0.01 & .  <= 0.10 ~ 3,
    . >  0.10 & .  <= 0.20 ~ 4,
    . >  0.20 & .  <= 1.00 ~ 5,
    . >  1.00 & .  <= 2.00 ~ 6,
    . >  2.00 & .  <= 5.00 ~ 7,
    . >  5.00 & .  <= 10.00 ~ 8,
    . >  10.00 & .  <= 25.00 ~ 9,
    . >  25.00 & .  <= 50.00 ~ 10,
    . >  50.00 & .  <= 75.00 ~ 11,
    . >  75.00 ~ 12))) %>%
  select_at(vars(contains("_R"))) %>%
  select_all(~str_replace(., "_R", ""))

#AQUI SI COMPLEXHEATMAP
#heatmap paleta de colores del mapa de calor
my_palette <- viridis::viridis (n = 12, option = "C", direction = -1)

#annotatio row (filas)
annotation_rows.recto<- tabla_modificada.recto %>%
  dplyr::select(OTUID, phylum) %>%
  column_to_rownames(var = "OTUID")

rownames(annotation_rows.recto) <- rownames(heatmap.recto)

#annotatio seccion y origen (columnas)
annotation_columns.recto<- heat.recto %>%
  dplyr::select(poblacion, temporada, sexo, comparacion) 
rownames(annotation_columns.recto) <- colnames(heatmap.recto)

newnames <- lapply(taxa.names.recto,function(x) bquote(italic(.(x))))


c5.phylum.recto<- RColorBrewer::brewer.pal(8, "Set2") [1:6]
c7.altitud.recto<- c("#676778","#D9D9C2")
c8.temporada.recto<- c("#808000", "#1B5E20")
c2.sexo.recto<- c("#5D3277","#AF6502")
c1.comparacion.recto<- c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")


#paletas para anotaciones
cols_altitud.recto <- list('Altitude' = c("2600 masl" = "#676778", 
                                           "4150 masl"= "#D9D9C2"))


cols_temporada.recto <- list('Season' = c("Reproductive" = "#808000", 
                                          "No Reproductive"= "#1B5E20"))

cols_sexo.recto <- list("Sex"= c("Female" = "#5D3277",
                                 "Male" ="#AF6502"))

cols_comparacion.recto <- list("Sex2"= c("Hembra2600Rep" = "#5D3277",
                                         "Hembra4150Rep" = "#9265ff",
                                         "Macho2600Rep" = "#af6502",
                                         "Macho4150Rep" = "#ca7a04",
                                         "Hembra2600NoRep" = "#b38bff",
                                         "Hembra4150NoRep" = "#cfb1ff",
                                         "Macho2600NoRep" = "#e49007",
                                         "Macho4150NoRep" = "#fea509"))


 col_phyl.recto<- c5.phylum.recto
names(col_phyl.recto)<- unique(annotation_rows.recto$phylum)
cols_phyl.recto<-list(Phylum=col_phyl.recto)


annphylum = HeatmapAnnotation("Phylum" = annotation_rows.recto$phylum, 
                              which = "row", 
                              show_legend = F,   show_annotation_name = T,
                              annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                              annotation_legend_param = list(
                                title_gp = gpar(fontsize = 10, fontface="bold"),
                                labels_gp = gpar(fontsize = 10),
                                direction ="vertical"),
                              gp = gpar(col = "white"), col = cols_phyl.recto)


annaltitud = HeatmapAnnotation("Altitude" = annotation_columns.recto$poblacion, 
                               which = "column", 
                               show_legend = F, 
                               annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                               gp = gpar(col = "white"), 
                               show_annotation_name = T, col = cols_altitud.recto)

anntemporada = HeatmapAnnotation("Season" = annotation_columns.recto$temporada, 
                              which = "column", 
                              annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                              show_legend = F, 
                              gp = gpar(col = "white"), 
                              show_annotation_name = T, col = cols_temporada.recto)

annsexo = HeatmapAnnotation("Sex" = annotation_columns.recto$sexo, 
                            which = "column", 
                            annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                            show_legend = F,  
                            gp = gpar(col = "white"),
                            show_annotation_name = T, col = cols_sexo.recto)

anncompar = HeatmapAnnotation("Sex2" = annotation_columns.recto$comparacion, 
                            which = "column", 
                            annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                            show_legend = F,  
                            gp = gpar(col = "white"),
                            show_annotation_name = T, col = cols_comparacion.recto)

#annscore = HeatmapAnnotation("Score (%)" = annotation_rows$score, 
                             #which = "row", 
                             #show_legend = T,   show_annotation_name = T,
                             #annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                             #annotation_legend_param = list(
                               #title_gp = gpar(fontsize = 10, fontface="bold"),
                               #labels_gp = gpar(fontsize = 8),
                               #direction ="horizontal"),
                             #gp = gpar(col = "white"), col = list("Score (%)" = colorRamp2(c(0,2,4,6,8,10),
                                                                                           #c("#fde725","#7ad151","#22a884","#2a788e","#414487","#440154"))))  


lgd1.recto = Legend(at = unique(annotation_rows.recto$phylum), legend_gp = gpar(fill = c5.phylum.recto), title = "Phylum", labels_gp = gpar(fontsize=11))
lgd2.recto = Legend(at = sort(unique(annotation_columns.recto$poblacion)), legend_gp = gpar(fill = c7.altitud.recto), title = "Altitude", labels_gp = gpar(fontsize=11))
lgd3.recto = Legend(at = sort(unique(annotation_columns.recto$temporada)), legend_gp = gpar(fill = c8.temporada.recto), title = "Season", labels_gp = gpar(fontsize=11))
lgd4.recto = Legend(at = sort(unique(annotation_columns.recto$sexo)), legend_gp = gpar(fill = c2.sexo.recto), title = "Sex", labels_gp = gpar(fontsize=11))

pd.recto = packLegend(list = list(lgd1.recto, lgd2.recto, lgd3.recto, lgd4.recto))
#pd1=lgd4

#Funcion para hacer cursiva la letra 
make_face_names<-function(mat, rc_fun, rc_names_b = NA, 
                          rc_names_i = NA) {
  f_names <- rc_fun(mat)
  ids_b <- rc_names_b %>% match(rc_fun(mat))
  ids_i <- rc_names_i %>% match(rc_fun(mat))
  ids_bi <- rc_names_i %>% match(rc_fun(mat))
  
  ids_b %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  ids_i %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(italic(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  
  f_names
}

#Colocar el nombre que quieres que haga cursiva 
row_labels = make_face_names(heatmap, rownames,  
                             rc_names_i = c("ASV1_Curvibacter lanceolatus",
                                            "ASV2_Stenotrophomonas" ,
                                            "ASV3_Stenotrophomonas",
                                            "ASV4_Helicobacter",
                                            "ASV5_Pseudomonas" ,
                                            "ASV6_Stenotrophomonas rhizophila",
                                            "ASV7_Hafnia paralvei", 
                                            "ASV8_Brevundimonas",
                                            "ASV9_Salmonella" ,
                                            "ASV10_Ralstonia",
                                            "ASV11_Sphingobium yanoikuyae",
                                            "ASV12_Variovorax" ,
                                            "ASV13_Brevundimonas diminuta",
                                            "ASV14_Hafnia",
                                            "ASV15_Curvibacter lanceolatus",
                                            "ASV16_Hafnia" ,
                                            "ASV18_Stenotrophomonas rhizophila",
                                            "ASV19_Acinetobacter",
                                            "ASV21_Bradyrhizobium", 
                                            "ASV22_Arthrobacter",
                                            "ASV24_Streptococcus",
                                            "ASV25_Hafnia",
                                            "ASV26_Pseudomonas" ,
                                            "ASV27_Buchnera",
                                            "ASV28_Pseudomonas",
                                            "ASV30_Helicobacter rappini",
                                            "ASV33_Lachnoclostridium" ,
                                            "ASV35_Enterococcus",
                                            "ASV36_Sphingomas",
                                            "ASV37_Enterococcus",
                                            "ASV38_Bacteroides" ,
                                            "ASV39_Roseburia",
                                            "ASV41_Hungatella",
                                            "ASV43_Curvibacter",
                                            "ASV45_Bosea", 
                                            "ASV46_Citrobacter",
                                            "ASV47_Paeniclostridium",
                                            "ASV49_Acinetobacter schindleri",
                                            "ASV50_Hungatella" ))

#######HEATMAP RECTO#####
heats.recto<-Heatmap(heatmap.recto, col=my_palette,
               heatmap_legend_param = list(direction = "horizontal",
                                           labels_gp = gpar(fontsize = 7),
                                           legend_gp = gpar(fontsize = 9),
                                           title = "Relative abundance (%)",
                                           title_position = "topcenter",
                                           at = c(0,1,2,3,5,8,10,25, 50, 100),
                                           break_dist = 1),
               rect_gp = gpar(col = "black", lwd = 0.5),    
               row_names_gp =  gpar(fontsize=7),
               column_names_gp = gpar(fontsize=7),
               cluster_columns = F, cluster_rows = T,
               show_column_names = T,
               show_heatmap_legend = TRUE, 
               #row_labels = row_labels,
               top_annotation =  c(annaltitud, anntemporada, annsexo), 
               left_annotation = c(annphylum))

heats.recto
draw(heats.recto, heatmap_legend_side = "top",
     annotation_legend_side = "right", 
     merge_legend=F,
     annotation_legend_list = list(pd.recto))



heatm<-grid.grabExpr(draw(heats.recto, heatmap_legend_side = "top",
                          annotation_legend_side = "right", 
                          merge_legend=F,
                          annotation_legend_list = list(pd.recto)))

heatm

#Guardar imagen 
#tiff("heatmap2.tiff", width = 8.5, height = 8)
#print(heatm)
#dev.off()


#HEATMAP DE ILEON
#otu de ILEON
gestacion.ileon<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")


#tabla inicial
phy.ra.completa.ileon <- t(t(gestacion.ileon.filtrada)/colSums(gestacion.ileon.filtrada)*100) %>% as.data.frame()

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl")))


#data de features importantes
#feature.important.taxa <- read_tsv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/feature.important.taxa2.txt") 

#taxonomia
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#tabla 2
tabla_abundantes.ileon<- phy.ra.completa.ileon %>%
  mutate(abun= rowMeans(.)) %>% 
  rownames_to_column(var="OTUID") %>% 
  arrange(-abun) %>%
  slice(1:50) %>%
  inner_join(taxonomia.nueva) %>%
  separate(taxonomy, into = c("dominio", "phylum", "clase", 
                              "orden", "familia",
                              "genero", "especie"), sep = ";") %>% 
  mutate(taxa=str_extract(genero, "[^_]+$"))

#guardar y exportar para editar nombres
write.csv(tabla_abundantes.ileon, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.ileon.csv", row.names = F)


#modifiqué tabla en excel y luego volví a importarla con los nombres modificados
tabla_modificada.ileon<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.ileon.csv", check.names = F) %>%  
  arrange(-abun) %>%
  slice(1:50) %>% 
  mutate(asv=paste0("ASV", rownames(.))) %>%
  mutate(phylum=str_replace(phylum,"p__", "")) %>%
  mutate(genero=str_replace(genero,"g__", "")) %>%
  mutate_if(is.character, str_trim) %>%
  unite("nombres", c("asv", "taxa"), remove = F)
#left_join(feature.important.taxa) %>% 
#mutate_at(c("importance"), ~replace(., is.na(.), 0)) %>% 
#mutate(score=importance*100)


#escogí los nombres de las taxas
taxa.names.ileon<- tabla_modificada.ileon$taxa

#ordenar la tabla por origen y seccion uniendo con la metadata
heat.ileon<- tabla_modificada.ileon  %>% 
  column_to_rownames(var = "nombres") %>%
  select_if(is.numeric) %>%
  dplyr::select(-abun, -Confidence) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "OTUID") %>%
  inner_join(metadata.gestacion.ileon, joining, by="OTUID") 


#Especificar el orden en el que quiero que grafique 
#heat.recto$orden<- factor(heat.recto$comparacion, levels=c("Hembra2600Rep","Hembra4150Rep",
#"Macho2600Rep", "Macho4150Rep",
#"Hembra2600NoRep","Hembra4150NoRep",
#"Macho2600NoRep", "Macho4150NoRep"))


heat.ileon<- metadata.gestacion.ileon %>% inner_join(heat.ileon)%>%
  arrange(#factor(poblacion, levels = c("2600 masl","4150 masl")), 
    #factor(temporada, levels = c("Reproductive","No Reproductive")),
    factor(comparacion, levels = c("Hembra2600Rep","Hembra4150Rep",
                                   "Macho2600Rep", "Macho4150Rep",
                                   "Hembra2600NoRep","Hembra4150NoRep",
                                   "Macho2600NoRep", "Macho4150NoRep")))
#factor(sexo, levels = c("Female","Male")))

#transformación de rangos
heatmap.ileon<- heat.ileon %>%
  column_to_rownames(var = "OTUID") %>%
  dplyr::select(`ASV1_Pseudomonas`:`ASV50_Bacteroides`) %>% 
  t() %>%
  as.data.frame() %>%
  mutate_all(., funs(R = case_when(
    . <= 0.001 ~ 0,
    . >  0.001 & .  <= 0.005 ~ 1,
    . >  0.005 & .  <= 0.01 ~ 2,
    . >  0.01 & .  <= 0.10 ~ 3,
    . >  0.10 & .  <= 0.20 ~ 4,
    . >  0.20 & .  <= 1.00 ~ 5,
    . >  1.00 & .  <= 2.00 ~ 6,
    . >  2.00 & .  <= 5.00 ~ 7,
    . >  5.00 & .  <= 10.00 ~ 8,
    . >  10.00 & .  <= 25.00 ~ 9,
    . >  25.00 & .  <= 50.00 ~ 10,
    . >  50.00 & .  <= 75.00 ~ 11,
    . >  75.00 ~ 12))) %>%
  select_at(vars(contains("_R"))) %>%
  select_all(~str_replace(., "_R", ""))

#AQUI SI COMPLEXHEATMAP
#heatmap paleta de colores del mapa de calor
my_palette <- viridis::viridis (n = 12, option = "C", direction = -1)

#annotatio row (filas)
annotation_rows.ileon<- tabla_modificada.ileon %>%
  dplyr::select(OTUID, phylum) %>%
  column_to_rownames(var = "OTUID")

rownames(annotation_rows.ileon) <- rownames(heatmap.ileon)

#annotatio seccion y origen (columnas)
annotation_columns.ileon<- heat.ileon %>%
  dplyr::select(poblacion, temporada, sexo, comparacion) 
rownames(annotation_columns.ileon) <- colnames(heatmap.ileon)

newnames <- lapply(taxa.names.ileon,function(x) bquote(italic(.(x))))


c5.phylum.ileon<- RColorBrewer::brewer.pal(8, "Set2") [1:4]
c7.altitud.ileon<- c("#676778","#D9D9C2")
c8.temporada.ileon<- c("#0E6251", "#1B5E20")
c2.sexo.ileon<- c("#5D3277","#AF6502")
c1.comparacion.ileon<- c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")


#paletas para anotaciones
cols_altitud.ileon <- list('Altitude' = c("2600 masl" = "#676778", 
                                          "4150 masl"= "#D9D9C2"))


cols_temporada.ileon <- list('Season' = c("Reproductive" = "#0E6251", 
                                          "No Reproductive"= "#1B5E20"))

cols_sexo.ileon <- list("Sex"= c("Female" = "#5D3277",
                                 "Male" ="#AF6502"))

cols_comparacion.ileon <- list("Sex2"= c("Hembra2600Rep" = "#5D3277",
                                         "Hembra4150Rep" = "#9265ff",
                                         "Macho2600Rep" = "#af6502",
                                         "Macho4150Rep" = "#ca7a04",
                                         "Hembra2600NoRep" = "#b38bff",
                                         "Hembra4150NoRep" = "#cfb1ff",
                                         "Macho2600NoRep" = "#e49007",
                                         "Macho4150NoRep" = "#fea509"))


col_phyl.ileon<- c5.phylum.ileon
names(col_phyl.ileon)<- unique(annotation_rows.ileon$phylum)
cols_phyl.ileon<-list(Phylum=col_phyl.ileon)


annphylum.ileon = HeatmapAnnotation("Phylum" = annotation_rows.ileon$phylum, 
                              which = "row", 
                              show_legend = F,   show_annotation_name = T,
                              annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                              annotation_legend_param = list(
                                title_gp = gpar(fontsize = 10, fontface="bold"),
                                labels_gp = gpar(fontsize = 10),
                                direction ="vertical"),
                              gp = gpar(col = "white"), col = cols_phyl.ileon)


annaltitud.ileon = HeatmapAnnotation("Altitude" = annotation_columns.ileon$poblacion, 
                               which = "column", 
                               show_legend = F, 
                               annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                               gp = gpar(col = "white"), 
                               show_annotation_name = T, col = cols_altitud.ileon)

anntemporada.ileon = HeatmapAnnotation("Season" = annotation_columns.ileon$temporada, 
                                 which = "column", 
                                 annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                                 show_legend = F, 
                                 gp = gpar(col = "white"), 
                                 show_annotation_name = T, col = cols_temporada.ileon)

annsexo.ileon = HeatmapAnnotation("Sex" = annotation_columns.ileon$sexo, 
                            which = "column", 
                            annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                            show_legend = F,  
                            gp = gpar(col = "white"),
                            show_annotation_name = T, col = cols_sexo.ileon)

anncompar.ileon = HeatmapAnnotation("Sex2" = annotation_columns.ileon$comparacion, 
                              which = "column", 
                              annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                              show_legend = F,  
                              gp = gpar(col = "white"),
                              show_annotation_name = T, col = cols_comparacion.ileon)


lgd1.ileon = Legend(at = unique(annotation_rows.ileon$phylum), legend_gp = gpar(fill = c5.phylum.ileon), title = "Phylum", labels_gp = gpar(fontsize=11))
lgd2.ileon = Legend(at = sort(unique(annotation_columns.ileon$poblacion)), legend_gp = gpar(fill = c7.altitud.ileon), title = "Altitude", labels_gp = gpar(fontsize=11))
lgd3.ileon = Legend(at = sort(unique(annotation_columns.ileon$temporada)), legend_gp = gpar(fill = c8.temporada.ileon), title = "Season", labels_gp = gpar(fontsize=11))
lgd4.ileon = Legend(at = sort(unique(annotation_columns.ileon$sexo)), legend_gp = gpar(fill = c2.sexo.ileon), title = "Sex", labels_gp = gpar(fontsize=11))

pd.ileon = packLegend(list = list(lgd1.ileon, lgd2.ileon, lgd3.ileon, lgd4.ileon))
#pd1=lgd4

#Funcion para hacer cursiva la letra 
make_face_names<-function(mat, rc_fun, rc_names_b = NA, 
                          rc_names_i = NA) {
  f_names <- rc_fun(mat)
  ids_b <- rc_names_b %>% match(rc_fun(mat))
  ids_i <- rc_names_i %>% match(rc_fun(mat))
  ids_bi <- rc_names_i %>% match(rc_fun(mat))
  
  ids_b %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  ids_i %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(italic(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  
  f_names
}

#Colocar el nombre que quieres que haga cursiva 
row_labels = make_face_names(heatmap, rownames,  
                             rc_names_i = c("ASV1_Curvibacter lanceolatus",
                                            "ASV2_Stenotrophomonas" ,
                                            "ASV3_Stenotrophomonas",
                                            "ASV4_Helicobacter",
                                            "ASV5_Pseudomonas" ,
                                            "ASV6_Stenotrophomonas rhizophila",
                                            "ASV7_Hafnia paralvei", 
                                            "ASV8_Brevundimonas",
                                            "ASV9_Salmonella" ,
                                            "ASV10_Ralstonia",
                                            "ASV11_Sphingobium yanoikuyae",
                                            "ASV12_Variovorax" ,
                                            "ASV13_Brevundimonas diminuta",
                                            "ASV14_Hafnia",
                                            "ASV15_Curvibacter lanceolatus",
                                            "ASV16_Hafnia" ,
                                            "ASV18_Stenotrophomonas rhizophila",
                                            "ASV19_Acinetobacter",
                                            "ASV21_Bradyrhizobium", 
                                            "ASV22_Arthrobacter",
                                            "ASV24_Streptococcus",
                                            "ASV25_Hafnia",
                                            "ASV26_Pseudomonas" ,
                                            "ASV27_Buchnera",
                                            "ASV28_Pseudomonas",
                                            "ASV30_Helicobacter rappini",
                                            "ASV33_Lachnoclostridium" ,
                                            "ASV35_Enterococcus",
                                            "ASV36_Sphingomas",
                                            "ASV37_Enterococcus",
                                            "ASV38_Bacteroides" ,
                                            "ASV39_Roseburia",
                                            "ASV41_Hungatella",
                                            "ASV43_Curvibacter",
                                            "ASV45_Bosea", 
                                            "ASV46_Citrobacter",
                                            "ASV47_Paeniclostridium",
                                            "ASV49_Acinetobacter schindleri",
                                            "ASV50_Hungatella" ))

#######HEATMAP ILEON#######
heats.ileon<-Heatmap(heatmap.ileon, col=my_palette,
                     heatmap_legend_param = list(direction = "horizontal",
                                                 labels_gp = gpar(fontsize = 7),
                                                 legend_gp = gpar(fontsize = 9),
                                                 title = "Relative abundance (%)",
                                                 title_position = "topcenter",
                                                 at = c(0,1,2,3,5,8,10,25, 50, 100),
                                                 break_dist = 1),
                     rect_gp = gpar(col = "black", lwd = 0.5),    
                     row_names_gp =  gpar(fontsize=7),
                     column_names_gp = gpar(fontsize=7),
                     cluster_columns = F, cluster_rows = T,
                     show_column_names = T,
                     show_heatmap_legend = TRUE, 
                     #row_labels = row_labels,
                     top_annotation =  c(annaltitud.ileon, anntemporada.ileon, annsexo.ileon), 
                     left_annotation = c(annphylum.ileon))

heats.ileon
draw(heats.ileon, heatmap_legend_side = "top",
     annotation_legend_side = "right", 
     merge_legend=F,
     annotation_legend_list = list(pd.ileon))



heatm.ileon<-grid.grabExpr(draw(heats.ileon, heatmap_legend_side = "top",
                          annotation_legend_side = "right", 
                          merge_legend=F,
                          annotation_legend_list = list(pd.ileon)))

heatm.ileon

#Guardar imagen 
#tiff("heatmap2.tiff", width = 8.5, height = 8)
#print(heatm)
#dev.off()


#HEATMAP COMPLETO

#OTU DE RECTO
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#OTU DE ILEON
gestacion.ileon<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.ileon2.txt", check.names = FALSE)

#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")

#especificar que la columna se llama rownames
gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% rownames_to_column(var = "OTUID")
gestacion.recto.filtrada <- gestacion.recto.filtrada %>% rownames_to_column(var = "OTUID")

#unir las dos otus anteriores 
gestacion.ileon.recto <- gestacion.ileon.filtrada %>%
                         full_join(gestacion.recto.filtrada, by= c("OTUID")) %>%
                         column_to_rownames(var = "OTUID") %>%
                         mutate_all(~replace(., is.na(.), 0))

#tabla inicial
phy.ra.completa.total <- t(t(gestacion.ileon.recto)/colSums(gestacion.ileon.recto)*100) %>% as.data.frame()

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset para eliminar estomago y tener ileon y recto 
metadata.gestacion.total<- metadata_gestacion %>% filter(!seccion=="Estomago") %>%
  mutate(sexo=factor(sexo, levels = c("Hembra","Macho"),
                     labels = c("Female","Male"))) %>%
  mutate(temporada=factor(temporada, levels = c("Reproductiva","No Reproductiva"),
                          labels = c("Reproductive","No Reproductive"))) %>%
  mutate(seccion=factor(seccion, levels = c("Ileon","Recto"),
                          labels = c("Small intestine","Rectum"))) %>%
  mutate(poblacion=factor(poblacion, levels = c("2600 msnm","4150 msnm"),
                          labels = c("2600 masl","4150 masl")))


#data de features importantes
#feature.important.taxa <- read_tsv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/feature.important.taxa2.txt") 

#taxonomia
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#tabla 2
tabla_abundantes.total<- phy.ra.completa.total %>%
  mutate(abun= rowMeans(.)) %>% 
  rownames_to_column(var="OTUID") %>% 
  arrange(-abun) %>%
  slice(1:50) %>%
  inner_join(taxonomia.nueva) %>%
  separate(taxonomy, into = c("dominio", "phylum", "clase", 
                              "orden", "familia",
                              "genero", "especie"), sep = ";") %>% 
  mutate(taxa=str_extract(genero, "[^_]+$"))

#guardar y exportar para editar nombres
write.csv(tabla_abundantes.total, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.total.csv", row.names = F)


#modifiqué tabla en excel y luego volví a importarla con los nombres modificados
tabla_modificada.total <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/tabla.abundates.total.csv", check.names = F) %>%  
  arrange(-abun) %>%
  slice(1:50) %>% 
  mutate(asv=paste0("ASV", rownames(.))) %>%
  mutate(phylum=str_replace(phylum,"p__", "")) %>%
  mutate(genero=str_replace(genero,"g__", "")) %>%
  mutate_if(is.character, str_trim) %>%
  unite("nombres", c("asv", "taxa"), remove = F)
#left_join(feature.important.taxa) %>% 
#mutate_at(c("importance"), ~replace(., is.na(.), 0)) %>% 
#mutate(score=importance*100)


#escogí los nombres de las taxas
taxa.names.total<- tabla_modificada.total$taxa

#ordenar la tabla y unir con la metadata
heat.total<- tabla_modificada.total  %>% 
  column_to_rownames(var = "nombres") %>%
  select_if(is.numeric) %>%
  dplyr::select(-abun, -Confidence) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "OTUID") %>%
  inner_join(metadata.gestacion.total, joining, by="OTUID") 


heat.total<- metadata.gestacion.total %>% inner_join(heat.total)%>%
  arrange(factor(seccion, levels = c("Small intestine","Rectum")),
          #factor(poblacion, levels = c("2600 masl","4150 masl")), 
          #factor(temporada, levels = c("Reproductive","No Reproductive")),
    factor(comparacion, levels = c("Hembra2600Rep","Hembra4150Rep",
                                   "Macho2600Rep", "Macho4150Rep",
                                   "Hembra2600NoRep","Hembra4150NoRep",
                                   "Macho2600NoRep", "Macho4150NoRep")))
          #factor(sexo, levels = c("Female","Male")))


#transformación de rangos
heatmap.total<- heat.total %>%
  column_to_rownames(var = "OTUID") %>%
  dplyr::select(`ASV1_Hafnia paralvei`:`ASV50_Alistipes`) %>% 
  t() %>%
  as.data.frame() %>%
  mutate_all(., funs(R = case_when(
    . <= 0.001 ~ 0,
    . >  0.001 & .  <= 0.005 ~ 1,
    . >  0.005 & .  <= 0.01 ~ 2,
    . >  0.01 & .  <= 0.10 ~ 3,
    . >  0.10 & .  <= 0.20 ~ 4,
    . >  0.20 & .  <= 1.00 ~ 5,
    . >  1.00 & .  <= 2.00 ~ 6,
    . >  2.00 & .  <= 5.00 ~ 7,
    . >  5.00 & .  <= 10.00 ~ 8,
    . >  10.00 & .  <= 25.00 ~ 9,
    . >  25.00 & .  <= 50.00 ~ 10,
    . >  50.00 & .  <= 75.00 ~ 11,
    . >  75.00 ~ 12))) %>%
  select_at(vars(contains("_R"))) %>%
  select_all(~str_replace(., "_R", ""))

#AQUI SI COMPLEXHEATMAP
#heatmap paleta de colores del mapa de calor
my_palette <- viridis::viridis (n = 12, option = "C", direction = -1)

#annotation (filas)
annotation_rows.total<- tabla_modificada.total %>%
  dplyr::select(OTUID, phylum) %>%
  column_to_rownames(var = "OTUID")
rownames(annotation_rows.total) <- rownames(heatmap.total)

#annotation (columnas)
annotation_columns.total<- heat.total %>%
  dplyr::select(seccion, poblacion, temporada, sexo, comparacion) 
rownames(annotation_columns.total) <- colnames(heatmap.total)

newnames <- lapply(taxa.names.total,function(x) bquote(italic(.(x))))


c5.phylum.total<- RColorBrewer::brewer.pal(8, "Set2") [1:5]
c9.seccion.total<- c("#7258a0","#8d176f")
c7.altitud.total<- c("#676778","#D9D9C2")
c8.temporada.total<- c("#808000", "#1B5E20")
c2.sexo.total<- c("#5D3277","#AF6502")
c1.comparacion.total<- c("#673dff", "#9265ff", "#af6502","#ca7a04", "#b38bff", "#cfb1ff","#e49007","#fea509")

#paletas para anotaciones
cols_seccion.total <- list('Section' = c("Small intestine" = "#7258a0", #423C6D, #4B0C3B
                                         "Rectum"= "#8d176f"))

cols_altitud.total <- list('Altitude' = c("2600 masl" = "#676778", 
                                          "4150 masl"= "#D9D9C2"))


cols_temporada.total <- list('Season' = c("Reproductive" = "#808000", 
                                          "No Reproductive"= "#1B5E20"))

cols_sexo.total<- list("Sex"= c("Female" = "#5D3277",
                                 "Male" ="#AF6502"))


cols_comparacion.total <- list("Sex2"= c("Hembra2600Rep" = "#5D3277",
                                         "Hembra4150Rep" = "#9265ff",
                                         "Macho2600Rep" = "#af6502",
                                         "Macho4150Rep" = "#ca7a04",
                                         "Hembra2600NoRep" = "#b38bff",
                                         "Hembra4150NoRep" = "#cfb1ff",
                                         "Macho2600NoRep" = "#e49007",
                                         "Macho4150NoRep" = "#fea509"))



col_phyl.total<- c5.phylum.total
names(col_phyl.total)<- unique(annotation_rows.total$phylum)
cols_phyl.total<-list(Phylum=col_phyl.total)


annphylum = HeatmapAnnotation("Phylum" = annotation_rows.total$phylum, 
                              which = "row", 
                              show_legend = F,   show_annotation_name = T,
                              annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                              annotation_legend_param = list(
                                title_gp = gpar(fontsize = 8, fontface="bold"),
                                labels_gp = gpar(fontsize = 8),
                                direction ="vertical"),
                              gp = gpar(col = "white"), col = cols_phyl.total)

annseccion = HeatmapAnnotation("Section" = annotation_columns.total$seccion, 
                               which = "column", 
                               show_legend = F, 
                               annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                               gp = gpar(col = "white"), 
                               show_annotation_name = T, col = cols_seccion.total)


annaltitud = HeatmapAnnotation("Altitude" = annotation_columns.total$poblacion, 
                               which = "column", 
                               show_legend = F, 
                               annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                               gp = gpar(col = "white"), 
                               show_annotation_name = T, col = cols_altitud.total)

anntemporada = HeatmapAnnotation("Season" = annotation_columns.total$temporada, 
                                 which = "column", 
                                 annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                                 show_legend = F, 
                                 gp = gpar(col = "white"), 
                                 show_annotation_name = T, col = cols_temporada.total)

annsexo = HeatmapAnnotation("Sex" = annotation_columns.total$sexo, 
                            which = "column", 
                            annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                            show_legend = F,  
                            gp = gpar(col = "white"),
                            show_annotation_name = T, col = cols_sexo.total)



anncompar = HeatmapAnnotation("Sex2" = annotation_columns.total$comparacion, 
                              which = "column", 
                              annotation_name_gp =gpar(fontsize = 11, fontface="bold"),
                              show_legend = F,  
                              gp = gpar(col = "white"),
                              show_annotation_name = T, col = cols_comparacion.total)


#leyendas
lgd1.total = Legend(at = unique(annotation_rows.total$phylum), legend_gp = gpar(fill = c5.phylum.total), title = "Phylum", labels_gp = gpar(fontsize=7))
lgd2.total = Legend(at = sort(unique(annotation_columns.total$seccion)), legend_gp = gpar(fill = c9.seccion.total), title = "Section", labels_gp = gpar(fontsize=7))
lgd3.total = Legend(at = sort(unique(annotation_columns.total$poblacion)), legend_gp = gpar(fill = c7.altitud.total), title = "Altitude", labels_gp = gpar(fontsize=7))
lgd4.total = Legend(at = sort(unique(annotation_columns.total$temporada)), legend_gp = gpar(fill = c8.temporada.total), title = "Season", labels_gp = gpar(fontsize=7))
lgd5.total = Legend(at = sort(unique(annotation_columns.total$sexo)), legend_gp = gpar(fill = c2.sexo.total), title = "Sex", labels_gp = gpar(fontsize=7))

pd.total = packLegend(list = list(lgd1.total, lgd2.total, lgd3.total, lgd4.total, lgd5.total))
#pd1=lgd4

#Funcion para hacer cursiva la letra 
make_face_names<-function(mat, rc_fun, rc_names_b = NA, 
                          rc_names_i = NA) {
  f_names <- rc_fun(mat)
  ids_b <- rc_names_b %>% match(rc_fun(mat))
  ids_i <- rc_names_i %>% match(rc_fun(mat))
  ids_bi <- rc_names_i %>% match(rc_fun(mat))
  
  ids_b %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  ids_i %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(italic(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  
  f_names
}

#Colocar el nombre que quieres que haga cursiva 
row_labels = make_face_names(heatmap, rownames,  
                             rc_names_i = c("ASV1_Curvibacter lanceolatus",
                                            "ASV2_Stenotrophomonas" ,
                                            "ASV3_Stenotrophomonas",
                                            "ASV4_Helicobacter",
                                            "ASV5_Pseudomonas" ,
                                            "ASV6_Stenotrophomonas rhizophila",
                                            "ASV7_Hafnia paralvei", 
                                            "ASV8_Brevundimonas",
                                            "ASV9_Salmonella" ,
                                            "ASV10_Ralstonia",
                                            "ASV11_Sphingobium yanoikuyae",
                                            "ASV12_Variovorax" ,
                                            "ASV13_Brevundimonas diminuta",
                                            "ASV14_Hafnia",
                                            "ASV15_Curvibacter lanceolatus",
                                            "ASV16_Hafnia" ,
                                            "ASV18_Stenotrophomonas rhizophila",
                                            "ASV19_Acinetobacter",
                                            "ASV21_Bradyrhizobium", 
                                            "ASV22_Arthrobacter",
                                            "ASV24_Streptococcus",
                                            "ASV25_Hafnia",
                                            "ASV26_Pseudomonas" ,
                                            "ASV27_Buchnera",
                                            "ASV28_Pseudomonas",
                                            "ASV30_Helicobacter rappini",
                                            "ASV33_Lachnoclostridium" ,
                                            "ASV35_Enterococcus",
                                            "ASV36_Sphingomas",
                                            "ASV37_Enterococcus",
                                            "ASV38_Bacteroides" ,
                                            "ASV39_Roseburia",
                                            "ASV41_Hungatella",
                                            "ASV43_Curvibacter",
                                            "ASV45_Bosea", 
                                            "ASV46_Citrobacter",
                                            "ASV47_Paeniclostridium",
                                            "ASV49_Acinetobacter schindleri",
                                            "ASV50_Hungatella" ))

#####HEATMAP COMPLETO#######
heats.total<-Heatmap(heatmap.total, col=my_palette,
                     heatmap_legend_param = list(direction = "horizontal",
                                                 labels_gp = gpar(fontsize = 7),
                                                 legend_gp = gpar(fontsize = 7),
                                                 title = "Relative abundance (%)",
                                                 title_position = "topcenter",
                                                 at = c(0,1,2,3,5,8,10,25, 50, 100),
                                                 break_dist = 1),
                     rect_gp = gpar(col = "#3b302a", lwd = 0.5),    
                     row_names_gp =  gpar(fontsize=6),
                     column_names_gp = gpar(fontsize=6),
                     cluster_columns = F, cluster_rows = T,
                     show_column_names = T,
                     show_heatmap_legend = TRUE, 
                     #row_labels = row_labels,
                     top_annotation =  c(annseccion, annaltitud, anntemporada, annsexo), 
                     left_annotation = c(annphylum))

heats.total
draw(heats.total, heatmap_legend_side = "top",
     annotation_legend_side = "right", 
     merge_legend=F,
     annotation_legend_list = list(pd.total))



heatm.total<-grid.grabExpr(draw(heats.total, heatmap_legend_side = "top",
                          annotation_legend_side = "right", 
                          merge_legend=F,
                          annotation_legend_list = list(pd.total)))

heatm.total

#Guardar imagen 
#tiff("heatmap2.tiff", width = 8.5, height = 8)
#print(heatm)
#dev.off()



heats.ileon
heats.recto
heats.total





























































































#HEATMAP A NIVEL DE ASV (50 MÁS ABUNDATES) con abundacia relativa
#Transformar a abundancia relativa 
recto.relative <- microbiome::transform(x = gestacion.recto.filtrada, transform = "compositional")
#Convertir a porcentaje
recto.relative <- recto.relative * 100
colSums(recto.relative)
#Elegir los top50
ps <- phyloseq(otu_table(recto.relative, taxa_are_rows = FALSE))
top50asvs <- sort(taxa_sums(ps), TRUE)[1:50]
ps.relative <- prune_taxa(taxa = names(top50asvs), ps)
View(tax_table(gpt.relative))
#Plot
plot_heatmap(ps.relative, sample.label="OTUID", low="#000033", high="#CCFF66", #sample.order = order.muestras,
             title = "Abundancia relativa de las 50 ASV's más abundantes")





#####ALDEX#############

library(tidyverse)
library(ALDEx2)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)

#otu de ILEON
gestacion.ileon<- read.csv("gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.ileon.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de ileon
gestacion.ileon.sin0 <- gestacion.ileon%>% filter(rowSums(across(where(is.numeric)))!=0) 

#asvs a filtrar
asvs.unassigned.ileon<- read.delim("gestacion/asvs.filtrar.ileon2.txt", check.names = FALSE)


#filtrar de la otu, los 105 ids anteriores
gestacion.ileon.filtrada <- gestacion.ileon.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.ileon$OTUID)

gestacion.ileon.filtrada <- gestacion.ileon.filtrada %>% column_to_rownames(var = "OTUID")

#write.csv(gestacion.ileon.filtrada, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/gestacion.ileon.filtrada.csv", row.names = F)

#Importar metadata
metadata_gestacion <- read.csv("gestacion/metadata.gestacion.csv", check.names = F)
#subset de ileon
metadata.gestacion.ileon<- metadata_gestacion %>% filter(seccion=="Ileon")

#Cambiar a factor, la variable poblacion
metadata.gestacion.ileon$poblacion <- factor(metadata.gestacion.ileon$poblacion)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable temporada
metadata.gestacion.ileon$temporada <- factor(metadata.gestacion.ileon$temporada)
str(metadata.gestacion.ileon)

#Cambiar a factor, la variable sexo
metadata.gestacion.ileon$sexo <- factor(metadata.gestacion.ileon$sexo)
str(metadata.gestacion.ileon)


#ILEON
#Comparaciones pareadas
#2600 vs 4150 msnm 
otu.aldex.poblacion.ileon<- read_csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/otu.aldex.poblacion.ileon.csv") %>% column_to_rownames(var = "OTUID")
conds.poblacion.ileon<- c(rep("2600masl", 40), rep("4150masl", 40)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_poblacion.ileon<- aldex(reads = otu.aldex.poblacion.ileon, conditions = conds.poblacion.ileon, mc.samples = 1000,
                                      effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_poblacion.ileon.filter<-aldex_poblacion.ileon %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_poblacion.ileon.filter.pcor<-aldex_poblacion.ileon %>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.poblacion.ileon<- aldex_poblacion.ileon.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "2600masl",
    . >  0 ~ "4150masl"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="bacterium"  ~ "Bacteroides",
                                            taxa=="Lachnospiraceae"  ~ "Lachnospiraceae", TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.poblacion.ileon<- aldex_plot.poblacion.ileon %>% dplyr::select(taxa, "2600 masl"= rab.win.2600masl, "4150 masl"= rab.win.4150masl) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
column_to_rownames(var = "taxa")

#Tabla para el heatmap. Util cuando hay duplicados 
#heat.poblacion.ileon <- aldex_plot.poblacion.ileon %>% mutate(taxa2= paste0(rownames(heat.poblacion.ileon),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
  #dplyr::select(taxa2, "2600"= rab.win.2600, "4150"= rab.win.4150) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
  #column_to_rownames(var = "taxa2")


#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col = structure(c("#676778","#D9D9C2"), #COLORES DE BARRAS 
                          names = c("2600masl", "4150masl"))
#Anotacion de las barras
barpl = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.poblacion.ileon$diff.btw, which = "row",
  gp = gpar(fill = treatment_col[aldex_plot.poblacion.ileon$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#capa de valor p
cols_pvalue <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))
#Anotacion del valor p 
annP = HeatmapAnnotation("p-value" =aldex_plot.poblacion.ileon$p.value, 
                         simple_anno_size = unit(0.45, "cm"),
                         annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                         which = 'row',
                         annotation_legend_param = list(
                           title_gp = gpar(fontsize = 8, fontface="bold"),
                           labels_gp = gpar(fontsize = 8),
                           direction ="vertical"),
                         col = cols_pvalue,
                         show_legend = T, gp = gpar(col = "white"), 
                         show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen"))
  #c("#B7C68B", "#DED29E", "#B3A580"))
#Anotacion del tamaño del efecto
annE = rowAnnotation("Effect size" = aldex_plot.poblacion.ileon$effect, 
                     annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                     col = list("Effect size" = effect_col_fun),
                     simple_anno_size = unit(0.45, "cm"),
                     annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                    fontface="bold"),
                                                    labels_gp = gpar(fontsize = 8),
                                                    direction ="vertical"),
                     show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun=colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))

#cambio de color del heatmap
poblacion.ileon.color_heatmap = colorRamp2(seq(min(heat.poblacion.ileon), max(heat.poblacion.ileon), length= 3), c("#cbdbcd", "#759c8a", "#00544d"))
HEAT.POBLACION.ILEON<-Heatmap(heat.poblacion.ileon, cluster_rows = F,
                             cluster_columns = F,  width = ncol(heat.poblacion.ileon)*unit(7, "mm"), 
                             height = nrow(heat.poblacion.ileon)*unit(6, "mm"),column_names_rot = 90,
                             rect_gp = gpar(col = "white", lwd = 2),
                             left_annotation = c(annE, annP),
                             right_annotation=c(barpl),
                             name = "Median \n clr value", 
                             heatmap_legend_param = list(direction = "vertical" , 
                                                         labels_gp = gpar(fontsize = 7),
                                                         title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                         legend_height = unit(1.4, "cm")), 
                             column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                             col = poblacion.ileon.color_heatmap,
                             row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.POBLACION.ILEON


#ALDEX DE RECTO POR POBLACION
#otu de recto
gestacion.recto<- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/table_merged_filtrada_asvs_gestacion0.99/table_merged_filtrada_asvs_gestacion0.99.recto.csv", row.names = 1, check.names = FALSE)

#Eliminar asvs con 0 de la otu filtrada de recto
gestacion.recto.sin0 <- gestacion.recto%>% filter(rowSums(across(where(is.numeric)))!=0)

#asvs a filtrar
asvs.unassigned.recto<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/asvs.filtrar.recto2.txt", check.names = FALSE)

#filtrar de la otu, los 31 ids anteriores
gestacion.recto.filtrada <- gestacion.recto.sin0 %>% 
  rownames_to_column(var = "OTUID") %>% filter(!OTUID %in% asvs.unassigned.recto$OTUID)

gestacion.recto.filtrada <- gestacion.recto.filtrada %>% column_to_rownames(var = "OTUID")

#write.csv(gestacion.recto.filtrada, "/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/gestacion.recto.filtrada.csv", row.names = F)

#Importar metadata
metadata_gestacion <- read.csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/metadata.gestacion.csv", check.names = F)
#subset de recto
metadata.gestacion.recto<- metadata_gestacion %>% filter(seccion=="Recto")

#RECTO
#Comparaciones pareadas
#2600 vs 4150 msnm 
otu.aldex.poblacion.recto <- read_csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/otu.aldex.poblacion.recto.csv") %>% column_to_rownames(var = "OTUID")
conds.poblacion.recto <- c(rep("2600masl", 38), rep("4150masl", 40)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_poblacion.recto <- aldex(reads = otu.aldex.poblacion.recto, conditions = conds.poblacion.recto, mc.samples = 1000,
                              effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_poblacion.recto.filter<-aldex_poblacion.recto %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_poblacion.recto.filter.pcor<-aldex_poblacion.recto%>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.poblacion.recto<- aldex_poblacion.recto.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "2600masl",
    . >  0 ~ "4150masl"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="uncultured"  ~ "Oscillospiraceae",
                                            taxa=="Hafnia-Obesumbacterium"  ~ "Hafnia obesumbacterium",
                                            taxa=="paralvei"  ~ "Hafnia paralvei",
                                            taxa=="bacterium"  ~ "Bacteroides",TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.poblacion.recto<- aldex_plot.poblacion.recto %>% dplyr::select(taxa, "2600 masl"= rab.win.2600masl, "4150 masl"= rab.win.4150masl) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
 

#Tabla para el heatmap. Util cuando hay duplicados 
heat.poblacion.recto <- aldex_plot.poblacion.recto %>% mutate(taxa2= paste0(rownames(heat.poblacion.recto),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
dplyr::select(taxa2, "2600 masl"= rab.win.2600masl, "4150 masl"= rab.win.4150masl) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
column_to_rownames(var = "taxa2")


#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.recto = structure(c("#676778","#D9D9C2"), #COLORES DE BARRAS 
                          names = c("2600masl", "4150masl"))
#Anotacion de las barras
barpl.recto = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.poblacion.recto$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.recto[aldex_plot.poblacion.recto$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#Anotacion de phylum
#capa de valor p
#cols_phylum <- list('p-value' = c(
  #"<0.001" = '#C70039',
  #"<0.01"='#FF5733',
  #"<0.05"="#FFC300",
  #">0.05"="#F9E79F"))


#capa de valor p
cols_pvalue.recto <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))

#Anotacion del valor p 
annP.recto = HeatmapAnnotation("p-value" =aldex_plot.poblacion.recto$p.value, 
                         simple_anno_size = unit(0.45, "cm"),
                         annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                         which = 'row',
                         annotation_legend_param = list(
                           title_gp = gpar(fontsize = 8, fontface="bold"),
                           labels_gp = gpar(fontsize = 8),
                           direction ="vertical"),
                         col = cols_pvalue.recto,
                         show_legend = T, gp = gpar(col = "white"), 
                         show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.recto =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen")) 
  #c("#B7C68B", "#DED29E", "#B3A580"))

#Anotacion del tamaño del efecto
annE.recto = rowAnnotation("Effect size" = aldex_plot.poblacion.recto$effect, 
                     annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                     col = list("Effect size" = effect_col_fun.recto),
                     simple_anno_size = unit(0.45, "cm"),
                     annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                    fontface="bold"),
                                                    labels_gp = gpar(fontsize = 8),
                                                    direction ="vertical"),
                     show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun.poblacion <- colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))
#"#759c8a", "#00544d", "#5f555a"
#"#cbdbcd", "#759c8a", "#00544d", verdes
#"#fbb021", "#1b8a5a","#1d4877", gama verde-azul
#cambio de color del heatmap
poblacion.recto.color_heatmap = colorRamp2(seq(min(heat.poblacion.recto), max(heat.poblacion.recto), length= 3), c("#cbdbcd", "#759c8a", "#00544d")) #"#DAF7A6", "#FFC300", "#FF5733"
HEAT.POBLACION.RECTO<-Heatmap(heat.poblacion.recto, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.poblacion.recto)*unit(7, "mm"), 
                              height = nrow(heat.poblacion.recto)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 1),
                              left_annotation = c(annE.recto, annP.recto),
                              right_annotation=c(barpl.recto),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = poblacion.recto.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.POBLACION.RECTO

#ggsave("heat.poblacion.recto.pdf", width = 8, height = 4, dpi = 600, plot = HEAT.POBLACION.RECTO, device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
aldex.poblacion.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  HEAT.POBLACION.ILEON+theme(legend.title = element_blank(),
                            legend.text = element_blank(),
                            legend.position = "none",
                            axis.text.x = element_text(size = 7),
                            axis.text.y = element_text(size = 7),
                            strip.text.x = element_text(size = 10),
                            axis.title.x = element_text(vjust = 0.3, size = 8),
                            axis.title.y = element_text(vjust = 0.3, size = 8),
                            plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  HEAT.POBLACION.RECTO+theme(legend.title = element_blank(),
                            legend.text = element_text(size = 8),
                            legend.position = "bottom",
                            legend.box.spacing = unit(0.5, "pt"),
                            axis.text.x = element_text(size = 7),
                            axis.text.y = element_text(size = 7),
                            plot.margin =unit(c(0,0,0,0), "cm"), 
                            strip.text.x = element_text(size = 10),
                            axis.title.x = element_text(vjust = 0.3, size = 8), 
                            axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

aldex.poblacion.completo

#ggsave("nmds.poblacion.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.poblacion.completo, device = "pdf")




#ILEON
#Comparaciones pareadas. TEMPORADA
#REPRODUCTIVA vs NO REPRODUCTIVA 
otu.aldex.temporada.ileon<- read_csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/otu.aldex.temporada.ileon.csv") %>% column_to_rownames(var = "OTUID")
conds.temporada.ileon<- c(rep("Reproductive", 40), rep("NoReproductive", 40)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_temporada.ileon<- aldex(reads = otu.aldex.temporada.ileon, conditions = conds.temporada.ileon, mc.samples = 1000,
                              effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_temporada.ileon.filter<-aldex_temporada.ileon %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_temporada.ileon.filter.pcor<-aldex_temporada.ileon %>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.temporada.ileon<- aldex_temporada.ileon.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "Reproductive",
    . >  0 ~ "NoReproductive"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="schindleri"  ~ "Acinetobacter schindleri",
                                            taxa=="oculi"  ~ "Massilia oculi",
                                            taxa=="fastidiosa"  ~ "Dielma fastidiosa",
                                            taxa=="sartorii"  ~ "Bacteroides sartorii",
                                            taxa=="bacterium"  ~ "Odoribacter", TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.temporada.ileon<- aldex_plot.temporada.ileon %>% dplyr::select(taxa, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
 

#Tabla para el heatmap. Util cuando hay duplicados 
heat.temporada.ileon <- aldex_plot.temporada.ileon %>% mutate(taxa2= paste0(rownames(heat.temporada.ileon),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
dplyr::select(taxa2, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
column_to_rownames(var = "taxa2")



#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.temp = structure(c("#808000","#1B5E20"), #COLORES DE BARRAS 
                          names = c("Reproductive", "NoReproductive"))
#Anotacion de las barras
barpl.temp = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.temporada.ileon$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.temp[aldex_plot.temporada.ileon$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#capa de valor p
cols_pvalue.temp <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))
#Anotacion del valor p 
annP.temp = HeatmapAnnotation("p-value" =aldex_plot.temporada.ileon$p.value, 
                         simple_anno_size = unit(0.45, "cm"),
                         annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                         which = 'row',
                         annotation_legend_param = list(
                           title_gp = gpar(fontsize = 8, fontface="bold"),
                           labels_gp = gpar(fontsize = 8),
                           direction ="vertical"),
                         col = cols_pvalue.temp,
                         show_legend = T, gp = gpar(col = "white"), 
                         show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.temp =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen"))
#c("#B7C68B", "#DED29E", "#B3A580"))
#Anotacion del tamaño del efecto
annE.temp = rowAnnotation("Effect size" = aldex_plot.temporada.ileon$effect, 
                     annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                     col = list("Effect size" = effect_col_fun.temp),
                     simple_anno_size = unit(0.45, "cm"),
                     annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                    fontface="bold"),
                                                    labels_gp = gpar(fontsize = 8),
                                                    direction ="vertical"),
                     show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun=colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))

#cambio de color del heatmap
temporada.ileon.color_heatmap = colorRamp2(seq(min(heat.temporada.ileon), max(heat.temporada.ileon), length= 3), c("#cbdbcd", "#759c8a", "#00544d"))
HEAT.TEMPORADA.ILEON<-Heatmap(heat.temporada.ileon, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.temporada.ileon)*unit(7, "mm"), 
                              height = nrow(heat.temporada.ileon)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 2),
                              left_annotation = c(annE.temp, annP.temp),
                              right_annotation=c(barpl.temp),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = temporada.ileon.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.TEMPORADA.ILEON


#ALDEX DE RECTO POR TEMPORADA
#RECTO
#Comparaciones pareadas
#Reproductiva vs No Reproductivq 
otu.aldex.temporada.recto <- read_csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/otu.aldex.temporada.recto.csv") %>% column_to_rownames(var = "OTUID")
conds.temporada.recto <- c(rep("Reproductive", 40), rep("NoReproductive", 38)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_temporada.recto <- aldex(reads = otu.aldex.temporada.recto, conditions = conds.temporada.recto, mc.samples = 1000,
                               effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_temporada.recto.filter<-aldex_temporada.recto %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_temporada.recto.filter.pcor<-aldex_temporada.recto%>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.temporada.recto<- aldex_temporada.recto.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "Reproductive",
    . >  0 ~ "NoReproductive"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="bacterium"  ~ "Ruminococcaceae",
                                            taxa=="uncultured"  ~ "Oscillospiraceae",TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.temporada.recto<- aldex_plot.temporada.recto %>% dplyr::select(taxa, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA


#Tabla para el heatmap. Util cuando hay duplicados 
heat.temporada.recto <- aldex_plot.temporada.recto %>% mutate(taxa2= paste0(rownames(heat.temporada.recto),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
  dplyr::select(taxa2, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
  column_to_rownames(var = "taxa2")


#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.temp.recto = structure(c("#808000","#1B5E20"), #COLORES DE BARRAS 
                                names = c("Reproductive", "NoReproductive"))
#Anotacion de las barras
barpl.temp.recto = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.temporada.recto$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.temp.recto[aldex_plot.temporada.recto$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#Anotacion de phylum
#capa de valor p
#cols_phylum <- list('p-value' = c(
#"<0.001" = '#C70039',
#"<0.01"='#FF5733',
#"<0.05"="#FFC300",
#">0.05"="#F9E79F"))


#capa de valor p
cols_pvalue.temp.recto <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))

#Anotacion del valor p 
annP.temp.recto = HeatmapAnnotation("p-value" =aldex_plot.temporada.recto$p.value, 
                               simple_anno_size = unit(0.45, "cm"),
                               annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                               which = 'row',
                               annotation_legend_param = list(
                                 title_gp = gpar(fontsize = 8, fontface="bold"),
                                 labels_gp = gpar(fontsize = 8),
                                 direction ="vertical"),
                               col = cols_pvalue.temp.recto,
                               show_legend = T, gp = gpar(col = "white"), 
                               show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.temp.recto =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen")) 
#c("#B7C68B", "#DED29E", "#B3A580"))

#Anotacion del tamaño del efecto
annE.temp.recto = rowAnnotation("Effect size" = aldex_plot.temporada.recto$effect, 
                           annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                           col = list("Effect size" = effect_col_fun.temp.recto),
                           simple_anno_size = unit(0.45, "cm"),
                           annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                          fontface="bold"),
                                                          labels_gp = gpar(fontsize = 8),
                                                          direction ="vertical"),
                           show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun.poblacion <- colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))
#"#759c8a", "#00544d", "#5f555a"
#"#cbdbcd", "#759c8a", "#00544d", verdes
#"#fbb021", "#1b8a5a","#1d4877", gama verde-azul
#cambio de color del heatmap
temporada.recto.color_heatmap = colorRamp2(seq(min(heat.temporada.recto), max(heat.temporada.recto), length= 3), c("#cbdbcd", "#759c8a", "#00544d")) #"#DAF7A6", "#FFC300", "#FF5733"
HEAT.TEMPORADA.RECTO<-Heatmap(heat.temporada.recto, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.temporada.recto)*unit(7, "mm"), 
                              height = nrow(heat.temporada.recto)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 1),
                              left_annotation = c(annE.temp.recto, annP.temp.recto),
                              right_annotation=c(barpl.temp.recto),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = temporada.recto.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.TEMPORADA.RECTO

#ggsave("heat.temporada.recto.pdf", width = 8, height = 4, dpi = 600, plot = HEAT.TEMPORADA.RECTO, device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
aldex.poblacion.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  HEAT.POBLACION.ILEON+theme(legend.title = element_blank(),
                             legend.text = element_blank(),
                             legend.position = "none",
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8),
                             axis.title.y = element_text(vjust = 0.3, size = 8),
                             plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  HEAT.POBLACION.RECTO+theme(legend.title = element_blank(),
                             legend.text = element_text(size = 8),
                             legend.position = "bottom",
                             legend.box.spacing = unit(0.5, "pt"),
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             plot.margin =unit(c(0,0,0,0), "cm"), 
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8), 
                             axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

aldex.poblacion.completo

#ggsave("nmds.poblacion.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.poblacion.completo, device = "pdf")


#ILEON
#Comparaciones pareadas. SEXO
#FEMALE vs MALE 
otu.aldex.sexo.ileon<- read_csv("/Users/ninamontoya/Desktop/Analisis.2022/analisisR/analisis.final/gestacion/otus.aldex.new/otu.aldex.sexo.ileon.csv") %>% column_to_rownames(var = "OTUID")
conds.sexo.ileon<- c(rep("Female", 40), rep("Male", 40)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_sexo.ileon<- aldex(reads = otu.aldex.sexo.ileon, conditions = conds.sexo.ileon, mc.samples = 1000,
                              effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_sexo.ileon.filter<-aldex_sexo.ileon %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_sexo.ileon.filter.pcor<-aldex_sexo.ileon %>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.sexo.ileon<- aldex_sexo.ileon.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "Female",
    . >  0 ~ "Male"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="schindleri"  ~ "Acinetobacter schindleri",
                                            taxa=="oculi"  ~ "Massilia oculi",
                                            taxa=="fastidiosa"  ~ "Dielma fastidiosa",
                                            taxa=="sartorii"  ~ "Bacteroides sartorii",
                                            taxa=="bacterium"  ~ "Odoribacter", TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.temporada.ileon<- aldex_plot.temporada.ileon %>% dplyr::select(taxa, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA


#Tabla para el heatmap. Util cuando hay duplicados 
heat.temporada.ileon <- aldex_plot.temporada.ileon %>% mutate(taxa2= paste0(rownames(heat.temporada.ileon),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
  dplyr::select(taxa2, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
  column_to_rownames(var = "taxa2")



#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.temp = structure(c("#808000","#1B5E20"), #COLORES DE BARRAS 
                               names = c("Reproductive", "NoReproductive"))
#Anotacion de las barras
barpl.temp = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.temporada.ileon$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.temp[aldex_plot.temporada.ileon$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#capa de valor p
cols_pvalue.temp <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))
#Anotacion del valor p 
annP.temp = HeatmapAnnotation("p-value" =aldex_plot.temporada.ileon$p.value, 
                              simple_anno_size = unit(0.45, "cm"),
                              annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                              which = 'row',
                              annotation_legend_param = list(
                                title_gp = gpar(fontsize = 8, fontface="bold"),
                                labels_gp = gpar(fontsize = 8),
                                direction ="vertical"),
                              col = cols_pvalue.temp,
                              show_legend = T, gp = gpar(col = "white"), 
                              show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.temp =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen"))
#c("#B7C68B", "#DED29E", "#B3A580"))
#Anotacion del tamaño del efecto
annE.temp = rowAnnotation("Effect size" = aldex_plot.temporada.ileon$effect, 
                          annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                          col = list("Effect size" = effect_col_fun.temp),
                          simple_anno_size = unit(0.45, "cm"),
                          annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                         fontface="bold"),
                                                         labels_gp = gpar(fontsize = 8),
                                                         direction ="vertical"),
                          show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun=colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))

#cambio de color del heatmap
temporada.ileon.color_heatmap = colorRamp2(seq(min(heat.temporada.ileon), max(heat.temporada.ileon), length= 3), c("#cbdbcd", "#759c8a", "#00544d"))
HEAT.TEMPORADA.ILEON<-Heatmap(heat.temporada.ileon, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.temporada.ileon)*unit(7, "mm"), 
                              height = nrow(heat.temporada.ileon)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 2),
                              left_annotation = c(annE.temp, annP.temp),
                              right_annotation=c(barpl.temp),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = temporada.ileon.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.TEMPORADA.ILEON


#ALDEX POR SEXO
#RECTO
#Comparaciones pareadas
#Female vs Male
otu.aldex.sexo.recto <- read_csv("gestacion/otus.aldex.new/otu.aldex.sexo.recto.csv") %>% column_to_rownames(var = "OTUID")
conds.sexo.recto <- c(rep("Female", 40), rep("Male", 38)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_sexo.recto <- aldex(reads = otu.aldex.sexo.recto, conditions = conds.sexo.recto, mc.samples = 1000,
                               effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_sexo.recto.filter<-aldex_sexo.recto %>% filter(abs(effect) >= 0.8)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_sexo.recto.filter.pcor<-aldex_sexo.recto%>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.temporada.recto<- aldex_temporada.recto.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "Reproductive",
    . >  0 ~ "NoReproductive"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="bacterium"  ~ "Ruminococcaceae",
                                            taxa=="uncultured"  ~ "Oscillospiraceae",TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.temporada.recto<- aldex_plot.temporada.recto %>% dplyr::select(taxa, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA


#Tabla para el heatmap. Util cuando hay duplicados 
heat.temporada.recto <- aldex_plot.temporada.recto %>% mutate(taxa2= paste0(rownames(heat.temporada.recto),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
  dplyr::select(taxa2, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
  column_to_rownames(var = "taxa2")


#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.temp.recto = structure(c("#808000","#1B5E20"), #COLORES DE BARRAS 
                                     names = c("Reproductive", "NoReproductive"))
#Anotacion de las barras
barpl.temp.recto = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.temporada.recto$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.temp.recto[aldex_plot.temporada.recto$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#Anotacion de phylum
#capa de valor p
#cols_phylum <- list('p-value' = c(
#"<0.001" = '#C70039',
#"<0.01"='#FF5733',
#"<0.05"="#FFC300",
#">0.05"="#F9E79F"))


#capa de valor p
cols_pvalue.temp.recto <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))

#Anotacion del valor p 
annP.temp.recto = HeatmapAnnotation("p-value" =aldex_plot.temporada.recto$p.value, 
                                    simple_anno_size = unit(0.45, "cm"),
                                    annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                                    which = 'row',
                                    annotation_legend_param = list(
                                      title_gp = gpar(fontsize = 8, fontface="bold"),
                                      labels_gp = gpar(fontsize = 8),
                                      direction ="vertical"),
                                    col = cols_pvalue.temp.recto,
                                    show_legend = T, gp = gpar(col = "white"), 
                                    show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.temp.recto =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen")) 
#c("#B7C68B", "#DED29E", "#B3A580"))

#Anotacion del tamaño del efecto
annE.temp.recto = rowAnnotation("Effect size" = aldex_plot.temporada.recto$effect, 
                                annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                                col = list("Effect size" = effect_col_fun.temp.recto),
                                simple_anno_size = unit(0.45, "cm"),
                                annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                               fontface="bold"),
                                                               labels_gp = gpar(fontsize = 8),
                                                               direction ="vertical"),
                                show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun.poblacion <- colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))
#"#759c8a", "#00544d", "#5f555a"
#"#cbdbcd", "#759c8a", "#00544d", verdes
#"#fbb021", "#1b8a5a","#1d4877", gama verde-azul
#cambio de color del heatmap
temporada.recto.color_heatmap = colorRamp2(seq(min(heat.temporada.recto), max(heat.temporada.recto), length= 3), c("#cbdbcd", "#759c8a", "#00544d")) #"#DAF7A6", "#FFC300", "#FF5733"
HEAT.TEMPORADA.RECTO<-Heatmap(heat.temporada.recto, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.temporada.recto)*unit(7, "mm"), 
                              height = nrow(heat.temporada.recto)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 1),
                              left_annotation = c(annE.temp.recto, annP.temp.recto),
                              right_annotation=c(barpl.temp.recto),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = temporada.recto.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.TEMPORADA.RECTO

#ggsave("heat.temporada.recto.pdf", width = 8, height = 4, dpi = 600, plot = HEAT.TEMPORADA.RECTO, device = "pdf")


#UNIR LAS 3 FIGURAS EN 1 
library(cowplot)
aldex.poblacion.completo<-plot_grid(#h1+theme(plot.margin =unit(c(0,0,0,-0.5), "cm")),
  HEAT.POBLACION.ILEON+theme(legend.title = element_blank(),
                             legend.text = element_blank(),
                             legend.position = "none",
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8),
                             axis.title.y = element_text(vjust = 0.3, size = 8),
                             plot.margin =unit(c(0,0,0.2,0), "cm")),                        
  HEAT.POBLACION.RECTO+theme(legend.title = element_blank(),
                             legend.text = element_text(size = 8),
                             legend.position = "bottom",
                             legend.box.spacing = unit(0.5, "pt"),
                             axis.text.x = element_text(size = 7),
                             axis.text.y = element_text(size = 7),
                             plot.margin =unit(c(0,0,0,0), "cm"), 
                             strip.text.x = element_text(size = 10),
                             axis.title.x = element_text(vjust = 0.3, size = 8), 
                             axis.title.y = element_text(vjust = 0.3, size = 8)),
  ncol = 1, axis="r", align = "v",
  #labels = c("","A)", "B)", "C)"), 
  #rel_heights = c(0,1,0.9,0.7),
  scale = 0.95, hjust = -2.5, vjust = -0.5, label_size = 12)

aldex.poblacion.completo

#ggsave("nmds.poblacion.completo.pdf", width = 3, height = 5, dpi = 600, plot = nmds.poblacion.completo, device = "pdf")



#ILEON
#Comparaciones pareadas. SEXO-TEMPORADA
#HEMBRA REPRODUCTIVA VS HEMBRA NO REPRODUCTIVA
otu.aldex.sextem.ileon.fem<- read_csv("gestacion/otus.aldex.new/otu.aldex.sextem.fem.ileon.csv") %>% column_to_rownames(var = "OTUID")
conds.sextem.ileon.fem<- c(rep("Reproductive.female", 20), rep("NonReproductive.female", 20)) #vector de condiciones que debe venir en el orden como estn las columnas

#Correr ALDEX
aldex_sextem.ileon.fem<- aldex(reads = otu.aldex.sextem.ileon.fem, conditions = conds.sextem.ileon.fem, mc.samples = 1000,
                         effect = TRUE, test = "t", verbose = TRUE, denom = "all", include.sample.summary = FALSE)

#Filtrar por tamaño de efecto
aldex_sextem.ileon.fem.filter<-aldex_sextem.ileon.fem %>% filter(abs(effect) >= 0.5)

#FILTRAR LA MISMA TABLA PERO AHORA POR EL VALOR P CORREGIDO 
#Filtrar con R 
aldex_sextem.ileon.fem.filter.pcor<-aldex_sextem.ileon.fem %>% filter(abs(wi.eBH) <= 0.05)

#plot
#Cargar taxonomia 
taxonomia.nueva<- read.delim("/Users/ninamontoya/Desktop/Analisis.2022/taxonomy.gestacion.0.99/taxonomy.tsv", check.names = F)

#declarar y cambiar condiciones para el plot 
aldex_plot.sexo.ileon<- aldex_sexo.ileon.filter.pcor %>% rownames_to_column(var = "OTUID") %>% 
  inner_join(taxonomia.nueva, by="OTUID") %>% #unir con taxonomia a traves de la columna OTUID
  mutate_at(c("diff.btw"), funs(seccion = case_when( #declarar si es alta o baja de acuerdo al valor negativo o positivo de diff.btw
    . < 0 ~ "Female",
    . >  0 ~ "Male"))) %>% 
  arrange(diff.btw) %>%  #ordenar de mayor a menor 
  mutate(taxa= str_extract(taxonomy, "[^_]+$")) %>% #Agrega una columna llamada taxa con el ultimo nombre del archivo taxonomia 
  mutate_at(c("taxa"), funs(taxa= case_when(taxa=="schindleri"  ~ "Acinetobacter schindleri",
                                            taxa=="oculi"  ~ "Massilia oculi",
                                            taxa=="fastidiosa"  ~ "Dielma fastidiosa",
                                            taxa=="sartorii"  ~ "Bacteroides sartorii",
                                            taxa=="bacterium"  ~ "Odoribacter", TRUE~as.character(taxa)))) %>% #Cambia el nombre de la columna taxa, si dice x por y 
  mutate_at(c("wi.eBH"), funs(p.value = case_when( #declarar cambio de valor p (wi.eBH) por rangos establecidos 
    . <= 0.001 ~ "<0.001",
    . >  0.001 & .  <= 0.01 ~ "<0.01",
    . >  0.01 & .  < 0.05 ~ "<0.05",
    . >=  0.05 ~ ">0.05")))


#Tabla para el heatmap.
heat.temporada.ileon<- aldex_plot.temporada.ileon %>% dplyr::select(taxa, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA


#Tabla para el heatmap. Util cuando hay duplicados 
heat.temporada.ileon <- aldex_plot.temporada.ileon %>% mutate(taxa2= paste0(rownames(heat.temporada.ileon),".", taxa)) %>% #agrega una columna llamada taxa 2 y le agrega numeros al inicio 
  dplyr::select(taxa2, "Reproductive"= rab.win.Reproductive, "No Reproductive"= rab.win.NoReproductive) %>% #Cambia el nombre de las columnas rab.win.ALTA y rab.win.BAJA
  column_to_rownames(var = "taxa2")



#FIGURA CAPA POR CAPA 
#capa de barras
treatment_col.temp = structure(c("#808000","#1B5E20"), #COLORES DE BARRAS 
                               names = c("Reproductive", "NoReproductive"))
#Anotacion de las barras
barpl.temp = rowAnnotation("difference \nbetween groups" = anno_barplot(
  aldex_plot.temporada.ileon$diff.btw, which = "row",
  gp = gpar(fill = treatment_col.temp[aldex_plot.temporada.ileon$seccion]),
  width = unit(4, "cm")),  show_annotation_name = T,
  annotation_name_gp =gpar(fontsize = 0),
  annotation_name_rot = 0)

#capa de valor p
cols_pvalue.temp <- list('p-value' = c(
  "<0.001" = '#C70039',
  "<0.01"='#FF5733',
  "<0.05"="#FFC300",
  ">0.05"="#F9E79F"))
#Anotacion del valor p 
annP.temp = HeatmapAnnotation("p-value" =aldex_plot.temporada.ileon$p.value, 
                              simple_anno_size = unit(0.45, "cm"),
                              annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                              which = 'row',
                              annotation_legend_param = list(
                                title_gp = gpar(fontsize = 8, fontface="bold"),
                                labels_gp = gpar(fontsize = 8),
                                direction ="vertical"),
                              col = cols_pvalue.temp,
                              show_legend = T, gp = gpar(col = "white"), 
                              show_annotation_name = T)


#capa tamaño de efecto
#OTRA OPCION DE COLOR: c("LightSalmon", "white", "IndianRed"))
effect_col_fun.temp =colorRamp2(
  c(-1.5, 0, 1.5), 
  c("lightsalmon4", "white", "lightseagreen"))
#c("#B7C68B", "#DED29E", "#B3A580"))
#Anotacion del tamaño del efecto
annE.temp = rowAnnotation("Effect size" = aldex_plot.temporada.ileon$effect, 
                          annotation_name_gp =gpar(fontsize = 8, fontface="bold"),
                          col = list("Effect size" = effect_col_fun.temp),
                          simple_anno_size = unit(0.45, "cm"),
                          annotation_legend_param = list(title_gp = gpar(fontsize = 8, 
                                                                         fontface="bold"),
                                                         labels_gp = gpar(fontsize = 8),
                                                         direction ="vertical"),
                          show_legend = T, gp = gpar(col = "white"), show_annotation_name = T)

#heatmap
#color_fun=colorRamp2(c(-10, 0, 10), c("blue", "white", "red"))

#cambio de color del heatmap
temporada.ileon.color_heatmap = colorRamp2(seq(min(heat.temporada.ileon), max(heat.temporada.ileon), length= 3), c("#cbdbcd", "#759c8a", "#00544d"))
HEAT.TEMPORADA.ILEON<-Heatmap(heat.temporada.ileon, cluster_rows = F,
                              cluster_columns = F,  width = ncol(heat.temporada.ileon)*unit(7, "mm"), 
                              height = nrow(heat.temporada.ileon)*unit(6, "mm"),column_names_rot = 90,
                              rect_gp = gpar(col = "white", lwd = 2),
                              left_annotation = c(annE.temp, annP.temp),
                              right_annotation=c(barpl.temp),
                              name = "Median \n clr value", 
                              heatmap_legend_param = list(direction = "vertical" , 
                                                          labels_gp = gpar(fontsize = 7),
                                                          title_gp = gpar(fontsize = 7,  fontface="bold"),
                                                          legend_height = unit(1.4, "cm")), 
                              column_names_gp = gpar(fontsize = 6, fontface="bold", direction="vertical"),
                              col = temporada.ileon.color_heatmap,
                              row_names_gp = gpar(fontsize = 7), show_heatmap_legend = T)

HEAT.TEMPORADA.ILEON








