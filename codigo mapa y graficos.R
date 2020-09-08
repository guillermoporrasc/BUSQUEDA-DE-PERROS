#Libreria Para Mapas

library(ggplot2)
library(plyr)
library(readxl)
library(data.table)
library(tidyverse)

#cargando base de datos
setwd("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS")

PRUEBA_PERROS<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/DATOS BUSQUEDA PERROS.xlsx",                            sheet = "DATOS_GENERAL")
na.omit(PRUEBA_PERROS)

poligonos<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/ASA.xlsx")

#escoger variables 
PRUEBA_PERROS<- select(PRUEBA_PERROS,ZONA,MES_FOTO,LONG,LAT,BUSQUEDA)

#escoger areas para graficar

PERROS_MAP<-filter(PRUEBA_PERROS, ZONA=="ALTO SELVA ALEGRE ZONA A" |
                     ZONA=="ALTO SELVA ALEGRE ZONA B" | ZONA=="ALTO SELVA ALEGRE ZONA C"|
                     ZONA=="URB. GRAFICOS"| ZONA=="ANDRES AVELINO CACERES"|
                     ZONA=="COOP. LA ESTRELLA"|ZONA=="ANTONIO JOSE DE SUCRE"|
                     ZONA=="SAN JOSE"|ZONA=="VILLA EL SOL P. POLANCO"|
                     ZONA=="UPIS RAMIRO PRIALE"|
                     ZONA=="APROMA PAMPAS POLANCO"|ZONA=="ASOC. VIV. J. V. ALVARADO"|
                     ZONA=="A.A.H.H. JAVIER HERAUD PAMPAS POLANCO" |
                     ZONA=="AUGUSTO SALAZAR BONDY"|ZONA=="AVIS RAFAEL HOYO RUBIO"|
                     ZONA=="COMPLEJO ARTESANAL"|ZONA=="URB. APURIMAC"|
                     ZONA=="EL MIRADOR P. POLANCO")

#convertir a numeros Y faCtores las variables

PERROS_MAP$LONG<-as.numeric(PERROS_MAP$LONG)
PERROS_MAP$LAT<-as.numeric(PERROS_MAP$LAT)
PERROS_MAP$BUSQUEDA<-factor(PERROS_MAP$BUSQUEDA,levels=c("A","B","C","GS"))

#Subset para genenar puntos sobre otros

BUSQUEDA.A<-subset(PERROS_MAP,BUSQUEDA=="A")
BUSQUEDA.B<-subset(PERROS_MAP,BUSQUEDA=="B")
BUSQUEDA.C<- subset(PERROS_MAP,BUSQUEDA=="C")
BUSQUEDA.GS<- subset(PERROS_MAP,BUSQUEDA=="GS")

#selecionar las zonas a graficar como poligonos en el mapa

ASA_ZONA_A<-filter(poligonos,ident=="1.-Alto Selva Alegre Zona A" |ident=="2.-Alto Selva Alegre Zona B"
                   | ident=="3.-Alto Selva Alegre Zona C"|ident=="4.-URB. Graficos"|
                     ident=="5.-Andres Avelino Caceres"|ident=="6.-Coop. La Estrella"|ident=="7.-Antonio Jose de Sucre"|
                     ident=="8.-San Jose"|ident=="9.-Villa el Sol P. Polanco"|ident=="10.-UPIS Ramiro Priale"|
                     ident=="11.-Aproma Pampas Polanco"|ident=="12.-Asoc.Viv. J. V. Alvarado"|ident=="13.-El Mirador P. Polanco"|
                     ident=="14.-A.A.H.H. Javier Heraud Pampas polanco"|
                     ident=="17.-Agusto Salazar Bondy"| ident=="28.-Avis Rafael Hoyo Rubio"|ident=="31.-Complejo Artesanal"|
                     ident=="34.-Urb Apurimac")

#Grafica de los perros en el mapa

ggplot(data = PERROS_MAP,aes(x=LONG, y=LAT,colour=BUSQUEDA))+ theme_void() +
  geom_polygon(aes(x=long, y=lat, group=ident), size=.3,fill="white",colour="black",
               data = ASA_ZONA_A)+
  geom_point(data=BUSQUEDA.GS, 
             aes(x=LONG, y=LAT), size =3.3)+
  geom_point(data=BUSQUEDA.C, 
             aes(x=LONG, y=LAT), size =2.1)+
  geom_point(data=BUSQUEDA.B, 
             aes(x=LONG, y=LAT), size =1.5)+
  geom_point(data=BUSQUEDA.A, 
             aes(x=LONG, y=LAT), size = 1)+
  
  scale_color_manual(values =c("GS"="violet","B"="green","C"="blue","A"="red"), name="",
                     labels = c("BUSQUEDA.A", "BUSQUEDA.B", "BUSQUEDA.C","BUSQUEDA.GS"))+
  labs(title = "PERROS ENCONTRADOS")
  
#PLOT DE PERROS POR KILOMETROS RECORRIDOS
  
PERROS_KILOMETROS<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/DATOS BUSQUEDA PERROS.xlsx", 
                               sheet = "PERROS_KM")
PERROS_KILOMETROS<-na.omit(PERROS_KILOMETROS)

ggplot(data = PERROS_KILOMETROS, aes(x=KILOMETROS, y=PERROS,colour=BUSQUEDA))+
  geom_point(size=3.2,alpha=0.8)+ facet_wrap(~BUSQUEDA)


### MAPA PARE PERROS DUPLICADOS

#perros duplicados en  busqueda

duplicados.GS<-select(BUSQUEDA.GS,LAT, LONG, BUSQUEDA)
unicos.GS<-unique(duplicados.GS)
duplicados<-duplicados.GS[duplicated(duplicados.GS),]

#a?adir columnas para identificar si son unicos o duplicados

unicos.GS<-cbind(unicos.GS,PERRO=rep("unico",530))
duplicados<-cbind(duplicados,PERRO=rep("duplicado",64))

# unir datas frames

PERROS_DUPLICADOS<-rbind.data.frame(unicos.GS,duplicados)

#convertir columna perro a factor
PERROS_DUPLICADOS$PERRO<-factor(PERROS_DUPLICADOS$PERRO,levels=c("unico","duplicado"))

#grafica de perros duplicados entre zonas

ggplot(data=PERROS_DUPLICADOS,aes(x=LONG, y=LAT,color=PERRO))+ theme_void() +
  geom_polygon(aes(x=long, y=lat, group=ident), size=.3,fill="white",colour="black",
               data = ASA_ZONA_A)+ geom_point(size=2)+labs(title = "PERROS ENCONTRADOS")
  
  ## RELACION DE PERROS ENTRE ENCUESTAS Y BUSQUEDA POR STREET VIEW ##

#cargando datos 
ENCUESTA_PERROS<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/ENCUESTA PERROS.xlsx") 
PRUEBA_PERROS<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/DATOS BUSQUEDA PERROS.xlsx", 
                           sheet = "DATOS_GENERAL")

#filtrar perros deambulantes encuestas
ENCUESTA_PERROS<-filter(ENCUESTA_PERROS,ACCESO_CALLE=="SI")

#filtrar perros GSV
PERROS_GSV<-filter(PRUEBA_PERROS,BUSQUEDA=="GS")
PERROS_GSV<-filter(PERROS_GSV, ZONA=="CONQUISTADOR II"|
                     ZONA=="A.A.H.H. JAVIER HERAUD PAMPAS POLANCO" |
                     ZONA=="VILLA FLORIDA"|ZONA=="VILLA SALVADOR"|
                     ZONA=="INDEPENDENCIA ZONA A I"|ZONA=="URB. APURIMAC"|
                     ZONA=="EL MIRADOR P. POLANCO"|ZONA=="AA.HH. PRIMERO DE ENERO"|
                     ZONA=="ANTONIO JOSE DE SUCRE")

#crenado data.frame de perros encuesta zona /perro
PERROS_ENCUESTAS<-select(ENCUESTA_PERROS,ZONA)

PERROS_ENCUESTAS$ZONA<-as.factor( PERROS_ENCUESTAS$ZONA)
PERROS_ENCUESTAS<-as.data.frame(table(PERROS_ENCUESTAS))

colnames(PERROS_ENCUESTAS)[1] <- "ZONA"
colnames(PERROS_ENCUESTAS)[2] <- "PERROS"
PERROS_ENCUESTAS$ZONA<-as.factor(PERROS_ENCUESTAS$ZONA)

#   CRENADO DATA FRAME PERROS GSV
PERROS_GSV<-select(PERROS_GSV,ZONA)
PERROS_GSV<-as.data.frame(table(PERROS_GSV))

colnames(PERROS_GSV)[1] <- "ZONA"
colnames(PERROS_GSV)[2] <- "PERROS"
PERROS_GSV$ZONA<-as.factor(PERROS_GSV$ZONA)

#AGREGANDO COLUMNAS DE METODO
PERROS_GSV<-cbind(PERROS_GSV,METODO=rep("GSV",9))
PERROS_ENCUESTAS<-cbind(PERROS_ENCUESTAS,METODO=rep("ENCUESTA",9))

#UNIENDO DATA FRAMES
PERROS_DEAMBULANTES<-rbind(PERROS_GSV,PERROS_ENCUESTAS)

#KM RECORRIDOS DE STREET VIEW

PERROS_KM<- read_excel("F:/BUSQUEDA PERROS/BUSQUEDA-DE-PERROS/DATOS BUSQUEDA PERROS.xlsx", 
                       sheet = "PERROS_KM")
PERROS_KM<-filter(PERROS_KM,BUSQUEDA=="GS")
PERROS_KM<-filter(PERROS_KM, ZONA=="CONQUISTADOR II"|
                    ZONA=="A.A.H.H. JAVIER HERAUD PAMPAS POLANCO" |
                    ZONA=="VILLA FLORIDA"|ZONA=="VILLA SALVADOR"|
                    ZONA=="INDEPENDENCIA ZONA A I"|ZONA=="URB. APURIMAC"|
                    ZONA=="EL MIRADOR P. POLANCO"|ZONA=="AA.HH. PRIMERO DE ENERO"|
                    ZONA=="ANTONIO JOSE DE SUCRE")
PERROS_KM<-select(PERROS_KM,ZONA,KILOMETROS,AREA)

#agregando los KM a las zonas
PERROS_DEAMBULANTES<-merge(PERROS_DEAMBULANTES,PERROS_KM,by="ZONA")
PERROS_DEAMBULANTES

#GRAFICO DE BARRAS

ggplot(data=PERROS_DEAMBULANTES,aes(ZONA,PERROS, fill=METODO)) +
  geom_bar(stat="identity",position="dodge") + labs(x="LOCALIDADES",y="PERROS", 
  title="PERROS ENCOTRADOS CON GSV Y ENCUESTAS") +
  coord_flip()+ geom_text(aes(label=(PERROS)))

#PLOT DE PERROS ENCONTRADOS

ggplot(data=PERROS_DEAMBULANTES,aes(x=ZONA, y=KILOMETROS,color=METODO,size=PERROS))+
  geom_point(alpha=0.5)+coord_flip()+labs(title = "PERROS")

#PLOT DE PERROS EN RELACION AL AREA DE LA ZONA
ggplot(data=PERROS_DEAMBULANTES,aes(x=AREA, y=PERROS,color=METODO,size=KILOMETROS))+
  geom_point(alpha=0.5)+coord_flip()+labs(title = "PERROS")
