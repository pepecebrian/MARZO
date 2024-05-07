

 #######muesting
muesting <- function(x) {
     library(stringi)
    muestreos_tallas$PUERTO <- toupper(stri_trans_general(
      muestreos_tallas$PUERTO,    "Latin-ASCII"))

    muestreos_tallas$FECHA_MUE <- as.character(muestreos_tallas$FECHA_MUE)
    muestreos_tallas$FECHA_MUE <- str_replace_all(muestreos_tallas$FECHA_MUE,
                                                       "ENE", "JAN")
         muestreos_tallas$FECHA_MUE <- str_replace_all(muestreos_tallas$FECHA_MUE,
                                                       "ABR", "APR")
         muestreos_tallas$FECHA_MUE <- str_replace_all(muestreos_tallas$FECHA_MUE,
                                                       "AGO", "AUG")
         muestreos_tallas$FECHA_MUE <- str_replace_all(muestreos_tallas$FECHA_MUE,
                                                       "DIC", "DEC")
         muestreos_tallas$FECHA <- dmy(muestreos_tallas$FECHA_MUE)
         muestreos_tallas$QUARTER <- quarter(muestreos_tallas$FECHA)
    tallas <- muestreos_tallas[, c("CALADERO_DCF", "COD_ID",
                                   "FECHA", "QUARTER", "ESTRATO_RIM", "PUERTO", "COD_TIPO_MUE",
                                   "BARCO", "ESP_MUE", "CATEGORIA", "ESP_CAT", "P_MUE_VIVO",
                                   "P_VIVO", "TALLA", "EJEM_MEDIDOS", "SOP")] %>% as.data.frame() %>%
      distinct()
   # fwrite(tallas, "tallas.txt")


   # substring2(tallas$PUERTO, "CILLERO") <- "CELEIRO"

    pesos <- tallas %>% group_by(COD_TIPO_MUE, COD_ID, ESTRATO_RIM,
                                 PUERTO, FECHA, QUARTER, BARCO, TAXON = ESP_MUE, CATEGORIA,
                                 ESPECIE = ESP_CAT, P_VIVO) %>%
    dplyr::summarise(EJEM_MEDIDOS_CAT = sum(EJEM_MEDIDOS), MUEST_SP_CAT = sum(SOP)) %>%
              group_by(COD_ID, TAXON, ESPECIE) %>%
    dplyr::mutate(MUEST_SP = sum(MUEST_SP_CAT)) %>%
              group_by(COD_ID, TAXON, CATEGORIA) %>%
    dplyr::mutate(MUEST_CAT = sum(MUEST_SP_CAT)) %>%
              group_by(COD_ID, ESPECIE) %>%
    dplyr::mutate(PESO_SP_CAT = round((P_VIVO * MUEST_SP_CAT)/MUEST_CAT, 2)) %>%
              group_by(COD_TIPO_MUE, COD_ID, ESTRATO_RIM, PUERTO, FECHA, BARCO, TAXON, ESPECIE) %>%
    dplyr::mutate(EJEM_MEDIDOS_SP = sum(EJEM_MEDIDOS_CAT), PESO_SP = sum(PESO_SP_CAT),
             PESO_SIRENO = sum(P_VIVO))

    pesos <- pesos[complete.cases(pesos[c("PESO_SP")]), ]

    pesos1 <- pesos[, c("COD_TIPO_MUE", "COD_ID", "FECHA", "QUARTER",
                        "ESTRATO_RIM", "PUERTO", "BARCO", "TAXON", "CATEGORIA",
                        "ESPECIE", "MUEST_SP_CAT", "PESO_SP_CAT", "MUEST_SP",
                        "PESO_SP", "PESO_SIRENO")]
    pesos1 <- distinct(pesos1)
    tallas1 <- distinct(tallas[, c("CALADERO_DCF", "COD_ID",
                                   "FECHA", "QUARTER", "ESTRATO_RIM", "PUERTO", "COD_TIPO_MUE",
                                   "ESP_MUE", "CATEGORIA", "ESP_CAT", "TALLA", "EJEM_MEDIDOS")])
    colnames(tallas1)[colnames(tallas1) %in% c("ESP_MUE", "ESP_CAT")] <- c("TAXON","ESPECIE")

    tallas1 <- tallas1[complete.cases(tallas1[c("EJEM_MEDIDOS")]), ]

    tallas2 <- dplyr::full_join(pesos1, tallas1) %>% distinct() %>%
            group_by(COD_ID, TALLA, ESPECIE) %>%
    dplyr:: mutate(EJEM_POND_CAT = round((PESO_SP_CAT * EJEM_MEDIDOS/MUEST_SP_CAT), 2)) %>%
            group_by(COD_ID, TALLA, ESPECIE) %>%
    dplyr::mutate(EJEM_MED_TALLA = sum(EJEM_MEDIDOS),
                  EJEM_POND_TALLA = sum(EJEM_POND_CAT),
                  PESO_MUEST_TALLA = sum(MUEST_SP_CAT),
                  PESO_DESEM_TALLA = sum(PESO_SP_CAT),
                  EJEM_POND_METODOB = round((PESO_DESEM_TALLA *EJEM_MED_TALLA/PESO_MUEST_TALLA), 2)) %>%
            group_by(COD_ID, ESPECIE) %>%
    dplyr::mutate(EJEM_MED_MAREA = sum(EJEM_MEDIDOS),
                  TALLA_MEDIA_MAREA = round(weighted.mean(TALLA, EJEM_POND_TALLA),                                                                                                                                                                                                                                                                                                                       2)) %>% group_by(COD_ID, ESPECIE, CATEGORIA) %>%
    dplyr::mutate(TALLA_MEDIA_CAT   = round(weighted.mean(TALLA, EJEM_POND_CAT),
                                     2))
    tallas2 <- tallas2[complete.cases(tallas2[c("PESO_SP")]),]

    TALLAS <- tallas2[, c("CALADERO_DCF", "COD_TIPO_MUE", "COD_ID",
                          "FECHA", "QUARTER", "ESTRATO_RIM", "PUERTO", "BARCO",
                          "TAXON", "ESPECIE", "TALLA_MEDIA_MAREA", "EJEM_MED_MAREA",
                          "PESO_SP")] %>% distinct() %>% as.data.table()


return(TALLAS)
  }


#######HEADER########3
header<-function(x, y) {
        as.data.frame(head(x,10))
}



   ################MODELANDO############
modelando <- function(x, y) {




  mod <- lm(TALLA_MEDIA_MAREA~ESTRATO_RIM*ESPECIE, data=TALLAS)

  cooksd <- cooks.distance(mod)



  cooksd2<-as.data.frame(cooksd)   #anadimos numero de observacion para cruzarlas


  cooksd2$ObsNumber <- 1:length(cooksd)
  TALLAS$ObsNumber <- 1:length(cooksd)


  sp2<-dplyr::full_join(TALLAS, cooksd2)%>%distinct()%>%arrange(ESTRATO_RIM, PUERTO, COD_ID)%>%
    arrange(ObsNumber)%>%as.data.frame()

  sp2<-sp2[complete.cases(sp2[c("cooksd")]),]

  dMean <- sp2 %>%
    group_by(ESPECIE, ESTRATO_RIM) %>%
    dplyr::summarise(MN = mean(cooksd))%>%arrange(-MN)%>%as.data.frame()

  dMean<-dMean[complete.cases(dMean[c("MN")]),]

  sp3<-left_join(sp2, dMean)%>%unique()%>%arrange(FECHA)%>%as.data.frame()


  sp3<-sp3[complete.cases(sp3[c("MN")]),]

  sp3<-sp3%>%group_by(ESTRATO_RIM,ESPECIE)%>%
    mutate(
      mareas=length(unique(COD_ID)),
      MAX=1.2*max(cooksd),
      t_max= max(TALLA_MEDIA_MAREA),
      t_min= min(TALLA_MEDIA_MAREA))%>%as.data.frame()


  OUTLIERS<-subset(sp3, cooksd>8*MN & EJEM_MED_MAREA>3)%>%
    dplyr::select(-c( MAX, MN)); as.data.frame(OUTLIERS)


 fwrite(OUTLIERS,"OUTLIERS.txt")
  library(openxlsx)
 # write.xlsx(OUTLIERS, 'OUTLIERS.xlsx')

  rm(sp2)

  library(ggbeeswarm)
  sp3$PUERTO2<-ifelse(sp3$COD_TIPO_MUE %in% c(4,6), "A BORDO", sp3$PUERTO)
  table(sp3$CALADERO_DCF)
  table(sp3$ESTRATO_RIM)
  sp3<-sp3%>%group_by(ESTRATO_RIM, ESPECIE)%>%mutate(
    FILTRO=ifelse(any(cooksd>8*MN),
                  "OUT", "NO OUT"  ))
sp3<-sp3%>%#  dplyr::select(1:6,10,14,26,25,29:36)%>%unique() %>%
    as.data.table()

  return(sp3)
  print(sp3)

}

###########cocinando

cocinando <- function(AC, cooksd, PESO_SP, PUERTO2, ESPECIE, ESTRATO_RIM, sp3, MN, FECHA) {


  AC$PUERTO <- toupper(stri_trans_general(
      AC$PUERTO,    "Latin-ASCII"))

     colp <- c("A CORUNA" = "steelblue", "SANTA EUGENIA DE RIBEIRA" = "blue", "CILLERO"="darkgreen",
            "VIGO" = "orange", "AVILES-GIJON" = "darkblue","AVILES"="red", "GIJON"="#00BFC4",
            "SANTONA" = "#7CAE00", "CEDEIRA"="forestgreen", "FINISTERRE"= "darkgoldenrod2",
            "LUARCA" = "chartreuse4", "MUROS"= "#619CFF", "CELEIRO"="darkgreen",
            "BURELA" ="yellowgreen","SUANCES"="deeppink3",
            "MARIN"= "mediumorchid", "SAN VICENTE DE LA BARQUERA"= "tomato",
            "ISLA CRISTINA" ="steelblue", "LLANES"= "darksalmon",
            "PUNTA UMBRIA" = "slateblue3", "BARBATE"= "red3","SANTANDER"= "red",
            "PUERTO DE SANTA MARIA"="darkorchid2","ROTA"="orange","A BORDO" = "black",
            "CADIZ"="Chartreuse2", "TARIFA"= "coral1", "AYAMONTE"= "coral3",
            "SANLUCAR DE BARRAMEDA"= "darksalmon","PUNTA DEL MORAL"= "red",
            "CASTLETOWN BERE" = "deeppink3", "PUERTO DE LA VEGA"="black", "MUXIA"="tomato2")


         temp_plot  <-ggplot(data =AC,
         mapping = aes(y = cooksd, x=TALLA_MEDIA_MAREA, col=factor(PUERTO2)))  +
     #geom_point(aes(color = factor(PUERTO2)), position = "jitter",size=3) +



    geom_quasirandom(aes(colour = PUERTO2, size=PESO_SP,x=TALLA_MEDIA_MAREA, y=cooksd  ),
                    method = "smiley")  +

    geom_hline(data = AC, aes(yintercept = 4*AC$MN),size=1.5, colour="red")  +
    guides(colour = guide_legend(override.aes = list(size = 3)))     +
    guides(scale= "none",size=FALSE,fill=guide_legend(override.aes=list(size=3))) +
    scale_size(range=c(4,10))  +
    facet_wrap(ESPECIE~ESTRATO_RIM, scales="free")   +

    labs(title=AC$ESPECIE,subtitle="Influential Obs by Cooks distance (cooksd>4*mean)",
         caption = "AÑO= 2023") +

    theme_minimal()    +  #theme(legend.position = "none") +
    theme(strip.text.x = element_text(size=12, angle=0,face="bold", colour="white"),
          strip.text.y = element_text(size=12, face="bold",colour="white"),
          strip.background = element_rect(colour="white", fill=c( "steelblue")))  +



    theme(plot.title = element_text(hjust=0.5,lineheight=7, face="bold", size=16),
          plot.subtitle = element_text(hjust=0.5,lineheight=10, face="bold.italic",
                                       size = 14)) +
    scale_colour_manual(values=colp,limits = force)   +
    theme(legend.text = element_text(colour="steelblue", size = 10, face = "bold"))+
    theme( legend.title = element_text(colour="blue", size=10, face="bold"))+
    theme(axis.text=element_text(angle=0, size=12, face="bold")) +

    geom_blank(aes(x = 0.99*t_min)) +
    geom_blank(aes(x = 1.05*t_max)) +
    geom_blank(aes(y = 0.99*MAX)) +



geom_label_repel(show.legend=FALSE,data=subset(AC,  EJEM_MED_MAREA>5 ),
aes(fontface="bold",  TALLA_MEDIA_MAREA,cooksd,
label = ifelse(cooksd>4*MN,paste(round(TALLA_MEDIA_MAREA,2), "cm", "\n",
FECHA, " ", "\n",EJEM_MED_MAREA, "Ejemplares"),"") , vjust=0, hjust=0.5))
    # scale_x_sqrt()

ggsave(temp_plot, file=paste0("2023_plot_TALLAS_MEDIAS ",unique(AC$ESPECIE)," ", ".png"),
       width = 35, height =25, units = "cm")

temp_plot

}
