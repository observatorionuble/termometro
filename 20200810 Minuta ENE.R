# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
# ++ REPORTES SECTORIALES ++ ----------------------------------------------------
# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
#
#
# Héctor Garrido Henríquez
# Analista Cuantitativo. Observatorio Laboral Ñuble
# Docente. Facultad de Ciencias Empresariales
# Universidad del Bío-Bío
# Avenida Andrés Bello 720, Casilla 447, Chillán
# Teléfono: +56-942353973
# http://www.observatoriolaboralnuble.cl

rm(list = ls())


## Este sí que sí 

#
Sys.setenv(LANG = "en")


load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}




packages = c("tidyverse", "stringi", "lubridate", 
             "data.table", "srvyr", "pbapply", 
             "ggrepel", "RColorBrewer", "readstata13", 
             "gtable", "gridExtra", "tidytext", 
             "wordcloud", "kableExtra", "captioner", 
             "foreign", "RPostgres", "haven", 
             "rJava", "openxlsx", "timetk", 
             "forecast","sweep", "tidyquant", 
             "ggmap", "rgeos", "ggalt", "maptools", 
             "rgdal", "readxl", "grid", "scales", 
             "fuzzyjoin", "survey", "directlabels", "microbenchmark")

load_pkg(packages)


path_ene = "C:/Users/omen03/ENE/"


#  ***********************************************************************************
#  %% Definición de funciones auxiliares para el análisis ============================
#  ***********************************************************************************


nene = list.files(path_ene, pattern = ".csv$")

# file.rename(list.files(pattern=".csv$"),
#              gsub("-","_", tolower(list.files(pattern=".csv$")), fixed = TRUE))



nenes = gsub(".csv","", nene)
x = 1:length(nenes)
nenes = nenes[x]
n = length(nenes)

drv <- dbDriver("PostgreSQL")

# CREATE DATABASE "ENE"
# WITH 
# OWNER = postgres
# ENCODING = 'UTF8'
# LC_COLLATE = 'Spanish_Chile.1252'
# LC_CTYPE = 'Spanish_Chile.1252'
# TABLESPACE = pg_default
# CONNECTION LIMIT = -1;

# Posibles problemas para conectarse con el servidor local se soluciona con esto: 
# https://stackoverflow.com/a/56576090/4950935


con =  dbConnect(RPostgres::Postgres(),
                 host = "localhost",
                 port = 5433,
                 dbname = "ENE",
                 user = "postgres",
                 password = "hecgarri"
)


## Funciones auxiliares ======== 
{

## Para ejecutar algunas funciones del lenguaje PostgreSQL es necesario crear algunas funciones auxiliares: 
# con: conexión a la base de datos 
# file.rename(list.files(pattern=".csv$"),gsub("-","_", list.files(pattern = ".csv$")))

# tn: abreviatura de "Table Name"

createEmptyTable = function(con,tn,df) {
  sql <- paste0("CREATE TABLE \"",tn,"\" (",paste0(collapse=',','"',names(df),'" ',
                                                   sapply(df[0,],postgresqlDataType)),");");
  dbSendQuery(con,sql);
  invisible();
};

insert_data = function(tn, source){
  sql = paste0('COPY ',tn,' FROM \'',paste0(getwd(),'\\',source),'\' DELIMITER \',\' CSV HEADER encoding \'latin1\'')
  dbSendQuery(con, sql);
}

format_quarters <- function(x) {
  year <- year(x)
  quart <- month(x)
  paste(c("Dic-Feb","Ene-Mar","Feb-Abr",
          "Mar-May","Abr-Jun","May-Jul",
          "Jun-Ago","Jul-Sep","Ago-Oct",
          "Sep-Nov","Oct-Dic", "Nov-Ene")[quart], 
        year)
}

}
### Creación de la base de datos ===== 

tn = nenes


# nenes__ = lapply(129:131, function(x) fread(nene[x]))
# 
# lapply(1:3, function(x) fwrite(nenes__[[x]], paste0(nenes[128+x], ".csv"), sep = ","))

if (dbExistsTable(con, nenes[(n-11)]) == FALSE){
  #lapply((n-12):n, function(x) dbRemoveTable(con,tn[x]));
  
  pblapply((n-12):n, function(x) createEmptyTable(con,tn[x],fread(nene[x], nrow = 100) %>%
                                               mutate_if(~!is.null(.x), ~ replace_na(.x, replace = "NULL"))));
  
  pblapply((n-12):n, function(x) insert_data(tn[x],nene[x]))
  
}


dbListTables(con)
dbListFields(con, nenes[131])



# Funciones auxiliares ====

{

  select_cols_ = function(colnames, tn, var = NULL, param = NULL){
    if (is.null(var) | is.null(param)){
      sql = paste0("SELECT ", paste0(colnames, collapse = ", "), " FROM ", tn,";")
    } else {
      sql = paste0("SELECT ", paste0(colnames, collapse = ", "), " FROM ", tn," WHERE ",var," = \'",param,"\'",  ";")
    }
    dbGetQuery(con, sql)
  }
  
  # Para declarar una columna nueva en la tabla
  # ALTER TABLE ",tn,"
  # ADD region_e VARCHAR(10);
  
  create_aux = function(tn, region_e, r_p_c ){
    sql = paste0("ALTER TABLE ",tn," ADD region_e VARCHAR(10);
               UPDATE ", tn, " SET ", region_e," =
               CASE
               WHEN length(",r_p_c,") =  4 THEN substring(",r_p_c," from 1 for 1)
               ELSE substring(",r_p_c," from 1 for 2)
               END;")
    dbSendStatement(con, sql)
  }
  
    
}



  # if (!("region_e" %in% dbListFields(con, nenes[n-1])) == TRUE) {
  #   pblapply(1:n, function(x) create_aux(tn[x], "region_e", "b18_codigo"))  
  # }
  
  
  # ene_nuble_ es un conjunto de datos que no considera el efecto de la conmutación en el empleo, pues usa
  # la variable region que identifica el lugar de residencia del trabajador. Por tanto la mirada es más bien teritorial.
  
  
  
colnames = c("ano_trimestre","mes_central", "ano_encuesta",
             "mes_encuesta", "id_directorio", "estrato", 
             "fact_cal", "cae_general", "cae_especifico", "b18_codigo", "region",
             "r_p_c", "sexo", "nacionalidad", "b11",
             "b15_1","b15_2","categoria_ocupacion", "nivel",
             "habituales", "efectivas", "edad", "curso",
             "termino_nivel","e19", "region_e", 
             "b17_mes", "b17_ano", # variables para calcular duración en el puesto de trabajo
             "e5_dia", "e5_mes", "e5_ano", # Variables para calcular duración del desempleo
             "e9", # Razones de inactividad 
             "tramo_edad") # Razones de despido
  
  #fix_wd("Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/")
  
nuevas_variables = function(ene){
  ene %>% 
    mutate_all(as.numeric) %>% 
    mutate(aux = stri_length(r_p_c), 
           cae_general2 = recode(cae_general,`0`=0,`1` = 1,`2` = 1,
                                 `3` = 1, `4` = 2,`5` = 2,`6` = 3,
                                 `7` = 3,`8` = 3,`9` = 3), 
           cae_general2 = factor(cae_general2,levels = c(0,1,2,3), 
                                 labels = c("Menor de 15","Ocupado", "Desocupado", "Inactivo")),
           activo = ifelse(cae_general2 == "Ocupado" | cae_general2=="Desocupado",1,0), 
           edad_activ = ifelse(cae_general2 != "Menor de 15",1,0), 
           desocupado = ifelse(cae_general2=="Desocupado",1,0), 
           ocupado = ifelse(cae_general2 == "Ocupado",1,0), 
           region = ifelse(prov==84,16,region) , 
           tramo_etario = ifelse(edad>=15 & edad<= 29,1,
                                 ifelse(edad>= 30 & edad<= 44,2,
                                        ifelse(edad>= 45 & edad<= 59,3,
                                               ifelse(edad>= 60 & edad<=64,4,
                                                      ifelse(edad>=65, 5, NA))))), 
    )
}
  
  rename_col = function(tn, old_n, new_n){
    sql = paste0("ALTER TABLE ",tn," 
  RENAME COLUMN ",old_n," TO ",new_n,";")
    dbSendStatement(con, sql)  
  }
  
  trimestres = seq.Date(as.Date("2010-02-01"), as.Date("2020-12-01"), "month")
  


  
  
  
  
# Códigos para gráficos =================


  # bar_chart = function(df, categoria, pct, variable, 
  #                      title = NULL){ 
  #   nb.cols <-     length(unique(eval(substitute(variable), df)))
  #   mycolors <- colorRampPalette(brewer.pal(min(9,nb.cols), "Blues"))(nb.cols)
  #   categoria = enquo(categoria)
  #   pct = enquo(pct)
  #   variable = enquo(variable)
  #   p = df %>% 
  #     ggplot(aes(x = !! categoria, y = !! pct, fill = !! variable))+
  #     geom_bar(stat = "identity", position = "fill")+
  #     coord_flip()+
  #     geom_text(aes(label=paste0(!! pct, "%")), position=position_fill(vjust = .5), size = 10) +
  #     scale_x_discrete(labels = function(x) str_wrap(x, width = 15) %>% str_trunc(width = 24), 
  #                      limits = function(x) rev(x))+
  #     scale_fill_manual(values = mycolors,
  #                       labels = function(x) str_wrap(x, width = 40) %>% str_trunc(width = 60))+
  #     labs(y = "% de respuestas", x = "",
  #          caption = "Fuente: Observatorio Laboral de Ñuble",
  #          fill = "", 
  #          subtitle = paste0("n: ", sum(eval(substitute(empresas), df), na.rm = TRUE)," respuestas"))+
  #     guides(fill = guide_legend(nrow = round(nb.cols/2)))+
  #     ggtitle(str_wrap(title, width  = 55))+
  #     theme(axis.text.x = element_text(size = 25, hjust = .5), 
  #           text = element_text(size = 25),
  #           strip.background=element_blank(),
  #           panel.border = element_blank(),
  #           panel.grid = element_blank(),
  #           panel.background = element_blank(), 
  #           axis.ticks=element_blank(),
  #           legend.position = "bottom",
  #           legend.text=element_text(size=25), 
  #           axis.text = element_text(size = 25), 
  #           axis.title = element_text(size = 25), 
  #           legend.key.size = unit(.75,"cm"), 
  #           legend.title = element_text(size = 15), 
  #           title = element_text(size = 30))+
  #     theme(axis.line = element_blank(),
  #           # axis.text = element_blank(),
  #           axis.ticks = element_blank(),
  #           axis.text.x = element_blank(), 
  #           legend.position = "top")
  #   p
  #
  
  
  
  theme_  = theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1), 
                  text = element_text(size = 18),
                  strip.background=element_blank(),
                  # panel.border = element_blank(),
                  # panel.grid = element_blank(),
                  axis.ticks=element_blank(),
                  legend.position = "bottom",
                  legend.text=element_text(size=15), 
                  axis.text = element_text(size = 20), 
                  axis.title = element_text(size = 25), 
                  legend.key.size = unit(.75,"cm"), 
                  legend.title = element_text(size = 20))
  

  
# Análisis  de la Encuesta Nacional de Empleo (ENE) =============================================================================
#********************************************************************************************************************************

  
  

# pblapply(119:119, function(x) rename_col(tn[x], "estrato_unico", "estrato"))
# pblapply(119:131, function(x) rename_col(tn[x], "conglomerado", "id_directorio"))
# pblapply(124, function(x) rename_col(tn[x], "e9_orig", "e9"))
  
i_ = 131

dbListFields(con, nenes[i_])[grep("conglomerado", dbListFields(con, nenes[i_]))]
  

new_vars = function(ene){
  ene %>% 
    mutate(cae_general2 = recode(cae_general,`0`=0,`1` = 1,`2` = 1,
                                 `3` = 1, `4` = 2,`5` = 2,`6` = 3,
                                 `7` = 3,`8` = 3,`9` = 3), 
           cae_general2 = factor(cae_general2,levels = c(0,1,2,3), 
                                 labels = c("Menor de 15","Ocupado", "Desocupado", "Inactivo")),
           activo = ifelse(cae_general2 == "Ocupado" | cae_general2=="Desocupado",1,0), 
           edad_activ = ifelse(cae_general2 != "Menor de 15",1,0), 
           desocupado = ifelse(cae_general2=="Desocupado",1,0), 
           ocupado = ifelse(cae_general2 == "Ocupado",1,0), 
           fact_cal = gsub(",",".", fact_cal) 
    ) 
}

 k = round(n/3,0)

# k = n  
    
alcance = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, c(1:16))
 
 
alcance_nom = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
                 "Valparaíso", "O'Higgins", "Maule", "Biobío", 
                 "La Araucanía", "Los Lagos", "Aysén", "Magallanes", 
                 "Metropolitana de Santiago", "Los Ríos", "Arica y Parinacota", "Ñuble", "Nacional")


# # 2019 : 108
# mbm = microbenchmark(
  ene_ = pblapply(1:length(alcance_nom), function(y) pblapply(c(n-12,n), function(x) select_cols_(c("ano_trimestre",  
                                                                                                "mes_central", "cae_general", "cae_especifico", 
                                                                                                "fact_cal", "estrato", "id_directorio", "region", 
                                                                                                "edad", "sexo","tramo_edad", "categoria_ocupacion", "b1", 
                                                                                                "nivel", "termino_nivel"),
                                                                                              nenes[x]) %>% 
                                                                as.data.table %>% 
                                                                filter(!is.na(id_directorio)) %>% 
                                                                filter(!is.na(fact_cal)) %>% 
                                                                filter(region %in% alcance[[y]]) %>% 
                                                                new_vars %>%    
                                                                #                   mutate_all(as.numeric) %>% 
                                                                mutate(alcance = alcance_nom[y], 
                                                                       trimestre = trimestres[x])))
  # ,
  # times = 100
#)

ene_ = unlist(ene_, recursive = FALSE)


beepr::beep(sound = 8)


k = n



####################################################################################################
# % de empleo según sector económico ===============================================================
####################################################################################################

path = "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/20200717_Coyuntura_ENE/"


n = length(ene_)/2


  options(survey.lonely.psu = "certainty") 
  
  ind = pblapply(1:(2*n), function(x) ene_[[x]] %>% 
                   as_survey_design(ids = id_directorio, strata = estrato, weights = fact_cal) %>% 
                   group_by(trimestre, alcance, b14_rev4cl_caenes,  tramo_tamaño) %>% 
                   mutate(mujer = ifelse(sexo == 2,1,0)) %>% 
                   summarise(
                     ocupados = survey_total(ocupado, vartype = "cv"), 
                     pet = survey_total(edad_activ, vartype = "cv"),
                     n_estratos = unweighted(n_distinct(estrato)),
                     n_conglomerados = unweighted(n_distinct(id_directorio)), 
                     n_ocup = unweighted(sum(ocupado)), 
                     n_pet = unweighted(sum(edad_activ)), 
                     prop_media = survey_mean(media_completa, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
                     prop_inform = survey_mean(ocup_inform, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
                     prop_mujer = survey_mean(mujer, proportion = TRUE, na.rm = TRUE, vartype = "se"), 
                     prop_edad = survey_mean(edad_obj, proportion = TRUE, na.rm = TRUE, vartype = "se"),
                     n_media = unweighted(sum(media_completa)),
                     n_inform = unweighted(sum(ocup_inform)),
                     n_mujer = unweighted(sum(mujer)),
                     n_edad = unweighted(sum(edad_obj))
                   )
  ) %>% bind_rows  %>% 
    mutate(se_max = ifelse(prop_media<0.5, (prop_media^(2/3))/4, ((1-prop_media)^(2/3))/9))
  
  
cacho =   ind %>% filter(alcance == "Ñuble" | alcance == "Nacional", b14_rev4cl_caenes == 7) %>% 
  mutate(year = year(trimestre)) %>% 
  group_by(alcance) %>% 
  mutate(media = mean(prop_media), 
         inform = mean(prop_inform), 
         mujer = mean(prop_mujer), 
         edad_obj = round(mean(prop_edad)*100,1))


####################################################################################################
# Indicadores generales ===========================================================================
####################################################################################################

path = "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/20200717_Coyuntura_ENE/"

k = length(dbListTables(conn = con))

n = length(list.files(pattern = ".csv$"))

if (n>=k){
  options(survey.lonely.psu = "certainty") 
  
  ind = pblapply(1:(2*k), function(x) ene_[[x]] %>% 
                   group_by(trimestre, alcance) %>% 
                   summarise(
                     desocupados = sum(desocupado*fact_cal, na.rm = TRUE), 
                     activos = sum(activo*fact_cal,  na.rm = TRUE), 
                     ocupados = sum(ocupado*fact_cal, na.rm = TRUE), 
                     edad_activ = sum(edad_activ*fact_cal, na.rm = TRUE), 
                     desocupados_ampliado = sum(((cae_general %in% c(4,5,7,8)) | cae_especifico == 10)*fact_cal, 
                                                         na.rm = TRUE), 
                     activos_ampliado = sum(((cae_general %in% c(1,2,3,4,5,7,8)) | cae_especifico == 10)*fact_cal, 
                                                     na.rm = TRUE))
  ) %>% bind_rows
  
  indicadores = ind %>% 
    dplyr::select(trimestre, alcance, desocupados, activos,
                  ocupados, edad_activ, desocupados_ampliado, activos_ampliado) %>% 
    mutate(desocupacion = round((desocupados/activos)*100,1), 
           participacion = round((activos/edad_activ)*100,1), 
           ocupacion = round((ocupados/edad_activ)*100,1), 
           desocupacion_ampliada = round((desocupados_ampliado/activos_ampliado)*100,1))
  
} else {
  indicadores = read_excel(path = paste0(path, "20200717_coyuntura_ene.xlsx"), sheet = "indicadores", range = paste0("B3:M",3+2*n))  
}

variable = c("participacion", "ocupacion","desocupacion",  "desocupacion_ampliada")

variable_nom = c("Participación (%)", "Ocupación (%)", "Desocupación (%)", "Desocupación Ampliada (%)")

max_ = sapply(indicadores, max, na.rm = TRUE)[variable] %>% as.numeric
min_ = sapply(indicadores, min, na.rm = TRUE)[variable] %>% as.numeric

plots = lapply(1:4, function(x) indicadores %>%  
                 arrange(trimestre) %>% 
                 ggplot(aes(x = as.Date(trimestre, format = "%Y-%m-%d", "2019-02-01"), y = !! sym(variable[x]), colour = alcance)) + 
                 geom_line(size = 1.5) +
                 geom_text_repel(data = filter(indicadores, trimestre %in% trimestres[c(n-2,n)]),
                           aes_string(label = variable[x]),
                           hjust = -.6, arrow = arrow(length = unit(0.01, "npc"), type = "closed", ends = "first"))+
                 geom_hline(yintercept = seq(min_[x],
                                             max_[x],
                                             (max_[x]-min_[x])/10),
                            colour = "grey30", size = .5, linetype = "dotted")+
                 scale_colour_manual(values = c("blue", "red"))+
                 scale_x_date(breaks = scales::date_breaks("3 month"), labels = format_quarters)+
                 labs(y = variable_nom[x],
                      x = "Trimestre móvil",
                      colour = " ")+
                 theme(axis.line = element_line(size = 1), 
                       panel.background = element_blank(), 
                       #panel.border = element_blank(), 
                       axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
                       text = element_text(size = 18),
                       #strip.background=element_blank(),
                       panel.grid = element_blank(),
                       #axis.ticks=element_blank(),
                       legend.position = "bottom",
                       legend.text=element_text(size=15), 
                       axis.text = element_text(size = 20), 
                       axis.title = element_text(size = 15), 
                       legend.key.size = unit(1.5,"cm"), 
                       legend.title = element_text(size = 20), 
                       plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
                       legend.key = element_rect(colour = "transparent", fill = "white")))
plots



iwalk(plots, ~{
  png(paste0(path, gsub(" ", "_", variable[.y]),".png"), width=10.6664,height=8, 
      units = "in", res = 300)
  print(.x)
  dev.off()
}) 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plots[[1]])

png(paste0(path, "desocupacion_desocupacion_ampliada.png"), width=10.6664,height=8, 
    units = "in", res = 300)
grid.arrange(arrangeGrob(plots[[3]]+theme(legend.position = "none", 
                                          axis.text.x = element_blank(), 
                                          axis.title.x = element_blank()), 
                         plots[[4]]+theme(legend.position = "none"), ncol = 1, heights = c(4,6)), mylegend, nrow = 2, 
             heights=c(10, 1))
dev.off()

png(paste0(path, "ocupacion_participacion.png"), width=10.6664,height=8, 
    units = "in", res = 300)
grid.arrange(arrangeGrob(plots[[1]]+theme(legend.position = "none", 
                                          axis.text.x = element_blank(), 
                                          axis.title.x = element_blank()), 
                         plots[[2]]+theme(legend.position = "none"), ncol = 1, 
                         heights = c(4,6)), mylegend, nrow = 2, 
             heights=c(10, 1))
dev.off()




indicadores_ =   lapply(1:2, function(x) indicadores %>%
                         filter(alcance == alcance_nom[x]) %>% 
  arrange(trimestre) %>% 
    ungroup() %>% 
  mutate(var_ocupados = round((log(ocupados)-log(lag(ocupados,n = 12)))*100,1))) %>% 
  bind_rows


max_ = sapply(indicadores_, max, na.rm = TRUE)["var_ocupados"] %>% as.numeric
min_ = sapply(indicadores_, min, na.rm = TRUE)["var_ocupados"] %>% as.numeric


plot_var_ocupados = lapply(1:1, function(x) indicadores_ %>% 
                 ggplot(aes(x = as.Date(trimestre, format = "%Y-%m-%d"), y = var_ocupados, colour = alcance)) + 
                 geom_line(size = 1.5) +
                 geom_text(data = filter(indicadores_, trimestre == max(trimestre)),
                           aes_string(label = "var_ocupados"),
                                      hjust = -.2)+
                 geom_hline(yintercept = seq(min_, max_, (max_-min_)/10), colour = "grey30", size = .5, linetype = "dotted")+
                 scale_colour_manual(values = c("blue", "red"))+
                 scale_x_date(breaks = scales::date_breaks("3 month"), labels = format_quarters)+
                 labs(y = "Creación/Destrucción de empleo (%)",
                      x = "Trimestre móvil",
                      colour = " ")+
                 theme(axis.line = element_line(size = 1), 
                       panel.background = element_blank(), 
                       #panel.border = element_blank(), 
                       axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
                       text = element_text(size = 18),
                       #strip.background=element_blank(),
                       panel.grid = element_blank(),
                       #axis.ticks=element_blank(),
                       legend.position = "bottom",
                       legend.text=element_text(size=15), 
                       axis.text = element_text(size = 20), 
                       axis.title = element_text(size = 15), 
                       legend.key.size = unit(1.5,"cm"), 
                       legend.title = element_text(size = 20), 
                       plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
                       legend.key = element_rect(colour = "transparent", fill = "white")))

plot_var_ocupados

png(paste0(path, "plot_var_ocupados.png"), width=10.6664,height=8, 
    units = "in", res = 300)
plot_var_ocupados
dev.off()




# **** Envío a Planillas Excel =============================================

wb = createWorkbook()



if (("Índice" %in% sheets(wb)) == TRUE){
  removeWorksheet(wb, sheet = "Índice")
  addWorksheet(wb, sheetName = "Índice", gridLines = FALSE)
} else {
  addWorksheet(wb, sheetName = "Índice", gridLines = FALSE)
}

setColWidths(wb, sheet = "Índice", cols = c(2,3), widths = 30)

n_ = dim(indicadores)[1]

if (("indicadores" %in% sheets(wb)) == TRUE){
    removeWorksheet(wb, sheet = "indicadores")
    addWorksheet(wb, sheetName = indicadores, gridLines = FALSE)
  } else {
    addWorksheet(wb, sheetName = "indicadores", gridLines = FALSE)
  }


  # addStyle(wb = wb, sheet = "indicadores", rows = 1, cols = 1, style = mts)
  setColWidths(wb, sheet = "indicadores", cols = 2:19, widths = 20)  
  writeData(wb, sheet = "indicadores",
            x = "indicadores",
            startRow = 1, startCol = 2)
  writeData(wb, sheet = "indicadores",
            x = indicadores,
            startRow = 3, startCol = 2, 
            colNames = TRUE, rowNames = FALSE,
            keepNA = FALSE, 
            withFilter = FALSE)
  writeData(wb, sheet = "indicadores",
            x = "Fuente: Observatorio Laboral Nacional",
            startRow = n_+4, startCol = 2)
  writeFormula(wb, sheet ="indicadores", startRow = 1, startCol = 1
               , x = makeHyperlinkString(sheet = "Índice",
                                         row = 1, col = 1, 
                                         text = "Índice"))
  

saveWorkbook(wb, file = paste0(path, "20200717_coyuntura_ene.xlsx"), overwrite = TRUE)



####################################################################################################
# Global mobility report =================================================================
####################################################################################################

# Aquí puedo usar el paquete sqldf, OJO!


gbr = fread("C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Google Mobility Report/Global_Mobility_Report.csv", 
            encoding = "UTF-8") %>% 
  filter(country_region == "Chile")


gbr_nuble = gbr %>% 
  filter(sub_region_2 == "Diguillín Province" | sub_region_2 == "Punilla Province" | sub_region_2 == "Itata Province")

provincias = names(table(gbr_nuble$sub_region_2))
provincias_ = gsub(" Province", "", names(table(gbr_nuble$sub_region_2)))

plot_gbr_nuble = lapply(1:3, function(x) gbr_nuble %>%
                          filter(sub_region_2 == provincias[[x]]) %>%
                            select(sub_region_2, date, 
                                   retail_and_recreation_percent_change_from_baseline, 
                                   grocery_and_pharmacy_percent_change_from_baseline, 
                                   parks_percent_change_from_baseline, 
                                   transit_stations_percent_change_from_baseline, 
                                   workplaces_percent_change_from_baseline, 
                                   residential_percent_change_from_baseline) %>% 
                             reshape2::melt(id=c("sub_region_2", "date")) %>% 
                             mutate(value = rollmean(x = value, 7, align = "right", fill = NA)) %>% 
                             ggplot(aes(x = as.Date(date, format = "%Y-%m-%d"), y = value, colour = variable)) + 
                             geom_line(size = 1.5) +
                             scale_colour_discrete(labels = c("Tiendas y ocio", 
                                                              "supermercados y farmacia", 
                                                              "Parques", 
                                                              "Estaciones de transporte", 
                                                              "Lugar de trabajo", 
                                                              "zonas residenciales"))+
                             labs(y = "Índice de movilidad Google",
                                  x = "Fecha",
                                  colour = " ", 
                                  title = paste0("Provincia: ",provincias_[x]))+
                             theme(axis.line = element_line(size = 1), 
                                   panel.background = element_blank(), 
                                   #panel.border = element_blank(), 
                                   axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
                                   text = element_text(size = 18),
                                   #strip.background=element_blank(),
                                   panel.grid = element_blank(),
                                   #axis.ticks=element_blank(),
                                   legend.position = "bottom",
                                   legend.text=element_text(size=10), 
                                   axis.text = element_text(size = 20), 
                                   axis.title = element_text(size = 15), 
                                   legend.key.size = unit(1.5,"cm"), 
                                   legend.title = element_text(size = 20), 
                                   plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
                                   legend.key = element_rect(colour = "transparent", fill = "white")))

plot_gbr_nuble



mylegend<-g_legend(plot_gbr_nuble[[1]])

png(paste0(path, "Índice de movilidad de google.png"), width=12,height=8, 
    units = "in", res = 300)
grid.arrange(arrangeGrob(plot_gbr_nuble[[1]]+theme(legend.position = "none"), 
                         plot_gbr_nuble[[2]]+theme(legend.position = "none"),
                         plot_gbr_nuble[[3]]+theme(legend.position = "none"),
                         ncol = 3), mylegend, nrow = 2, 
             heights=c(4,1))
dev.off()


grid.arrange(plot_gbr_nuble[[1]], plot_gbr_nuble[[2]], plot_gbr_nuble[[3]], ncol = 3)

iwalk(plot_gbr_nuble, ~{
  png(paste0(path, gsub(" ", "_", provincias[.y]),".png"), width=10.6664,height=8, 
      units = "in", res = 300)
  print(.x)
  dev.off()
}) 




# # ***************************************************************************
# * Indicador mensual de confianza empresarial Sector Construcción ============
# # ***************************************************************************  

# **** Manipulación de datos =================================================
path2 = "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/"


imce = read_excel(paste0(path2,"20201014 Índice Mensual de Confianza Empresarial (IMCE).xlsx"), range = "A3:E132")

imce = imce %>% mutate(Periodo = gsub("\\.","-", Periodo), 
                       date = seq.Date(as.Date("2010-01-01"),as.Date("2020-09-01"), by = "month"))


# **** Representación gráfica 

png(paste0(path, "20200319 Índice Mensual de Confianza Empresarial (IMCE) - Sector Construcción.png"),width=13,height=10.208331, 
    units = "in", res = 300)
imce %>% 
  mutate(year = year(date)) %>% 
  filter(year>=2018) %>% 
ggplot(aes(x=`date`)) +
  geom_line(aes(y = `1. IMCE: Comercio`,
                colour = "Comercio"), size = 1.5)+
  geom_line(aes(y = `2. IMCE: Construcción`,
                colour = "Construcción"), size = 1.5)+
  geom_line(aes(y = `3. IMCE: Industria`,
                colour = "Industria"), size = 1.5)+
  geom_line(aes(y = `4. IMCE: Minería`, colour = "Minería"), size = 1.5)+
  theme(axis.line = element_line(size = 1), 
        panel.background = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.x = element_text(size = 20, angle = 90, hjust = .5), 
        text = element_text(size = 18),
        #strip.background=element_blank(),
        panel.grid = element_blank(),
        #axis.ticks=element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=15), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 15), 
        legend.key.size = unit(1.5,"cm"), 
        legend.title = element_text(size = 20), 
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  labs(y = "IMCE", 
       x = "Mes", 
       colour = "")+
  scale_x_date(date_labels = "%m-%Y",breaks = "3 month")
dev.off()

# # ***************************************************************************
# * Sectores ============
# # ***************************************************************************  


ind_sect = pblapply(1:((k+1)-73), function(x) ene_2[[x]] %>% 
                      tibble %>% 
                      mutate(trimestre = trimestres[72+x], 
                             fact_cal = as.numeric(fact_cal)) %>% 
                 group_by(trimestre, alcance, rama) %>% 
                 summarise(ocupados = sum(ocupado*fact_cal, na.rm = TRUE))
) %>% bind_rows %>% 
  mutate(year = year(trimestre), 
         month = month(trimestre)) %>% 
  filter(year>=2019, month == 7)

nb.cols = 2
mycolors = colorRampPalette(brewer.pal(min(9,nb.cols), "Blues"))(nb.cols)


png(paste0(path, "20201021 Número de ocupados por sector económico.png"),width=12,height=6, 
    units = "in", res = 300)
ind_sect %>% 
  filter(rama!="(Missing)") %>% 
ggplot(aes(x= fct_drop(reorder(rama, -ocupados)), fill = as.factor(year))) + 
  geom_bar(aes(y = ocupados), stat = "identity",position = "dodge", colour = "black") + 
  # geom_bar(data=subset(ind_sect,year == "2019"),
  #           aes(y=ocupados,group = year), stat = "identity", 
  #           size = 1.5, fill = "red") + 
  labs(y = "Número de ocupados", x = "", fill = "", colour = "")+
  geom_text(aes(x = as.factor(rama), y = ocupados,
                label=format(round(ocupados), big.mark = ".",
                             scientific = FALSE), group = year, colour = as.factor(year)), position=position_dodge(width = 1), 
            hjust = -.3, size = 5, show.legend = FALSE)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                            scientific = FALSE), limits = c(0,45000))+
  scale_fill_manual(values = c( "#DEEBF7","#3182BD"))+
  scale_colour_manual(values = c( "#3182BD","#3182BD"))+
#  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  theme(axis.text.y = element_text(size = 12))+
  theme(#axis.line = element_line(size = 1), 
        panel.background = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.x = element_text(size = 12, angle = 0, hjust = .5), 
        text = element_text(size = 18),
        #strip.background=element_blank(),
        panel.grid = element_blank(),
        #axis.ticks=element_blank(),
        legend.position = c(.9,.9),
        legend.text=element_text(size=15), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 15), 
        legend.key.size = unit(1,"cm"), 
        legend.title = element_text(size = 20), 
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))
dev.off()  


## CATEGORÍA OCUPACIONAL

categorias = c(`0` = "Menor de 15 años",
               `1` = "Ocupado tradicional", 
               `2` = "Ocupado no tradicional", 
               `3` = "Ocupado ausente", 
               `4` = "Cesante", 
               `5` = "Busca trabajo por primera vez", 
               `6` = "Iniciador", 
               `7` = "Inactivos que buscaron", 
               `8` = "Inactivos que estuvieron disponibles", 
               `9` = "Inactivos que no buscaron ni estuvieron disponibles")

ind_cat_ = pblapply(1:((k+1)-73), function(x) ene_2[[x]] %>% 
                      tibble %>% 
                      mutate(trimestre = trimestres[72+x], 
                             fact_cal = as.numeric(fact_cal), 
                             cae_general = recode(cae_general, !!! categorias)) %>% 
                      group_by(trimestre, cae_general) %>% 
                      summarise(ocupados = sum(fact_cal, na.rm = TRUE))
) %>% bind_rows %>% 
  mutate(year = year(trimestre), 
         month = month(trimestre))

ind_cat = ind_cat_ %>% 
  filter(year>=2019, month == 7)

write.csv(ind_cat_, paste0(path, "cae_ene.csv"))

nb.cols = 2
mycolors = colorRampPalette(brewer.pal(min(9,nb.cols), "Blues"))(nb.cols)

png(paste0(path, "20200825 Número de ocupados por categoría ocupacional.png"),width=12,height=6, 
    units = "in", res = 300)
ind_cat %>% 
  filter(cae_general!="Menor de 15 años") %>% 
  ggplot(aes(x= fct_drop(reorder(cae_general, -ocupados)), fill = as.factor(year))) + 
  geom_bar(aes(y = ocupados), stat = "identity",position = "dodge", colour = "black") + 
  # geom_bar(data=subset(ind_sect,year == "2019"),
  #           aes(y=ocupados,group = year), stat = "identity", 
  #           size = 1.5, fill = "red") + 
  labs(y = "Número de personas", x = "", fill = "", colour = "")+
  geom_text(aes(x = as.factor(cae_general), y = ocupados,
                label=format(round(ocupados), big.mark = ".",
                             scientific = FALSE), group = year, colour = as.factor(year)), position=position_dodge(width = 1), 
            hjust = -.3, size = 5, show.legend = FALSE)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                                                 scientific = FALSE), limits = c(0,205000))+
  scale_fill_manual(values = c( "#DEEBF7","#3182BD"))+
  scale_colour_manual(values = c( "#3182BD","#3182BD"))+
  #  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  theme(axis.text.y = element_text(size = 12))+
  theme(#axis.line = element_line(size = 1), 
    panel.background = element_blank(), 
    #panel.border = element_blank(), 
    axis.text.x = element_text(size = 12, angle = 0, hjust = .5), 
    text = element_text(size = 18),
    #strip.background=element_blank(),
    panel.grid = element_blank(),
    #axis.ticks=element_blank(),
    legend.position = c(.9,.9),
    legend.text=element_text(size=15), 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 15), 
    legend.key.size = unit(1,"cm"), 
    legend.title = element_text(size = 20), 
    plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
    legend.key = element_rect(colour = "transparent", fill = "white"))
dev.off()  




# # ***************************************************************************
# * Sectores ============
# # ***************************************************************************  


ind_sectores = pblapply(1:((k+1)-73), function(x) ene_2[[x]] %>% 
                      tibble %>% 
                      mutate(trimestre = trimestres[72+x], 
                             fact_cal = as.numeric(fact_cal)) %>% 
                      group_by(trimestre, alcance, rama) %>% 
                      summarise(ocupados = sum(ocupado*fact_cal, na.rm = TRUE)
)) %>% bind_rows %>% 
  mutate(year = year(trimestre), 
         month = month(trimestre)) %>% 
  filter(year>=2019, month == 5)


indicadores_sectores = indicadores %>% 
  filter(alcance == "Ñuble") %>% 
  mutate(anio = year(trimestre) %>% as.character, 
         mes = month(trimestre) %>% as.character) %>% 
  dplyr::select(anio, mes, trimestre, desocupados, activos,
                ocupados, edad_activ, desocupados_ampliado, activos_ampliado) %>% 
  mutate(desocupacion = round((desocupados/activos)*100,1), 
         participacion = round((activos/edad_activ)*100,1), 
         ocupacion = round((ocupados/edad_activ)*100,1), 
         desocupacion_ampliada = round((desocupados_ampliado/activos_ampliado)*100,1))


ocupados_plot = indicadores_sectores %>%  
  filter(anio>=2015) %>%
  mutate(mes = case_when(mes == 1~"Dic-Feb", 
                         mes == 2~"Ene-Mar",
                         mes == 3~"Feb-Abr", 
                         mes == 4~"Mar-May", 
                         mes == 5~"Abr-Jun", 
                         mes == 6~"May-Jul", 
                         mes == 7~"Jun-Ago", 
                         mes == 8~"Jul-Sep", 
                         mes == 9~"Ago-Oct", 
                         mes == 10~"Sep-Nov", 
                         mes == 11~"Oct-Dic", 
                         mes == 12~"Nov-Ene"), 
         mes = factor(mes, levels = c("Dic-Feb", "Ene-Mar", 
                                      "Feb-Abr", "Mar-May", "Abr-Jun", 
                                      "May-Jul", "Jun-Ago", "Jul-Sep", 
                                      "Ago-Oct", "Sep-Nov", "Oct-Dic", 
                                      "Nov-Ene"))) %>% 
  ggplot(aes(x = mes, y = ocupados, colour = anio, group = anio)) + 
  geom_line(aes(group = anio), size = 1.5) +
  labs(y = "N° Ocupados",
       x = "Trimestre móvil",
       colour = " ")+
  theme(axis.line = element_line(size = 1), 
        panel.background = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
        text = element_text(size = 18),
        #strip.background=element_blank(),
        panel.grid = element_blank(),
        #axis.ticks=element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=15), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 15), 
        legend.key.size = unit(1.5,"cm"), 
        legend.title = element_text(size = 20), 
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))



png(paste0(path, "20200822_ocupados_nuble.png"), width=10.6664,height=8, 
    units = "in", res = 300)
ocupados_plot
dev.off()


png(paste0(path, "20200825 Número de inactivos por razón.png"),width=15,height=8, 
    units = "in", res = 300)
ind_inactivo %>% 
  na.omit() %>% 
  # filter(rama!="(Missing)") %>% 
  ggplot(aes(x= fct_drop(reorder(e9, -inactivos)), fill = as.factor(year))) + 
  geom_bar(aes(y = inactivos), stat = "identity",position = position_dodge(preserve = "single"), colour = "black") + 
  # geom_bar(data=subset(ind_sect,year == "2019"),
  #           aes(y=ocupados,group = year), stat = "identity", 
  #           size = 1.5, fill = "red") + 
  labs(y = "Número de Inactivos", x = "", fill = "", colour = "")+
  geom_text(aes(x = as.factor(e9), y = inactivos,
                label=format(round(inactivos), big.mark = ".",
                             scientific = FALSE), group = year, colour = as.factor(year)),
            position=position_dodge(width = 1), 
            hjust = -.3, size = 5)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                                                 scientific = FALSE), limits = c(0,45000))+
  #  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  theme(axis.text.y = element_text(size = 12))+
  theme(#axis.line = element_line(size = 1), 
    panel.background = element_blank(), 
    #panel.border = element_blank(), 
    axis.text.x = element_text(size = 12, angle = 0, hjust = .5), 
    text = element_text(size = 18),
    #strip.background=element_blank(),
    panel.grid = element_blank(),
    #axis.ticks=element_blank(),
    legend.position = c(.9,.9),
    legend.text=element_text(size=15), 
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 15), 
    legend.key.size = unit(1,"cm"), 
    legend.title = element_text(size = 20), 
    plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
    legend.key = element_rect(colour = "transparent", fill = "white"))
dev.off()  



## Acceso al crédito 




x = "20200427_Versión definitiva de Covid-19 y su impacto en el mercado del trabajo de la región de Ñuble (respuestas).xlsx" 

encuesta = read_excel(paste0(path2,x)) %>% 
  filter(A6_ == "Ñuble")



nb.cols =     4

mycolors = colorRampPalette(brewer.pal(min(9,nb.cols), "Blues"))(nb.cols)


D15 = encuesta %>%
  select_at(vars(sexo_, fact_, contains("D15"))) %>%
  gather(key = "D15", value = "Opinion", contains("D15")) %>% 
  mutate(D15 = recode(D15, 'D15_' = 'La mujer que se queda en casa es mejor madre', 
                      'D15_2' = 'La mujer que trabaja es más interesante como persona', 
                      'D15_3' = 'Si la mujer no trabaja los hijos tienen mejor rendimiento escolar', 
                      'D15_4' = 'Si la mujer trabaja es más probable que la pareja se separe'),  
         D15 = as.factor(D15)) %>% 
  filter(!is.na(Opinion)) %>%
  group_by(D15, Opinion, sexo_, .drop = TRUE) %>%
  mutate(total = n(), 
         fact_ = as.numeric(fact_)) %>%
  ungroup() %>% 
  group_by(sexo_, D15) %>% 
  mutate(totales = sum(fact_)) %>%
  filter(Opinion!="Falso") %>% 
  as_survey_design(weights  = fact_) %>%
  group_by(sexo_, Opinion, D15, .drop = TRUE) %>%
  summarise(total = survey_total(na.rm = TRUE), 
            empresas = unweighted(n()), 
            totales = unweighted(totales[1])) %>% 
  mutate(pct = round((total/totales)*100,1)) %>% 
  bar_chart_2(D15, pct, title = "")+
  facet_wrap(~sexo_)+
  labs(y = "%", fill = "")+
  scale_fill_manual(values = mycolors,
                    labels = function(x) str_wrap(x, width = 25))+
#  labs(y = "%", subtitle = "", fill = "", title = "Dificultades asociadas al teletrabajo")+
  theme(axis.text.x = element_blank(), 
        legend.position = "left")+
  guides(fill = guide_legend(nrow = 7))

png(paste0(path, "20200826 Estereotipos de género.png"),width=15,height=8, 
    units = "in", res = 300)
D15
dev.off()
D15 = grid.arrange(D15, bottom = textGrob(paste0("Pregunta: ",str_wrap(title, width = 80), "\n número de casos: ", casos, 
                                                 "\n Nota: Responden sólo los individuos que declararon trabajar desde su hogar"), x = 1, hjust = 1, gp = gpar(fontface = 3L, fontsize = 25)))





# **** Manipulación de datos =================================================
path2 = "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/"




# # ***************************************************************************
# * Expectativas IMACEC ============
# # ***************************************************************************  

# **** Manipulación de datos =================================================
path2 = "C:/Users/omen03/Google Drive/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/"


imacec = read_excel(paste0(path2,"Expectativas IMACEC.xlsx"))

imce = imce %>% mutate(Periodo = gsub("\\.","-", Periodo), 
                       date = seq.Date(as.Date("2010-01-01"),as.Date("2020-07-01"), by = "month"))


# **** Representación gráfica 

png(paste0(path, "20200319 Índice Mensual de Confianza Empresarial (IMCE) - Sector Construcción.png"),width=12,height=6, 
    units = "in", res = 300)
imce %>% 
  mutate(year = year(date)) %>% 
  filter(year>=2018) %>% 
  ggplot(aes(x=`date`)) +
  geom_line(aes(y = `1. IMCE: Comercio`,
                colour = "Comercio"), size = 1.5)+
  geom_line(aes(y = `2. IMCE: Construcción`,
                colour = "Construcción"), size = 1.5)+
  geom_line(aes(y = `3. IMCE: Industria`,
                colour = "Industria"), size = 1.5)+
  geom_line(aes(y = `4. IMCE: Minería`, colour = "Minería"), size = 1.5)+
  theme(axis.line = element_line(size = 1), 
        panel.background = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
        text = element_text(size = 18),
        #strip.background=element_blank(),
        panel.grid = element_blank(),
        #axis.ticks=element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=15), 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 15), 
        legend.key.size = unit(1.5,"cm"), 
        legend.title = element_text(size = 20), 
        plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  labs(y = "IMCE", 
       x = "Mes", 
       colour = "")+
  scale_x_date(date_labels = "%m-%Y",breaks = "3 month")
dev.off()




####################################################################################################
# Indicadores generales - por sexo =================================================================
####################################################################################################

options(survey.lonely.psu = "certainty") 
ind_sexo = pblapply(1:k, function(x) ene_[[x]] %>% 
                 group_by(trimestre, sexo) %>% 
                 summarise(
                   desocupados = survey_total(desocupado, na.rm = TRUE, vartype = "cv"), 
                   activos = survey_total(activo,  na.rm = TRUE, vartype = "cv"), 
                   ocupados = survey_total(ocupado, na.rm = TRUE, vartype = "cv"), 
                   edad_activ = survey_total(edad_activ, na.rm = TRUE, vartype = "cv"), 
                   desocupados_ampliado = survey_total((cae_general %in% c(4,5,7,8)) | cae_especifico == 10, 
                                                       na.rm = TRUE, vartype = "cv"), 
                   activos_ampliado = survey_total((cae_general %in% c(1,2,3,4,5,7,8)) | cae_especifico == 10, 
                                                   na.rm = TRUE, vartype = "cv"))
) %>% bind_rows

indicadores_sexo = ind_sexo %>% 
  dplyr::select(trimestre, sexo, desocupados, activos,
                ocupados, edad_activ, desocupados_ampliado, activos_ampliado) %>% 
  mutate(desocupacion = round((desocupados/activos)*100,1), 
         participacion = round((activos/edad_activ)*100,1), 
         ocupacion = round((ocupados/edad_activ)*100,1), 
         desocupacion_ampliada = round((desocupados_ampliado/activos_ampliado)*100,1))


variable = c("participacion", "ocupacion","desocupacion",  "desocupacion_ampliada")

variable_nom = c("Participación (%)", "Ocupación (%)", "Desocupación (%)", "Desocupación Ampliada (%)")

max_ = sapply(indicadores_sexo, max)[variable] %>% as.numeric
min_ = sapply(indicadores_sexo, min)[variable] %>% as.numeric

plots_sexo = lapply(1:4, function(x) indicadores_sexo %>% 
                 ggplot(aes(x = as.Date(trimestre, format = "%Y-%m-%d"), y = !! sym(variable[x]),
                            colour = factor(sexo, levels = c(1,2), labels = c("Hombre", "mujer")))) + 
                 geom_line(size = 1.5) +
                 geom_text(data = filter(indicadores_sexo, trimestre == max(trimestre)),
                           aes(label = !! sym(variable[x]),
                                      hjust = -.2))+
                 geom_hline(yintercept = seq(min_[x], max_[x], (max_[x]-min_[x])/10), colour = "grey30", size = .5, linetype = "dotted")+
                 scale_colour_manual(values = c("blue", "red"))+
                 scale_x_date(breaks = scales::date_breaks("3 month"), labels = format_quarters)+
                 labs(y = variable_nom[x],
                      x = "Trimestre móvil",
                      colour = " ")+
                 theme(axis.line = element_line(size = 1), 
                       panel.background = element_blank(), 
                       #panel.border = element_blank(), 
                       axis.text.x = element_text(size = 10, angle = 90, hjust = .5), 
                       text = element_text(size = 18),
                       #strip.background=element_blank(),
                       panel.grid = element_blank(),
                       #axis.ticks=element_blank(),
                       legend.position = "bottom",
                       legend.text=element_text(size=15), 
                       axis.text = element_text(size = 20), 
                       axis.title = element_text(size = 15), 
                       legend.key.size = unit(1.5,"cm"), 
                       legend.title = element_text(size = 20), 
                       plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
                       legend.key = element_rect(colour = "transparent", fill = "white")))
plots_sexo


iwalk(plots_sexo, ~{
  png(paste0(path, gsub(" ", "_", variable[.y]),".png"), width=10.6664,height=8, 
      units = "in", res = 300)
  print(.x)
  dev.off()
}) 





beepr::beep(sound = 8)

options(survey.lonely.psu = "certainty") 
ind_2 = pblapply(1:k, function(x) ene_3[[1+(x-1)*3]] %>% 
                   mutate(indicadores = map(encuesta, ~.x %>% 
                                              summarise(unemp = survey_ratio(desocupado,activo,  na.rm = TRUE, vartype = "cv"), 
                                                        lfpr = survey_ratio(activo, edad_activ, na.rm = TRUE, vartype = "cv")) 
                   )) %>%  
                   mutate(fecha = trimestres[1+(x-1)*3], 
                          year = year(fecha), 
                          quarter = quarter(fecha)) %>% 
                   unnest(indicadores)) %>% 
  bind_rows %>% 
  select(fecha, year, quarter, region, tramo_edad, unemp, lfpr) 


# setwd("C:/Users/omen03/Google Drive/Héctor Garrido/Added worker effect/")
# 
# fwrite(ind, file = "20200531 Panel por sexo.csv")
# 
# fwrite(ind_2, file = "20200531 Panel por sexo y tramo etario.csv")



ventas_region = read_excel(paste0(path2,"20200908 Estadísticas de Empresas por Región y Subrubro económico.xlsx")) %>% 
  filter(`Region del domicilio o casa matriz` == "Región de Ñuble")  


theme_  = theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1), 
                text = element_text(size = 18),
                strip.background=element_blank(),
                # panel.border = element_blank(),
                # panel.grid = element_blank(),
                axis.ticks=element_blank(),
                legend.position = "bottom",
                legend.text=element_text(size=15), 
                axis.text = element_text(size = 20), 
                axis.title = element_text(size = 25), 
                legend.key.size = unit(.75,"cm"), 
                legend.title = element_text(size = 20))




ventas_sector_ = ventas_region %>% 
  mutate(ventas =  as.numeric(`Ventas anuales en UF`), 
         `Subrubro economico` = as_factor(`Subrubro economico`)) %>%
  filter(ventas>=0) %>% 
  group_by(`Año Comercial`, `Subrubro economico`, `Rubro economico`) %>% 
  summarise(ventas = sum(ventas)) %>% 
  group_by(`Rubro economico`) %>% 
  nest() %>% 
  mutate(ventas = map2(data, `Rubro economico`, ~.x %>% 
                         group_by(`Año Comercial`) %>% 
                         mutate(ventas_y = sum(ventas, na.rm = TRUE)) %>% 
                         group_by(`Año Comercial`,`Subrubro economico`) %>% 
                         summarise(ventas = sum(ventas, na.rm = TRUE), 
                                   pct = (ventas/ventas_y)*100 %>% round(1)) %>% 
                         ungroup() %>% 
                         mutate(`Subrubro economico` = case_when(pct<=5~"Otros", 
                                                                 TRUE~as.character(`Subrubro economico`))) %>%
                         group_by(`Año Comercial`,`Subrubro economico`) %>% 
                         summarise(ventas = sum(ventas, na.rm = TRUE), 
                                   pct = sum(pct, na.rm = TRUE)) %>% 
                         ggplot(aes(x = factor(`Año Comercial`), y = ventas,
                                    fill = reorder(`Subrubro economico`, ventas, FUN = sum),
                                    group = reorder(`Subrubro economico`, ventas, FUN = sum), 
                                    label = paste(round(pct,1), "%") ))+
                         geom_bar(position = "stack", stat="identity", color = "black")+
                         geom_text(aes(label = format(paste0(round(pct,1), "%"), big.mark = ".",
                                                      decimal.mark = ",", scientific = FALSE)), 
                                   size = 5, position = position_stack(vjust = 0.5), 
                                   family = "bold")+
                         scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                                      decimal.mark = ",", 
                                                                      scientific = FALSE))+
                         scale_fill_discrete(labels = function(x) str_wrap(x, width = 25))+
                         scale_x_discrete(breaks = 2005L:2018L)+
                         labs(caption = "Fuente: Elaboración propia conforme a Estadísticas del Servicio de Impuestos Internos (SII)", 
                              fill = paste0("Sector ", str_wrap(.y, width = 35)))+
                         xlab("Año comercial")+ylab("Ventas anuales (UF)")+
                         theme_+
                         theme(legend.text = element_text(size = 18, ), 
                               legend.position = "left", 
                               # plot.background=element_rect(fill = "lightskyblue2"), 
                               # legend.background = element_rect(fill = "lightskyblue2"),
                               plot.margin=unit(c(4,1,1.5,5),"cm")                              )+
                         guides(fill = guide_legend(nrow = 8))
  )) 

aspect_ratio = 2.5  

if (!(file.exists("/Reportes sectoriales 2020/20200409 Ventas segun SII - Subrubro Económico - 1.png")) == TRUE){
  iwalk(ventas_sector_$ventas, ~{
    png(paste0(path,"20200409 Ventas segun SII - Subrubro Económico",as.character(.y),".png"),width=9*aspect_ratio,height=9, 
        units = "in", res = 300)
    print(.x)
    dev.off()
  }) 
}


ventas_ = ventas_region %>% 
  group_by(`Rubro economico`, `Subrubro economico`) %>% 
  count()

write.table(ventas_, paste0(path2, "Subsectores SII.csv"), sep = "[", fileEncoding = "Latin1")


# group_by(`Año Comercial`) %>% 
# mutate(`Ventas anuales en UF` = as.numeric(`Ventas anuales en UF`)) %>% 
# summarise(ventas = sum(`Ventas anuales en UF`, na.rm = TRUE)) %>% 
# na.omit() %>% filter(`Año Comercial`>=2013) %>% 
# arrange(ventas) %>% 
# mutate(var = round(((ventas-lag(ventas))/lag(ventas))*100,1))


ventas_region_ = lapply(c(2013, 2018), function(x) ventas_region %>% 
                          mutate(rama = ifelse(`Rubro economico` %in% c("C - Industria manufacturera", "F - Construcción",
                                                                        "H - Transporte y almacenamiento", "I - Actividades de alojamiento y de servicio de comidas", 
                                                                        "A - Agricultura, ganadería, silvicultura y pesca", 
                                                                        "G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas"),
                                               `Rubro economico`, 
                                               ifelse(`Rubro economico` == "(Missing)", NA, "Otros"))) %>%
                          filter(`Año Comercial` == x) %>% 
                          group_by(`Año Comercial`, rama) %>% 
                          summarise(`Ventas anuales en UF` = sum(`Ventas anuales en UF` %>% as.numeric)) %>% 
                          arrange(desc(`Ventas anuales en UF` %>% as.numeric)) %>% 
                          mutate(ventas = `Ventas anuales en UF` %>% as.numeric,
                                 total = sum(ventas), 
                                 cum = cumsum(ventas),
                                 prop = round((ventas/total)*100,1),
                                 labels = paste0(prop, "%")))


ventas_region_plot = ventas_region %>%
  select(`Año Comercial`, `Rubro economico`, `Ventas anuales en UF`) %>%
  mutate(rama = ifelse(`Rubro economico` %in% c("C - Industria manufacturera", "F - Construcción",
                                                "H - Transporte y almacenamiento", "I - Actividades de alojamiento y de servicio de comidas", 
                                                "A - Agricultura, ganadería, silvicultura y pesca", 
                                                "G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas"),
                       `Rubro economico`, 
                       ifelse(`Rubro economico` == "(Missing)", NA, "Otros"))) %>%
  na.omit() %>% 
  group_by(`Año Comercial`, rama) %>%
  summarise(ventas = sum(`Ventas anuales en UF` %>% as.numeric)) %>%
  filter(`Año Comercial`>=2013) %>% 
  ungroup() %>%
  group_by(`Año Comercial`) %>%
  mutate(total = sum(ventas),
         prop = (ventas/total)*100) %>%
  ggplot(aes(x = `Año Comercial`,
             y = ventas, fill = reorder(rama, ventas, FUN = sum), 
             group = reorder(rama, ventas, FUN = sum))) +
  geom_area(position = "stack", color = "black")+
  scale_x_continuous(breaks = 2013L:2018L)+
  scale_fill_brewer(type = "seq", palette = "Set2", guide = guide_legend())+
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 45))+
  guides(fill = guide_legend(nrow = 7))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                               decimal.mark = ",", 
                                               scientific = FALSE))+
  labs(y = "Ventas anuales en UF",
       x = "Año Comercial",
       fill = "")+
  theme(strip.background=element_blank(),
        panel.border = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks=element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.key.size = unit(1,"cm"),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(size = 15,
                                   angle = 90, hjust = 1),
        text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position="left")+
  geom_text_repel(data=ventas_region_[[2]],
                  aes(x=`Año Comercial`, y=cum, label=labels), 
                  check_overlap = TRUE, 
                  inherit.aes = FALSE, size = 12)+
  geom_text_repel(data=ventas_region_[[1]],
                  aes(x=`Año Comercial`, y=cum, label=labels), 
                  check_overlap = TRUE, 
                  inherit.aes = FALSE, size = 12)



aaspect_ratio = 2.5
png(paste0(getwd(),"/Reportes Sectoriales 2020/20191127 Distribución Histórica de ventas.png"),width=9*aspect_ratio,height=9,
    units = "in", res = 300)
print(ventas_region_plot)
dev.off()



beepr::beep(sound = 8)


####################################################################################################
# Análisis de Evolución Covid-19 ===================================================================
####################################################################################################

covid = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo_T.csv"

data_covid = fread(covid, encoding = "UTF-8") %>% 
  reshape2::melt(id=c("Region")) 

covid_comuna = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto15/FechaInicioSintomas.csv"

data_covid_comuna_ = fread(covid_comuna, encoding = "UTF-8")

vars_ = names(data_covid_comuna_)[grep("^SE", names(data_covid_comuna_))]

data_covid_comuna = data_covid_comuna_ %>% 
  filter(Region == "Ñuble") %>% 
  reshape2::melt(id.vars = "Comuna", measure.vars = vars_)

png(paste0(path, "20201020 Nuevos casos de covid 19 en la región.png"),width=15,height=8, 
    units = "in", res = 300)
data_covid %>% 
  filter(variable != "Total") %>% 
  #mutate(value = ifelse(value ==0, NA, value)) %>% 
ggplot(aes(x = as.Date(Region), y = rollmean(value, 7, na.pad=TRUE),
           group = variable, colour = variable,
           alpha = variable, size = variable))+
  geom_line(show.legend = TRUE)+
  scale_y_continuous(trans = "log10", breaks = 10^(seq(1,4,1)))+
  scale_alpha_manual(values = c(rep(.2,6),.2,.2,.2,1, .2,rep(.2,6)))+
  scale_size_manual(values = c(rep(.8,9), 1.5, rep(.8,6)))+
#  scale_colour_manual(values = c(rep(1,6),.2,.2,.2,.2, 2,rep(1,6)))+
  scale_x_date(breaks = "1 month")+
  labs(y = "Nuevos contagios diarios (log)", x = "Fecha")+
  theme(
        strip.background=element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_line(size = 1),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.key.size = unit(1,"cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 15,
                                   angle = 60, hjust = 1),
        text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position="left")
dev.off()
  
comunas = grep(c("Chillan", "San Carlos", "Bulnes", "Itata"), levels(fct_drop(data_covid_comuna$Comuna)))

data_covid_comuna %>% 
  mutate(variable = gsub("SE", "Semana ", variable)) %>% 
  filter(variable != "Desconocido Nuble") %>% 
#  mutate(variable = )
  #mutate(value = ifelse(value ==0, NA, value)) %>% 
  ggplot(aes(x = variable, y = value,
             group = Comuna, colour = Comuna, alpha = Comuna))+
  geom_line( size = 1.5)+
  scale_y_continuous(trans = "log10", breaks = 10^(seq(1,4,1)))+
   scale_alpha_manual(values = )+
  # scale_size_manual(values = c(rep(.8,9), 1.5, rep(.8,6)))+
  # scale_colour_manual(values = c(rep(1,9), 2, rep(1,6)))+
 # scale_x_date(breaks = "1 month")+
  labs(y = "Nuevos contagios semanales", x = "Fecha")+
  theme(
    strip.background=element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_line(size = 1),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.key.size = unit(1,"cm"),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(size = 15,
                               angle = 60, hjust = 1),
    text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position="bottom")



########################################################################################################################
# Indicadore de coyuntura =======================================================
########################################################################################################################


coyuntura = read_excel(paste0(path2,"estadisticas_regionales_ñuble.xlsx"), range = "A4:CK41")

emat_ = coyuntura$`Glosa Variable`[grep("EMAT", coyuntura$`Glosa Variable`)]

months_ = names(coyuntura)[grep("^4", names(coyuntura))]

emat = coyuntura %>% 
  filter(`Glosa Variable` %in% emat_) %>%
  filter(`Glosa Variable` == "EMAT - Pernoctaciones" | `Glosa Variable` == "EMAT - Llegadas") %>% 
  reshape2::melt(id.vars ="Glosa Variable", measure.vars = months_) %>% 
  filter(!is.na(value)) %>% 
  group_by(`Glosa Variable`) %>% 
  arrange(variable) %>% 
  mutate(fecha = seq.Date(as.Date("2018-09-01"), as.Date("2020-08-01"), by = "month"))


png(paste0(path, "20201020 Encuesta Mensual de alojamiento turístico.png"),width=12,height=6, 
    units = "in", res = 300)
emat %>% 
  #mutate(value = ifelse(value ==0, NA, value)) %>% 
  ggplot(aes(x = fecha, y = value,
             group = `Glosa Variable`, colour = `Glosa Variable`))+
  geom_line(show.legend = TRUE, size = 1.5)+
  scale_x_date(breaks = "1 month")+
  labs(y = "Unidades", x = "Fecha")+
  theme(
    strip.background=element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_line(size = 1),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.key.size = unit(1,"cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 15,
                               angle = 60, hjust = 1),
    text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position="left")
dev.off()




isup = coyuntura %>% 
  filter(`Glosa Variable` == "ISUP a precios constantes") %>% 
  reshape2::melt(id.vars ="Glosa Variable", measure.vars = months_) %>% 
  filter(!is.na(value)) %>% 
  group_by(`Glosa Variable`) %>% 
  arrange(variable) %>% 
  mutate(fecha = seq.Date(as.Date("2014-01-01"), as.Date("2020-08-01"), by = "month"))


png(paste0(path, "20201020 Índice de supermercados.png"),width=12,height=6, 
    units = "in", res = 300)
isup %>% 
  #mutate(value = ifelse(value ==0, NA, value)) %>% 
  ggplot(aes(x = fecha, y = value,
             group = `Glosa Variable`, colour = `Glosa Variable`))+
  geom_line(show.legend = TRUE, size = 1.5)+
  scale_x_date(breaks = "3 month")+
  labs(y = "Índice", x = "Fecha")+
  theme(
    strip.background=element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_line(size = 1),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.key.size = unit(1,"cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10,
                               angle = 60, hjust = 1),
    text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position="bottom")
dev.off()