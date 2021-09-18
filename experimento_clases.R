# Un mercado competitivo con impuestos: 



## 
# Una estructura para el análisis de la evolución de los marcados laborales puede hacerse
# registrando un panel de indicadores que esten relacionados a través de una escala de apreciación 
## 


# este programa junto al script de Python para automatizar la descarga podría automatizar la producción de datos con la encuesta nacional de empleo 

load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}


packages = c("gmailr", "gargle", "discreteRV")

load_pkg(packages)


gm_auth_configure(path = "keys.json")


costos = c(3,8, 13, 18, 23, 28)

valores = c(45,40,35,30,25,20)

random_costs = RV(outcomes = costos, probs = 1/6)

random_values = RV(outcomes = valores, probs = 1/6)

sim_costs = rsim(random_costs, 7)
sim_values = rsim(random_values, 6)

correos_of = c("ornella.silva@alu.ucm.cl",
               "mariapazad.10@gmail.com",
               "daniela.rios.o1@alu.ucm.cl",
               "josefa1308@hotmail.com",
               "javiera.salgado.01@alu.ucm.cl",
               "matias.vergara.01@alu.ucm.cl",
               "angy.abrigo345@gmail.com")

correos_dem = c("valentina.nunez.01@alu.ucm.cl",
                "maria.vergara.03@alu.ucm.cl",
                "maria.perez.07@alu.ucm.cl",
                "constanza.herrera.02@alu.ucm.cl",
                "javiera.badilla@alu.ucm.cl",
                "javiera.aravena.03@alu.ucm.cl")



costs = sim_costs[1:7]

values = sim_values[1:6]

lapply(2:7, function(x)
  gm_mime() %>%
    gm_to(correos_of[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un oferente y tu costo de producción es de", costs[x])) %>% 
    gm_send_message()
) 


lapply(1:6, function(x)
  gm_mime() %>%
    gm_to(correos_dem[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un demandante y tu valor de compra es de", values[x])) %>% 
    gm_send_message()
) 


# link planilla de transacciones: 
#   https://docs.google.com/forms/d/e/1FAIpQLSduif-pKrWwrp0s9-1-4Twki3B8rfLVVH7Op2aTyqSQngh7_Q/viewform


## Segunda ronda 

lapply(1:13, function(x)
  gm_mime() %>%
    gm_to(correos_dem[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un oferente y tu costo de producción es de", costs[x])) %>% 
    gm_send_message()
) 


lapply(1:13, function(x)
  gm_mime() %>%
    gm_to(correos_of[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un demandante y tu valor de compra es de", values[x])) %>% 
    gm_send_message()
) 







## Tercera ronda 

lapply(1:13, function(x)
  gm_mime() %>%
    gm_to(correos_of[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un oferente y tu costo de producción es de", costs[x])) %>% 
    gm_send_message()
) 


lapply(1:13, function(x)
  gm_mime() %>%
    gm_to(correos_dem[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un demandante y tu valor de compra es de", values[x])) %>% 
    gm_send_message()
) 
