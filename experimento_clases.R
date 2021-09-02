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

sim_costs = rsim(random_costs, 13)
sim_values = rsim(random_values, 13)

correos_of = c("katalina.fuenzalida@alu.ucm.cl",
"ivana.ruminot@alu.ucm.cl",
"matias.pinto@alu.ucm.cl",
"mauricio.mardones@alu.ucm.cl",
"matiasjaraespinoza@gmail.com",
"catalina.leal@alu.ucm.cl", 
"paula.maturana@alu.ucm.cl",
"Alexiarojasc@gmail.com",
"carla.donoso@alu.ucm.cl",
"pamelaestervs@gmail.com",
"maureiracristian31@gmail.com",
"arlette.val31@gmail.com",
"bianca.valenzuela@alu.ucm.cl")

correos_dem = c("antonia.zambrano@alu.ucm.cl",
                "marjorie.letelier@alu.ucm.cl",
                "Tamara.rios@alu.ucm.cl",
                "sofia.hormazabal.01@alu.ucm.cl",
                "darrin.alegria@alu.ucm.cl",
                "francisca.garrido.04@alu.ucm.cl",
                "sebastian.ortiz.01@alu.ucm.cl",
                "angela.cursach@alu.ucm.cl", 
                "sebastian.valderrama@alu.ucm.cl",
                "javiera.tobar.01@alu.ucm.cl",
                "carla.donoso@alu.ucm.cl",
                "Josefa.skaida@alu.ucm.cl",
                "valentina.rodriguez.01@alu.ucm.cl")

correos_dem = c()

costs = sim_costs[1:13]

values = sim_values[1:13]

lapply(1:13, function(x)
  gm_mime() %>%
    gm_to(correos_of[x]) %>%
    gm_from("hgarrido@ubiobio.cl") %>%
    gm_subject("Tus datos secretos para ejercicio en clases - Desarrollo economico") %>%
    gm_text_body(paste0("Hola, : 
                        En esta sesión eres un oferente y tu costo de producción es de", costs[x])) %>% 
    gm_send_message()
) 


lapply(1:2, function(x)
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
