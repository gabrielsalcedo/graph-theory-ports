library(here)

here()

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)

# =============================================================
# 1. Cargar datos
# =============================================================
dataset_graphs <- read_excel(here("data","raw","dataset_graphs.xlsx"))
p_1 <- read_excel(here("data","raw","puertos_1.xlsx"))
p_2 <- read_excel(here("data","raw","puertos_2.xlsx"))

complete_ports <- bind_rows(p_1,p_2)%>%
  distinct()

complete_ports <- complete_ports%>%
  select(-Arrival_Time,
         -Departure_Time,
         -Maps_Der,
         -Eta_For_Next_Destination_Change,
         -Vessel_Name,
         -Vessel_Imo)

puertos <- complete_ports%>%
  rename(exp_imp = Berth_Type,
         puerto_partida = Berth_Name,
         puerto_llegada = Vessel_Destination_On_Departure,
         pais_partida = Port_Country,
         tipo_embarcacion = Vessel_Class,
         capacidad_embarcacion = Vessel_Capacity,
         cantidad_est = Estimated_Quantity)

#eliminar 18 filas con exp_imp == imports/exports, no es claro la dirección

puertos <- puertos %>%
  filter(exp_imp != 'Imports/Exports')

#filtrar por exp_imp para saber q paises importan y cuales exportan
#esto se hace para construir la base de datos de la WITS (aranceles)

imp <- puertos %>%
  filter(exp_imp=='Imports')%>%
  select(pais_partida) %>%
  distinct()

exp <- puertos %>%
  filter(exp_imp=='Exports')%>%
  distinct(pais_partida)%>%
  pull(pais_partida)

#descargamos datos arancelarios
tariffs <- read.csv(here("data", "raw","tariffs.csv"))
tariffs <- tariffs%>%
  select(-Reporter,
         -Product,
         -Partner)
paises_lista <- split(complete_ports, complete_ports$pais)

# Revisar uno a la vez, cambiando el nombre:
View(paises_lista[["Algeria"]])
View(paises_lista[["India"]])
View(paises_lista[["United States of America"]])

































































# =============================================================
# 2. Resolver destinos con formato de ruta desviada (ej. "US CRP>US PAU")
#    El destino real es lo que está DESPUÉS de la última flecha ">"
# =============================================================

# 2a. Eliminar filas donde el formato de flecha está roto (nada antes/después)
dataset_graphs <- dataset_graphs %>%
  filter(
    !str_detect(Vessel_Destination_On_Departure, ">\\s*$"),   # termina en flecha sin nada después
    !str_detect(Vessel_Destination_On_Departure, "^\\s*>")    # empieza con flecha sin nada antes
  )

# 2b. Extraer el destino real: todo lo que está después de la última ">"
dataset_graphs <- dataset_graphs %>%
  mutate(
    Vessel_Destination_On_Departure = if_else(
      str_detect(Vessel_Destination_On_Departure, ">"),
      str_trim(str_extract(Vessel_Destination_On_Departure, "[^>]+$")),
      Vessel_Destination_On_Departure
    )
  )

# =============================================================
# 3. Traducir las 2 excepciones de código LOCODE que sí importan
#    (el resto de códigos LOCODE se van a eliminar más abajo)
# =============================================================
locode_a_nombre <- c(
  "AE FJR" = "Fujairah",
  "US BPT" = "Houston Bayport"
)

dataset_graphs <- dataset_graphs %>%
  mutate(
    Vessel_Destination_On_Departure = recode(
      str_trim(Vessel_Destination_On_Departure),
      !!!locode_a_nombre,
      .default = str_trim(Vessel_Destination_On_Departure)
    )
  )

# =============================================================
# 4. Eliminar "For Orders" (destino desconocido al zarpar)
# =============================================================
dataset_graphs <- dataset_graphs %>%
  filter(
    !str_detect(str_to_lower(Berth_Name), "for orders"),
    !str_detect(str_to_lower(Vessel_Destination_On_Departure), "for orders")
  )

# =============================================================
# 5. Eliminar el resto de destinos que quedaron en formato LOCODE
#    (2 letras + espacio + 3 letras, ej. "US PAU")
# =============================================================
patron_locode <- "^[A-Z]{2}\\s[A-Z]{3}$"

dataset_graphs <- dataset_graphs %>%
  filter(!str_detect(str_trim(Vessel_Destination_On_Departure), patron_locode))

# =============================================================
# 6. Normalizar nombres de puerto (quitar sufijos de muelle, paréntesis, etc.)
# =============================================================
normalizar_puerto <- function(nombre) {
  nombre %>%
    str_to_lower() %>%
    str_replace_all("_", " ") %>%
    str_remove_all("\\([^)]*\\)") %>%
    str_remove("\\s+\\d+(-\\d+)?\\s*$") %>%
    str_remove("\\s+[a-z]\\s*$") %>%
    str_squish() %>%
    str_to_title()
}

correcciones_manuales <- c(
  "Cpc Ceyhan" = "Ceyhan",
  "Novorossiysk Urals" = "Novorossiysk",
  "Forcados Oil Terminal" = "Forcados",
  "Salina" = "La Salina",
  "Houston Enterprise" = "Houston",
  "Acu Superport" = "Acu",
  "Primorsk Crude" = "Primorsk",
  "Chiriqui Grande Terminal" = "Chiriqui Grande",
  "Basra Oil Terminal Spm" = "Basra",
  "Vadinar Terminal" = "Vadinar",
  "Djeno Offshore Terminal" = "Djeno",
  "Kozmino" = "Kozmino Rus"
)

dataset_graphs <- dataset_graphs %>%
  mutate(
    Berth_Name_clean = normalizar_puerto(Berth_Name),
    Vessel_Destination_clean = normalizar_puerto(Vessel_Destination_On_Departure),
    Berth_Name_clean = recode(Berth_Name_clean, !!!correcciones_manuales),
    Vessel_Destination_clean = recode(Vessel_Destination_clean, !!!correcciones_manuales)
  )

# =============================================================
# 7. Eliminar self-loops (origen y destino son el mismo puerto)
# =============================================================
dataset_graphs <- dataset_graphs %>%
  filter(Berth_Name_clean != Vessel_Destination_clean)

# =============================================================
# 8. Selección de puertos por volumen (Pareto al 50%)
# =============================================================
# ports <- dataset_graphs %>%
#   group_by(Berth_Name_clean) %>%
#   summarise(
#     n_viajes = n(),
#     vol_total = sum(Estimated_Quantity, na.rm = TRUE)
#   ) %>%
#   arrange(desc(vol_total))
# 
# # Distribución de volúmenes (visual)
# ggplot(ports, aes(x = vol_total)) +
#   geom_histogram(binwidth = 100000, fill = "steelblue", color = "black") +
#   labs(title = "Distribution of Port Volumes", x = "Volume", y = "Frequency") +
#   theme_minimal()
# 
# # Pareto
# ports <- ports %>%
#   mutate(
#     pct_vol = vol_total / sum(vol_total),
#     pct_acum = cumsum(pct_vol)
#   )
# 
# n_top <- sum(ports$pct_acum <= 0.50)
# n_top
# 
# top_ports <- ports %>%
#   arrange(desc(vol_total)) %>%
#   slice_head(n = n_top)
# 
# # =============================================================
# # 9. Dataset final: solo viajes entre los puertos top (por volumen)
# # =============================================================
# df_gr <- inner_join(dataset_graphs, top_ports, by = "Berth_Name_clean")
#write.csv(df_gr, here("data","final","df_grafo.csv"), row.names = FALSE)
