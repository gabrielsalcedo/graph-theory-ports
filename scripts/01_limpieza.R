library(here)

here()

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# =============================================================
# 1. Cargar datos
# =============================================================
dataset_graphs <- read_csv(here("data","raw","comercio_puertos.csv"))
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
ports <- dataset_graphs %>%
  group_by(Berth_Name_clean) %>%
  summarise(
    n_viajes = n(),
    vol_total = sum(Estimated_Quantity, na.rm = TRUE)
  ) %>%
  arrange(desc(vol_total))

# Distribución de volúmenes (visual)
ggplot(ports, aes(x = vol_total)) +
  geom_histogram(binwidth = 100000, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Port Volumes", x = "Volume", y = "Frequency") +
  theme_minimal()

# Pareto
ports <- ports %>%
  mutate(
    pct_vol = vol_total / sum(vol_total),
    pct_acum = cumsum(pct_vol)
  )

n_top <- sum(ports$pct_acum <= 0.50)
n_top

top_ports <- ports %>%
  arrange(desc(vol_total)) %>%
  slice_head(n = n_top)

# =============================================================
# 9. Dataset final: solo viajes entre los puertos top (por volumen)
# =============================================================
df_gr <- inner_join(dataset_graphs, top_ports, by = "Berth_Name_clean")
write.csv(df_gr, "df_grafo.csv", row.names = FALSE)
