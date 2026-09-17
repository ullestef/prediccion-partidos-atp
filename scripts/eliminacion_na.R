library(readr)
library(dplyr)
library(purrr)

# Ruta a tus archivos
ruta <- "data/atp_matches_raw"

# Listar todos los archivos CSV de partidos
archivos <- list.files(path = ruta, pattern = "atp_matches_\\d{4}\\.csv", full.names = TRUE)

# Leer una fila de cada archivo para comparar columnas
nombres_columnas <- map(archivos, ~ {
  df <- read_csv(.x, n_max = 1, show_col_types = FALSE)
  colnames(df)
})

# Ver cuántos conjuntos de nombres distintos hay
length(unique(nombres_columnas))

# Si hay diferencias, mostrarlas
unique(nombres_columnas)

# Combinar todos los archivos en un solo dataframe
df_todos <- map_dfr(archivos, ~ read_csv(.x, show_col_types = FALSE))

# Guardar como CSV unificado (opcional)
# write_csv(df_final, "data/tennis_dataset_final.csv")


library(dplyr)

set.seed(123)

df_final <- df_todos %>%
  filter(!is.na(winner_name), !is.na(loser_name)) %>%
  rowwise() %>%
  mutate(
    flip = sample(c(TRUE, FALSE), 1),
    
    # Identidad básica
    player_A = ifelse(flip, winner_name, loser_name),
    player_B = ifelse(flip, loser_name, winner_name),
    gano_A   = ifelse(flip, 1, 0),
    
    # Info general
    player_A_hand = ifelse(flip, winner_hand, loser_hand),
    player_B_hand = ifelse(flip, loser_hand, winner_hand),
    
    player_A_age = ifelse(flip, winner_age, loser_age),
    player_B_age = ifelse(flip, loser_age, winner_age),
    
    player_A_ht = ifelse(flip, winner_ht, loser_ht),
    player_B_ht = ifelse(flip, loser_ht, winner_ht),
    
    player_A_ioc = ifelse(flip, winner_ioc, loser_ioc),
    player_B_ioc = ifelse(flip, loser_ioc, winner_ioc),
    
    player_A_entry = ifelse(flip, winner_entry, loser_entry),
    player_B_entry = ifelse(flip, loser_entry, winner_entry),
    
    player_A_seed = ifelse(flip, winner_seed, loser_seed),
    player_B_seed = ifelse(flip, loser_seed, winner_seed),
    
    player_A_rank = ifelse(flip, winner_rank, loser_rank),
    player_B_rank = ifelse(flip, loser_rank, winner_rank),
    
    player_A_rank_pts = ifelse(flip, winner_rank_points, loser_rank_points),
    player_B_rank_pts = ifelse(flip, loser_rank_points, winner_rank_points),
    
    # Estadísticas del partido
    player_A_ace = ifelse(flip, w_ace, l_ace),
    player_B_ace = ifelse(flip, l_ace, w_ace),
    
    player_A_df = ifelse(flip, w_df, l_df),
    player_B_df = ifelse(flip, l_df, w_df),
    
    player_A_svpt = ifelse(flip, w_svpt, l_svpt),
    player_B_svpt = ifelse(flip, l_svpt, w_svpt),
    
    player_A_1stIn = ifelse(flip, w_1stIn, l_1stIn),
    player_B_1stIn = ifelse(flip, l_1stIn, w_1stIn),
    
    player_A_1stWon = ifelse(flip, w_1stWon, l_1stWon),
    player_B_1stWon = ifelse(flip, l_1stWon, w_1stWon),
    
    player_A_2ndWon = ifelse(flip, w_2ndWon, l_2ndWon),
    player_B_2ndWon = ifelse(flip, l_2ndWon, w_2ndWon),
    
    player_A_SvGms = ifelse(flip, w_SvGms, l_SvGms),
    player_B_SvGms = ifelse(flip, l_SvGms, w_SvGms),
    
    player_A_bpSaved = ifelse(flip, w_bpSaved, l_bpSaved),
    player_B_bpSaved = ifelse(flip, l_bpSaved, w_bpSaved),
    
    player_A_bpFaced = ifelse(flip, w_bpFaced, l_bpFaced),
    player_B_bpFaced = ifelse(flip, l_bpFaced, w_bpFaced),
    
    # Variables contextuales
    surface = surface,
    round = round,
    tourney_level = tourney_level,
    year = as.integer(substr(tourney_date, 1, 4)),
    best_of = best_of,
    minutes = minutes
  ) %>%
  filter(!is.na(player_A_ace)) %>%  # 🔴 Elimina filas con NA en player_A_ace
  ungroup() %>%
  select(
    tourney_id, tourney_name, tourney_date, match_num,
    player_A, player_B, gano_A,
    player_A_hand, player_B_hand,
    player_A_age, player_B_age,
    player_A_ht, player_B_ht,
    player_A_ioc, player_B_ioc,
    player_A_entry, player_B_entry,
    player_A_seed, player_B_seed,
    player_A_rank, player_B_rank,
    player_A_rank_pts, player_B_rank_pts,
    player_A_ace, player_B_ace,
    player_A_df, player_B_df,
    player_A_svpt, player_B_svpt,
    player_A_1stIn, player_B_1stIn,
    player_A_1stWon, player_B_1stWon,
    player_A_2ndWon, player_B_2ndWon,
    player_A_SvGms, player_B_SvGms,
    player_A_bpSaved, player_B_bpSaved,
    player_A_bpFaced, player_B_bpFaced,
    surface, round, tourney_level, year, best_of, minutes
  )

#write_csv(df_final, "C:/Users/agusg/Documents/Proyecto CDD/tennis_dataset_final.csv")
colnames(df_final)
head(df_final)
nrow(df_final)
sample(df_final,10)
summary(df_final)
n_filas_con_na <- sum(!complete.cases(df_final))
print(n_filas_con_na)


library(dplyr)

df_final <- df_final %>%
  group_by(best_of) %>%
  mutate(minutes = ifelse(is.na(minutes), mean(minutes, na.rm = TRUE), minutes)) %>%
  ungroup()
summary(df_final)

library(dplyr)
library(lubridate)
library(purrr)

# Asegurarse de que la fecha esté en formato Date
df_final <- df_final %>%
  mutate(tourney_date = ymd(tourney_date))

### --- IMPUTAR player_B_rank_pts ---
df_na_B <- df_final %>% filter(is.na(player_B_rank_pts))
df_no_na_B <- df_final %>% filter(!is.na(player_B_rank_pts))

imputar_rank_B <- function(jugador, fecha) {
  historial <- df_no_na_B %>% filter(player_B == jugador)
  if (nrow(historial) == 0) return(NA)
  historial <- historial %>%
    mutate(dif_dias = abs(as.numeric(fecha - tourney_date))) %>%
    arrange(dif_dias)
  return(historial$player_B_rank_pts[1])
}

df_na_B <- df_na_B %>%
  mutate(player_B_rank_pts = map2_dbl(player_B, tourney_date, imputar_rank_B))

### --- IMPUTAR player_A_rank_pts ---
df_na_A <- df_final %>% filter(is.na(player_A_rank_pts))
df_no_na_A <- df_final %>% filter(!is.na(player_A_rank_pts))

imputar_rank_A <- function(jugador, fecha) {
  historial <- df_no_na_A %>% filter(player_A == jugador)
  if (nrow(historial) == 0) return(NA)
  historial <- historial %>%
    mutate(dif_dias = abs(as.numeric(fecha - tourney_date))) %>%
    arrange(dif_dias)
  return(historial$player_A_rank_pts[1])
}

df_na_A <- df_na_A %>%
  mutate(player_A_rank_pts = map2_dbl(player_A, tourney_date, imputar_rank_A))

### --- REEMPLAZAR filas originales por las imputadas ---
df_restante <- df_final %>%
  filter(!is.na(player_A_rank_pts), !is.na(player_B_rank_pts))

df_final <- bind_rows(df_restante, df_na_A, df_na_B) %>%
  arrange(tourney_date)
df_final <- df_final %>%
  filter(!is.na(player_A_rank_pts), !is.na(player_B_rank_pts))

###INPUT RANK A,B
# Crear tabla auxiliar con valores no NA
# Asegurate de tener data.table cargado y el dataframe como data.table
library(data.table)
setDT(df_final)

# Tabla auxiliar sin NA
rankA_non_na <- df_final[!is.na(player_A_rank), .(tourney_date, player_A, player_A_rank)]
setorder(rankA_non_na, player_A, tourney_date)

# Imputar con valor más cercano
library(data.table)
library(lubridate)

setDT(df_final)
df_final[, tourney_date := ymd(as.character(tourney_date))]

# Guardar valores no NA
rankA_non_na <- df_final[!is.na(player_A_rank), .(player_A, tourney_date, player_A_rank)]
setorder(rankA_non_na, player_A, tourney_date)
# Filas que queremos imputar
rows_na <- which(is.na(df_final$player_A_rank))
for (i in rows_na) {
  jugador <- df_final$player_A[i]
  fecha <- df_final$tourney_date[i]
  
  # Buscar historial del jugador
  historial <- rankA_non_na[player_A == jugador]
  
  if (nrow(historial) > 0) {
    # Buscar la fecha más cercana
    idx <- which.min(abs(historial$tourney_date - fecha))
    df_final$player_A_rank[i] <- historial$player_A_rank[idx]
  }
}
# Tabla auxiliar para player_B
rankB_non_na <- df_final[!is.na(player_B_rank), .(player_B, tourney_date, player_B_rank)]
setorder(rankB_non_na, player_B, tourney_date)

# Filas con NA
rows_na_b <- which(is.na(df_final$player_B_rank))

# Imputación
for (i in rows_na_b) {
  jugador <- df_final$player_B[i]
  fecha <- df_final$tourney_date[i]
  
  historial <- rankB_non_na[player_B == jugador]
  
  if (nrow(historial) > 0) {
    idx <- which.min(abs(historial$tourney_date - fecha))
    df_final$player_B_rank[i] <- historial$player_B_rank[idx]
  }
}
sum(is.na(df_final$player_A_rank))  # Debería ser 0 o muy bajo
sum(is.na(df_final$player_B_rank))
summary(df_final)

library(ggplot2)

# Filtrar los datos: casos donde player_A_seed es NA
df_rank_sin_seed <- df_final[is.na(player_A_seed)]

# Histograma de player_A_rank
ggplot(df_rank_sin_seed, aes(x = player_A_rank)) +
  geom_histogram(binwidth = 10, fill = "#69b3a2", color = "black") +
  labs(title = "Distribución de player_A_rank donde player_A_seed es NA",
       x = "Ranking de Player A", y = "Cantidad de jugadores") +
  theme_minimal()

# Para Player A
df_final[, player_A_has_seed := !is.na(player_A_seed) & player_A_seed > 0]

# Para Player B
df_final[, player_B_has_seed := !is.na(player_B_seed) & player_B_seed > 0]

df_final[is.na(player_A_seed), player_A_seed := 0]
df_final[is.na(player_B_seed), player_B_seed := 0]

summary(df_final)
sum(!complete.cases(df_final))
colSums(is.na(df_final))
df_final$player_A_entry[is.na(df_final$player_A_entry) | df_final$player_A_entry == ""] <- "Direct"
df_final$player_B_entry[is.na(df_final$player_B_entry) | df_final$player_B_entry == ""] <- "Direct"

df_final$player_A_entry <- as.factor(df_final$player_A_entry)
df_final$player_B_entry <- as.factor(df_final$player_B_entry)


library(data.table)
setDT(df_final)

media_altura <- mean(df_final$player_A_ht, na.rm = TRUE)
df_final[is.na(player_A_ht), player_A_ht := media_altura]
# Idem para player_B_ht
media_altura_B <- mean(df_final$player_B_ht, na.rm = TRUE)
df_final[is.na(player_B_ht), player_B_ht := media_altura_B]

colSums(is.na(df_final))

df_final <- na.omit(df_final)
nrow(df_final)
summary(df_final)
write_csv(df_final, "data/tennis_dataset_final.csv")
