library(readr)
df <- read_csv("data/tennis_dataset_final.csv")
summary(df)
colnames(df)
library(data.table)
setDT(df)
# Entry como factor, imputando vacíos a "Direct"

df$player_A_entry <- as.factor(df$player_A_entry)
df$player_B_entry <- as.factor(df$player_B_entry)


## Si querés dummy para has_seed
#df[, player_A_has_seed := as.integer(player_A_has_seed)]
#df[, player_B_has_seed := as.integer(player_B_has_seed)]

#crear diferencia de ranking
df[, diff_rank := player_A_rank - player_B_rank]


# Si querés año como factor
#df[, year := as.factor(year)]
#como numero
#df[, year := as.numeric(as.character(year))]

# Revisar valores únicos, frecuencias
unique(df$player_A_entry)
table(df$player_A_entry)
summary(df)
colnames(df)

library(data.table)
library(zoo)
setDT(df)

# Promedio de aces últimos 5 partidos para player A
setorder(df, player_A, tourney_date)
df[, player_A_ace_hist10 := shift(rollapply(player_A_ace, 10, mean, fill = NA, align = "right"), 1), by = player_A]

# Para player B
setorder(df, player_B, tourney_date)
df[, player_B_ace_hist10 := shift(rollapply(player_B_ace, 10, mean, fill = NA, align = "right"), 1), by = player_B]
media_aces_A <- mean(df$player_A_ace, na.rm = TRUE)
df[is.na(player_A_ace_hist10), player_A_ace_hist10 := media_aces_A]

media_aces_B <- mean(df$player_B_ace, na.rm = TRUE)
df[is.na(player_B_ace_hist10), player_B_ace_hist10 := media_aces_B]

factor_vars <- c(
  "surface", "tourney_level", "round", "year", "best_of",
  "player_A_hand", "player_B_hand",
  "player_A_ioc", "player_B_ioc",
  "player_A_entry", "player_B_entry",
  "player_A_has_seed", "player_B_has_seed"
)

df[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]


library(data.table)
library(zoo)
setDT(df)  # Asegurate de usar tu objeto df

# Lista de variables a transformar
stats <- c("svpt", "1stIn", "1stWon", "2ndWon", "SvGms", "bpFaced", "bpSaved","df")

# Para cada estadística y jugador
for (stat in stats) {
  # Nombre de la columna para A y B
  var_A <- paste0("player_A_", stat)
  var_B <- paste0("player_B_", stat)
  hist_A <- paste0(var_A, "_hist10")
  hist_B <- paste0(var_B, "_hist10")
  
  # Player A
  setorder(df, player_A, tourney_date)
  df[, (hist_A) := shift(rollapply(get(var_A), 10, mean, fill=NA, align="right"), 1), by=player_A]
  
  # Player B
  setorder(df, player_B, tourney_date)
  df[, (hist_B) := shift(rollapply(get(var_B), 10, mean, fill=NA, align="right"), 1), by=player_B]
}

# Opcional: imputar medias globales para los NA iniciales
for (stat in stats) {
  for (who in c("A", "B")) {
    hist_var <- paste0("player_", who, "_", stat, "_hist10")
    orig_var <- paste0("player_", who, "_", stat)
    media_global <- mean(df[[orig_var]], na.rm = TRUE)
    df[is.na(get(hist_var)), (hist_var) := media_global]
  }
}

#STREAK
# 1. Armar tabla larga de participaciones
library(data.table)
dtA <- df[, .(jugador = player_A, rival = player_B, fecha = tourney_date, torneo = tourney_id, match_num, gano = gano_A)]
dtB <- df[, .(jugador = player_B, rival = player_A, fecha = tourney_date, torneo = tourney_id, match_num, gano = 1 - gano_A)]
jugador_partidos <- rbind(dtA, dtB)

# 2. Crear una columna "orden" por jugador, ordenando bien
setorder(jugador_partidos, jugador, fecha, torneo, match_num)
jugador_partidos[, orden := 1:.N, by = jugador]

# 3. Ahora el (jugador, orden) es único para cada partido
# Calcula la racha usando el orden

jugador_partidos[, win_streak := {
  streak <- integer(.N)
  run <- 0
  for (i in 1:.N) {
    streak[i] <- run
    if (gano[i] == 1) {
      run <- run + 1
    } else {
      run <- 0
    }
  }
  streak
}, by = jugador]

# Merge para player_A
df <- merge(
  df, 
  jugador_partidos[, .(jugador, fecha, torneo, match_num, win_streak)], 
  by.x = c("player_A", "tourney_date", "tourney_id", "match_num"),
  by.y = c("jugador", "fecha", "torneo", "match_num"),
  all.x = TRUE
)
setnames(df, "win_streak", "player_A_win_streak")

# Merge para player_B
df <- merge(
  df, 
  jugador_partidos[, .(jugador, fecha, torneo, match_num, win_streak)], 
  by.x = c("player_B", "tourney_date", "tourney_id", "match_num"),
  by.y = c("jugador", "fecha", "torneo", "match_num"),
  all.x = TRUE
)
setnames(df, "win_streak", "player_B_win_streak")

df[, diff_rank_pts := player_A_rank_pts - player_B_rank_pts]


#MINUTOS JUGADOS
library(data.table)
# Asegúrate de tener el data.table
setDT(df)
# Creamos tabla larga: cada fila es (jugador, torneo, match)
dtA <- df[, .(jugador = player_A, tourney_id, tourney_date, match_num, minutos = minutes)]
dtB <- df[, .(jugador = player_B, tourney_id, tourney_date, match_num, minutos = minutes)]
participaciones <- rbind(dtA, dtB)

# Ordenamos por jugador, torneo y el orden de los partidos
setorder(participaciones, jugador, tourney_id, tourney_date, match_num)

# Sumamos minutos acumulados PREVIOS al partido actual
participaciones[, minutos := ifelse(is.na(minutos), 0, minutos)]
participaciones[, minutos_previos := shift(cumsum(minutos), 1, fill = 0), 
                by = .(jugador, tourney_id)]

# Para player_A
df <- merge(
  df, 
  participaciones[, .(jugador, tourney_id, tourney_date, match_num, player_A_min_prev_torneo = minutos_previos)], 
  by.x = c("player_A", "tourney_id", "tourney_date", "match_num"),
  by.y = c("jugador", "tourney_id", "tourney_date", "match_num"),
  all.x = TRUE
)

# Para player_B
df <- merge(
  df, 
  participaciones[, .(jugador, tourney_id, tourney_date, match_num, player_B_min_prev_torneo = minutos_previos)], 
  by.x = c("player_B", "tourney_id", "tourney_date", "match_num"),
  by.y = c("jugador", "tourney_id", "tourney_date", "match_num"),
  all.x = TRUE
)
names(df)
df[, diff_min_prev_torneo := player_A_min_prev_torneo - player_B_min_prev_torneo]

df[, diff_age := player_A_age - player_B_age]
df[, diff_ht := player_A_ht - player_B_ht]



#HISTORIAL
library(data.table)

setDT(df)

# Primero, ordená los partidos para asegurar secuencia temporal:
setorder(df, player_A, player_B, tourney_date, match_num)

# Creamos una tabla auxiliar para todas las combinaciones de jugadores (A vs B y B vs A)
df[, pair_id := ifelse(player_A < player_B,
                       paste(player_A, player_B, sep = " vs "),
                       paste(player_B, player_A, sep = " vs "))]

df[, A_is_first := player_A < player_B]

# Creamos vector de resultados siempre respecto al orden alfabético
df[, win_for_first := fifelse(A_is_first & gano_A == 1, 1,
                              fifelse(!A_is_first & gano_A == 0, 1, 0))]

# Para cada par de jugadores, calculemos el acumulado previo:
df[, h2h_diff := shift(cumsum(ifelse(A_is_first, gano_A, 1 - gano_A)) - 
                         cumsum(ifelse(!A_is_first, gano_A, 1 - gano_A)), 
                       n = 1, fill = 0),
   by = .(pair_id)]

# Para que sea POSITIVO si favorece a A, NEGATIVO si favorece a B:
# (Ya está correcto porque si A es primero, cuenta sus victorias; si B es primero, cuenta sus derrotas)

# Si querés, podés limpiar columnas auxiliares:
df[, c("pair_id", "A_is_first", "win_for_first") := NULL]

# Resultado: df$h2h_diff, POSITIVO favorece A, NEGATIVO favorece B, 0 = parejos o primer enfrentamiento
names(df)

# Primero asegurate de tener las dummies:
df[, player_A_top10 := as.integer(player_A_rank <= 10)]
df[, player_B_top10 := as.integer(player_B_rank <= 10)]

# Ahora creá la variable de matchup:
df[, top10_matchup := fifelse(player_A_top10 == 1 & player_B_top10 == 0, "A",
                              fifelse(player_A_top10 == 0 & player_B_top10 == 1, "B", "0"))]

df[, top10_matchup := factor(top10_matchup, levels = c("0", "A", "B"))]

cols_leak <- c(
  "player_A_ace", "player_B_ace",
  "player_A_df", "player_B_df",
  "player_A_svpt", "player_B_svpt",
  "player_A_1stIn", "player_B_1stIn",
  "player_A_1stWon", "player_B_1stWon",
  "player_A_2ndWon", "player_B_2ndWon",
  "player_A_SvGms", "player_B_SvGms",
  "player_A_bpSaved", "player_B_bpSaved",
  "player_A_bpFaced", "player_B_bpFaced",
  "minutes"
)

df[, (cols_leak) := NULL]

write_csv(df, "data/df_transform.csv")


names(df)
