#!/usr/bin/env Rscript
#
# Valida los datos de UBIGEO: integridad referencial, formato de los códigos y
# consistencia entre los tres formatos publicados (CSV, JSON y RDS).
#
#   Rscript validar-datos.R
#
# Sale con código 1 si encuentra errores. Los avisos describen problemas
# conocidos de las fuentes originales, o cosas que se arreglan regenerando
# archivos, y no hacen fallar la validación.
#
# Solo depende de R base y de jsonlite (que ya se usa en generar-json.R).

suppressPackageStartupMessages(library(jsonlite))

errores <- character()
avisos <- character()

err <- function(...) errores <<- c(errores, paste0(...))
avi <- function(...) avisos <<- c(avisos, paste0(...))

seccion <- function(titulo) cat("\n", titulo, "\n", sep = "")
paso <- function(bien, texto) cat(if (bien) "  [ok]   " else "  [ERR]  ", texto, "\n", sep = "")

# `NA` en estos archivos es el texto "NA", no un valor ausente de R: los leemos
# como carácter para comparar contra el CSV tal cual está escrito.
leer_csv <- function(base) {
  read.csv(paste0(base, ".csv"),
           colClasses = "character", na.strings = character(0),
           check.names = FALSE, encoding = "UTF-8")
}

vacio <- function(x) is.na(x) | x == "" | x == "NA"

# Dos valores de `capital` traen un salto de línea incrustado, que el .rds
# guarda como CRLF y read.csv normaliza a LF. Es la misma cadena.
sin_crlf <- function(x) gsub("\r\n", "\n", x, fixed = TRUE)

# Nombres que `ubigeo_ccpp.json` trae en inglés. El .json publicado se generó
# antes de que estos campos se renombraran en el .rds, así que quedó desfasado;
# se aceptan para poder comparar el resto de los valores, con aviso.
LEGADO_JSON <- c(inei_distrito = "inei_district", tipo = "type")

NIVELES <- c("ubigeo_departamento", "ubigeo_provincia", "ubigeo_distrito", "ubigeo_ccpp")
csv <- lapply(NIVELES, leer_csv)
names(csv) <- NIVELES

dep <- csv$ubigeo_departamento
prov <- csv$ubigeo_provincia
dist <- csv$ubigeo_distrito
ccpp <- csv$ubigeo_ccpp

# Empareja cada columna del CSV con su equivalente en el otro formato,
# aceptando los nombres heredados. NA si la columna no está.
emparejar <- function(esperados, reales) {
  mapa <- ifelse(esperados %in% reales, esperados, NA_character_)
  falta <- is.na(mapa)
  if (any(falta)) {
    alias <- unname(LEGADO_JSON[esperados[falta]])
    mapa[falta] <- ifelse(!is.na(alias) & alias %in% reales, alias, NA_character_)
  }
  mapa
}

# ---------------------------------------------------------------------------
seccion("1. El README documenta los campos que existen")
# ---------------------------------------------------------------------------
# Detecta la deriva entre la documentación y los datos: campos renombrados,
# agregados o quitados sin actualizar las tablas del README.

readme <- readLines("README.md", encoding = "UTF-8", warn = FALSE)
cortes <- grep("^### ", readme)
documentado <- list()
for (i in seq_along(cortes)) {
  ini <- cortes[i]
  fin <- if (i < length(cortes)) cortes[i + 1] - 1 else length(readme)
  archivo <- regmatches(readme[ini], regexpr("ubigeo_[a-z]+(?=\\.csv`)", readme[ini], perl = TRUE))
  if (!length(archivo)) next
  filas <- grep("^\\| `[a-z0-9_]+`\\s*\\|", readme[ini:fin], value = TRUE)
  documentado[[archivo]] <- gsub("^\\| `([a-z0-9_]+)`.*$", "\\1", filas)
}

for (base in NIVELES) {
  reales <- names(csv[[base]])
  docs <- documentado[[base]]
  bien <- identical(docs, reales)
  paso(bien, sprintf("%-22s %d campos documentados == cabecera del CSV", base, length(reales)))
  if (!bien) {
    err(sprintf("%s: el README no coincide con el CSV.\n      README: %s\n      CSV   : %s",
                base, paste(docs, collapse = ", "), paste(reales, collapse = ", ")))
  }
}

# ---------------------------------------------------------------------------
seccion("2. Formato y unicidad de los códigos UBIGEO")
# ---------------------------------------------------------------------------

# `unico` solo para las claves primarias: en ubigeo_ccpp, inei_distrito es una
# clave foránea y se repite una vez por cada centro poblado del distrito.
revisar_codigo <- function(base, columna, digitos, sufijo = NULL, unico = TRUE) {
  v <- csv[[base]][[columna]]
  presentes <- v[!vacio(v)]

  mal <- presentes[!grepl(sprintf("^[0-9]{%d}$", digitos), presentes)]
  paso(length(mal) == 0, sprintf("%-22s %-14s son %d dígitos (%d)", base, columna, digitos, length(presentes)))
  if (length(mal)) {
    err(sprintf("%s$%s: %d código(s) no son %d dígitos: %s",
                base, columna, length(mal), digitos, paste(head(mal, 5), collapse = ", ")))
  }

  if (unico) {
    dups <- unique(presentes[duplicated(presentes)])
    paso(length(dups) == 0, sprintf("%-22s %-14s sin duplicados", base, columna))
    if (length(dups)) {
      err(sprintf("%s$%s: %d código(s) duplicados: %s",
                  base, columna, length(dups), paste(head(dups, 5), collapse = ", ")))
    }
  }

  if (!is.null(sufijo)) {
    mal <- presentes[!endsWith(presentes, sufijo)]
    paso(length(mal) == 0, sprintf("%-22s %-14s termina en '%s'", base, columna, sufijo))
    if (length(mal)) {
      err(sprintf("%s$%s: %d código(s) no terminan en '%s': %s",
                  base, columna, length(mal), sufijo, paste(head(mal, 5), collapse = ", ")))
    }
  }
}

revisar_codigo("ubigeo_departamento", "inei", 6, "0000")
revisar_codigo("ubigeo_departamento", "reniec", 6, "0000")
revisar_codigo("ubigeo_provincia", "inei", 6, "00")
revisar_codigo("ubigeo_provincia", "reniec", 6, "00")
revisar_codigo("ubigeo_distrito", "inei", 6)
revisar_codigo("ubigeo_distrito", "reniec", 6)
revisar_codigo("ubigeo_ccpp", "inei_ccpp", 10)
revisar_codigo("ubigeo_ccpp", "inei_distrito", 6, unico = FALSE)

# ---------------------------------------------------------------------------
seccion("3. Integridad referencial entre niveles")
# ---------------------------------------------------------------------------
# El UBIGEO de INEI es jerárquico: DDPPDD (departamento, provincia, distrito),
# y el de un centro poblado son los 6 dígitos de su distrito más 4 de correlativo.

huerfanos <- function(hijos, padres, etiqueta) {
  hijos <- hijos[!vacio(hijos)]
  falta <- unique(hijos[!hijos %in% padres])
  paso(length(falta) == 0, sprintf("%-46s (%d)", etiqueta, length(hijos)))
  if (length(falta)) {
    err(sprintf("%s: %d código(s) sin padre: %s",
                etiqueta, length(falta), paste(head(falta, 5), collapse = ", ")))
  }
}

huerfanos(substr(prov$inei, 1, 2), substr(dep$inei, 1, 2), "cada provincia cuelga de un departamento")
huerfanos(substr(dist$inei, 1, 4), substr(prov$inei, 1, 4), "cada distrito cuelga de una provincia")
huerfanos(ccpp$inei_distrito, dist$inei, "cada CCPP cuelga de un distrito")
huerfanos(substr(ccpp$inei_ccpp, 1, 6), dist$inei, "el prefijo del código de CCPP es su distrito")

# Los nombres están desnormalizados en cada nivel: deben coincidir con el padre.
nombre_padre <- function(hijos, claves_hijo, padres, claves_padre, columna, etiqueta) {
  idx <- match(claves_hijo, claves_padre)
  comparables <- !is.na(idx)
  difieren <- comparables & hijos[[columna]] != padres[[columna]][idx]
  paso(!any(difieren), sprintf("%-46s (%d)", etiqueta, sum(comparables)))
  if (any(difieren)) {
    ej <- head(which(difieren), 3)
    err(sprintf("%s: %d fila(s) discrepan. Ej: %s", etiqueta, sum(difieren),
                paste(sprintf("%s dice '%s', el padre dice '%s'",
                              hijos[[1]][ej], hijos[[columna]][ej],
                              padres[[columna]][idx][ej]), collapse = "; ")))
  }
}

nombre_padre(prov, substr(prov$inei, 1, 2), dep, substr(dep$inei, 1, 2),
             "departamento", "provincia.departamento == departamento")
nombre_padre(dist, substr(dist$inei, 1, 4), prov, substr(prov$inei, 1, 4),
             "provincia", "distrito.provincia == provincia")

# ---------------------------------------------------------------------------
seccion("4. Coordenadas dentro del Perú")
# ---------------------------------------------------------------------------
# Caja envolvente del territorio continental, con holgura.
LAT <- c(-18.4, 0.0)
LON <- c(-81.4, -68.6)

for (base in NIVELES) {
  d <- csv[[base]]
  lat <- suppressWarnings(as.numeric(ifelse(vacio(d$latitude), NA, d$latitude)))
  lon <- suppressWarnings(as.numeric(ifelse(vacio(d$longitude), NA, d$longitude)))
  con <- !is.na(lat) & !is.na(lon)
  fuera <- con & (lat < LAT[1] | lat > LAT[2] | lon < LON[1] | lon > LON[2])
  paso(!any(fuera), sprintf("%-22s %d coordenadas dentro de la caja del Perú", base, sum(con)))
  if (any(fuera)) {
    ej <- head(which(fuera), 3)
    err(sprintf("%s: %d coordenada(s) fuera del Perú. Ej: %s", base, sum(fuera),
                paste(sprintf("%s (%s, %s)", d[[1]][ej], d$latitude[ej], d$longitude[ej]), collapse = "; ")))
  }
}

# ---------------------------------------------------------------------------
seccion("5. Consistencia entre CSV, JSON y RDS")
# ---------------------------------------------------------------------------
# Los tres formatos se publican como equivalentes: deben tener las mismas filas,
# los mismos campos y los mismos valores.

desfasados <- character()

for (base in NIVELES) {
  d <- csv[[base]]

  for (fmt in c("json", "rds")) {
    ruta <- paste0(base, ".", fmt)
    if (!file.exists(ruta)) {
      paso(FALSE, sprintf("%-22s %-4s existe", base, fmt))
      err(sprintf("falta %s", ruta))
      next
    }
    otro <- if (fmt == "json") {
      jsonlite::fromJSON(ruta, simplifyDataFrame = TRUE)
    } else {
      as.data.frame(readRDS(ruta))
    }

    mapa <- emparejar(names(d), names(otro))
    heredados <- !is.na(mapa) & mapa != names(d)
    completo <- !any(is.na(mapa)) && identical(mapa[!is.na(mapa)], names(otro))

    paso(completo, sprintf("%-22s %-4s mismos campos que el CSV", base, fmt))
    if (!completo) {
      err(sprintf("%s.%s: campos distintos al CSV.\n      %-5s: %s\n      CSV  : %s",
                  base, fmt, fmt, paste(names(otro), collapse = ", "),
                  paste(names(d), collapse = ", ")))
    } else if (any(heredados)) {
      desfasados <- union(desfasados, ruta)
      paso(TRUE, sprintf("%-22s %-4s usa nombres heredados: %s", base, fmt,
                         paste(sprintf("%s -> %s", names(d)[heredados], mapa[heredados]), collapse = ", ")))
    }

    bien <- nrow(otro) == nrow(d)
    paso(bien, sprintf("%-22s %-4s mismas filas que el CSV (%d)", base, fmt, nrow(d)))
    if (!bien) {
      err(sprintf("%s.%s: %d filas, el CSV tiene %d", base, fmt, nrow(otro), nrow(d)))
      next
    }

    # Valores: exacto para texto; para los numéricos con tolerancia, porque el
    # .json actual está escrito con menos decimales (ver los avisos).
    for (k in seq_along(names(d))) {
      if (is.na(mapa[k])) next
      col_csv <- d[[k]]
      col_otro <- otro[[mapa[k]]]

      if (is.numeric(col_otro)) {
        a <- suppressWarnings(as.numeric(ifelse(vacio(col_csv), NA, col_csv)))
        b <- as.numeric(col_otro)
        discrepa <- xor(is.na(a), is.na(b))
        ambos <- !is.na(a) & !is.na(b)
        discrepa[ambos] <- abs(a[ambos] - b[ambos]) > 1e-3 * pmax(abs(a[ambos]), 1)
        if (any(discrepa)) {
          ej <- head(which(discrepa), 1)
          err(sprintf("%s.%s: '%s' difiere del CSV en %d fila(s). Ej: fila %d, CSV=%s %s=%s",
                      base, fmt, names(d)[k], sum(discrepa), ej, col_csv[ej], fmt, format(b[ej])))
        }
      } else {
        b <- ifelse(is.na(col_otro), "NA", as.character(col_otro))
        difieren <- sin_crlf(col_csv) != sin_crlf(b)
        if (any(difieren, na.rm = TRUE)) {
          ej <- head(which(difieren), 1)
          err(sprintf("%s.%s: '%s' difiere del CSV en %d fila(s). Ej: fila %d, CSV='%s' %s='%s'",
                      base, fmt, names(d)[k], sum(difieren, na.rm = TRUE), ej,
                      col_csv[ej], fmt, b[ej]))
        }
      }
    }
  }
}

# ---------------------------------------------------------------------------
seccion("6. Avisos: datos por regenerar y rarezas de las fuentes")
# ---------------------------------------------------------------------------

# 6.1 Precisión perdida al exportar a JSON: jsonlite::toJSON() redondea a 4
#     decimales salvo que se le pase digits = NA.
peor <- 0
afectados <- character()
for (base in NIVELES) {
  d <- csv[[base]]
  j <- jsonlite::fromJSON(paste0(base, ".json"), simplifyDataFrame = TRUE)
  mapa <- emparejar(names(d), names(j))
  for (k in seq_along(names(d))) {
    if (is.na(mapa[k])) next
    col <- j[[mapa[k]]]
    if (!is.numeric(col)) next
    a <- suppressWarnings(as.numeric(ifelse(vacio(d[[k]]), NA, d[[k]])))
    ambos <- !is.na(a) & !is.na(col)
    if (!any(ambos)) next
    rel <- abs(a[ambos] - col[ambos]) / pmax(abs(a[ambos]), 1e-12)
    if (max(rel) > 1e-9) {
      afectados <- union(afectados, base)
      peor <- max(peor, max(rel))
    }
  }
}
if (length(afectados)) {
  avi(sprintf(paste0("Los .json tienen menos precisión que los .csv (error relativo de hasta %.2g).\n",
                     "      Afecta a: %s\n",
                     "      Causa: jsonlite::toJSON() redondea a 4 decimales por defecto. generar-json.R\n",
                     "      ya pasa digits = NA, pero los .json publicados son anteriores a ese cambio."),
              peor, paste(afectados, collapse = ", ")))
}

# 6.2 Archivos generados antes de un renombrado de campos.
if (length(desfasados)) {
  avi(sprintf(paste0("%s usa(n) nombres de campo que el .rds ya no tiene.\n",
                     "      Se generaron antes del renombrado; regenerar con: Rscript generar-json.R"),
              paste(desfasados, collapse = ", ")))
}

# 6.3 Filas sin código en alguna de las dos codificaciones.
for (base in c("ubigeo_departamento", "ubigeo_provincia", "ubigeo_distrito")) {
  d <- csv[[base]]
  for (columna in c("inei", "reniec")) {
    faltan <- which(vacio(d[[columna]]))
    if (length(faltan)) {
      etiqueta <- if ("distrito" %in% names(d)) d$distrito else d$departamento
      avi(sprintf("%s: %d fila(s) sin código %s: %s", base, length(faltan), toupper(columna),
                  paste(sprintf("%s / %s", etiqueta[faltan], d$departamento[faltan]), collapse = "; ")))
    }
  }
}

# 6.4 Distritos sin los datos suplementarios.
sin_datos <- which(vacio(dist$superficie))
if (length(sin_datos)) {
  avi(sprintf("ubigeo_distrito: %d distrito(s) sin datos aumentados (superficie, altitud, coordenadas, índices).",
              length(sin_datos)))
}

# 6.5 Saltos de línea incrustados en campos de texto.
for (base in NIVELES) {
  d <- csv[[base]]
  for (columna in names(d)) {
    n <- sum(grepl("[\r\n]", d[[columna]]))
    if (n) {
      ej <- d[[columna]][grepl("[\r\n]", d[[columna]])][1]
      avi(sprintf("%s: %d valor(es) de '%s' traen un salto de línea incrustado. Ej: %s",
                  base, n, columna, gsub("[\r\n]+", "\\\\n", ej)))
    }
  }
}

# 6.6 Nombres de CCPP que no coinciden con los de su distrito.
idx <- match(ccpp$inei_distrito, dist$inei)
comparables <- !is.na(idx)
difieren <- comparables &
  (ccpp$departamento != dist$departamento[idx] |
   ccpp$provincia != dist$provincia[idx] |
   ccpp$distrito != dist$distrito[idx])
if (any(difieren)) {
  pares <- unique(sprintf("%s/%s != %s/%s",
                          dist$provincia[idx][difieren], dist$distrito[idx][difieren],
                          ccpp$provincia[difieren], ccpp$distrito[difieren]))
  avi(sprintf(paste0("ubigeo_ccpp: %d fila(s) nombran su departamento/provincia/distrito distinto que\n",
                     "      ubigeo_distrito (%d combinaciones). Son variantes ortográficas entre las fuentes\n",
                     "      de INEI, no errores de código: la integridad referencial por UBIGEO sí se cumple.\n",
                     "      Ej: %s"),
              sum(difieren), length(pares), paste(head(pares, 3), collapse = " | ")))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("-", 70), "\n", sep = "")

if (length(avisos)) {
  cat("\nAVISOS (", length(avisos), ") - no hacen fallar la validación:\n", sep = "")
  for (a in avisos) cat("  * ", a, "\n", sep = "")
}

if (length(errores)) {
  cat("\nERRORES (", length(errores), "):\n", sep = "")
  for (e in errores) cat("  ! ", e, "\n", sep = "")
  cat("\nValidación FALLIDA\n")
  quit(status = 1)
}

cat("\nValidación correcta: ", length(avisos), " aviso(s), 0 errores\n", sep = "")
