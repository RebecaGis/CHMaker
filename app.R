#
# Aplicativo Shiny para Processamento LiDAR - Versão Simplificada com Progresso
#

# Verificar e instalar pacotes necessários
required_packages <- c("shiny", "shinythemes", "shinyFiles", "fs", "lidR", "sf", "terra", "DT")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  message("Instalando pacotes necessários: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Carregar pacotes
library(shiny)
library(shinythemes)
library(shinyFiles)
library(fs)
library(sf)
library(terra)
library(DT)

# Verificar se o lidR está disponível
lidR_available <- require("lidR", quietly = TRUE)
if (!lidR_available) {
  message("Pacote lidR não está disponível. Algumas funcionalidades estarão limitadas.")
}

# Configurações para melhor performance
options(lidR.progress = FALSE)
if (lidR_available) {
  options(lidR.check.nested.parallelism = FALSE)
  lidR:::set_lidr_threads(1)  # Reduzir para 1 thread para evitar sobrecarga
}

# Desativar S2 para melhor performance em operações geográficas
sf::sf_use_s2(FALSE)

# FUNÇÃO GERAR DTM (MODIFICADA COM LOOP DE RESOLUÇÃO)
gerar_dtm <- function(las, res = 1) {
  if (!lidR_available) stop("lidR não disponível para gerar DTM")
  if (lidR::is.empty(las) || lidR::npoints(las) == 0) {
    message("Arquivo LAS vazio - pulando geração de DTM")
    return(NULL)
  }
  
  # Verificar se há pontos de solo
  ground_points <- sum(las$Classification == 2, na.rm = TRUE)
  if (ground_points == 0) {
    message("Nenhum ponto de solo encontrado - classificando...")
    las <- classificar_solo_csf(las)
    ground_points <- sum(las$Classification == 2, na.rm = TRUE)
    if (ground_points == 0) {
      message("Ainda não há pontos de solo após classificação - retornando NULL")
      return(NULL)
    }
  }
  
  resolucoes <- c(res, res * 1.5, res * 2, res * 3, res * 5)
  ultimo_erro <- NULL
  
  for (tentativa in seq_along(resolucoes)) {
    res_atual <- resolucoes[tentativa]
    
    tryCatch({
      message(paste("Tentativa", tentativa, "- Gerando DTM com resolução:", res_atual, "m"))
      dtm <- lidR::rasterize_terrain(las, res = res_atual, algorithm = lidR::tin())
      message(paste("DTM gerado com sucesso na resolução:", res_atual, "m"))
      return(dtm)
      
    }, error = function(e) {
      ultimo_erro <- e$message
      if (grepl("point out of raster", e$message, ignore.case = TRUE) || 
          grepl("unexpected internal error", e$message, ignore.case = TRUE) ||
          grepl("filter_with_grid", e$message, ignore.case = TRUE)) {
        message(paste("Erro 'point out of raster' na resolução", res_atual, "m"))
        message(paste("Tentativa", tentativa, "de", length(resolucoes), "- Próxima resolução:", 
                      ifelse(tentativa < length(resolucoes), resolucoes[tentativa + 1], "Nenhuma")))
      } else {
        message(paste("Outro erro na resolução", res_atual, "m:", e$message))
      }
      gc()
    })
  }
  
  message(paste("Todas as tentativas falharam. Último erro:", ultimo_erro))
  return(NULL)
}

classificar_solo_csf <- function(las) {
  if (!lidR_available) stop("lidR não disponível para classificação do solo")
  if (lidR::is.empty(las) || lidR::npoints(las) == 0) {
    message("Arquivo LAS vazio - pulando classificação do solo")
    return(las)
  }
  csf_algo <- lidR::csf(
    cloth_resolution = 0.5,
    class_threshold = 0.5,
    time_step = 0.65,
    sloop_smooth = FALSE
  )
  lidR::classify_ground(las, algorithm = csf_algo)
}

gerar_dsm <- function(las, res, suavizar = TRUE, clip_percentil = NA) {
  if (!lidR_available) stop("lidR não disponível para gerar DSM")
  resolucoes <- c(res, res * 1.5, res * 2, res * 3, res * 5)
  ultimo_erro <- NULL
  
  for (tentativa in seq_along(resolucoes)) {
    res_atual <- resolucoes[tentativa]
    tryCatch({
      message(paste("Tentativa", tentativa, "- Gerando DSM com resolução:", res_atual, "m"))
      pf_algo <- lidR::pitfree(thresholds = c(0, 2, 5, 10, 15, 20), subcircle = 0.2)
      dsm <- lidR::rasterize_canopy(las, res = res_atual, algorithm = pf_algo)
      if (suavizar) {
        dsm <- terra::focal(dsm, w = matrix(1, 3, 3), fun = median, na.rm = TRUE)
      }
      if (!is.na(clip_percentil)) {
        vals <- terra::values(dsm, na.rm = TRUE)
        if (length(vals) > 0) {
          lim <- quantile(vals, probs = clip_percentil/100, na.rm = TRUE)
          dsm <- terra::clamp(dsm, lower = min(vals, na.rm = TRUE), upper = lim, values = TRUE)
        }
      }
      message(paste("DSM gerado com sucesso na resolução:", res_atual, "m"))
      return(dsm)
    }, error = function(e) {
      ultimo_erro <- e$message
      if (grepl("point out of raster", e$message, ignore.case = TRUE) || 
          grepl("unexpected internal error", e$message, ignore.case = TRUE) ||
          grepl("filter_with_grid", e$message, ignore.case = TRUE)) {
        message(paste("Erro 'point out of raster' no DSM na resolução", res_atual, "m"))
      } else {
        message(paste("Outro erro no DSM na resolução", res_atual, "m:", e$message))
      }
      gc()
    })
  }
  message(paste("Todas as tentativas de DSM falharam. Último erro:", ultimo_erro))
  return(NULL)
}

alinhar_para <- function(src, template) {
  if (!terra::compareGeom(template, src, stopOnError = FALSE)) {
    terra::resample(src, template, method = "bilinear")
  } else {
    src
  }
}

estatisticas <- function(r) {
  v <- terra::values(r, na.rm = TRUE)
  if (length(v) == 0) {
    return(list(min = NA, max = NA, mean = NA, median = NA, p1 = NA, p50 = NA, p99 = NA))
  }
  list(
    min = min(v, na.rm = TRUE), max = max(v, na.rm = TRUE),
    mean = mean(v, na.rm = TRUE), median = median(v, na.rm = TRUE),
    p1 = quantile(v, 0.01, na.rm = TRUE),
    p50 = quantile(v, 0.50, na.rm = TRUE),
    p99 = quantile(v, 0.99, na.rm = TRUE)
  )
}

tratar_ruido <- function(las) {
  if (!lidR_available) return(las)
  if (lidR::is.empty(las)) return(las)
  las <- lidR::classify_noise(las, algorithm = lidR::sor(k = 8, m = 2))
  las <- lidR::filter_poi(las, Classification != LASNOISE)
  return(las)
}

verificar_projecao <- function(las, shapefile) {
  tryCatch({
    las_crs <- sf::st_crs(las)
    shp_crs <- sf::st_crs(shapefile)
    if (!is.na(las_crs$epsg) && !is.na(shp_crs$epsg) && las_crs != shp_crs) {
      message(paste("Transformando shapefile de", shp_crs$epsg, "para", las_crs$epsg))
      shapefile <- sf::st_transform(shapefile, crs = las_crs)
    }
    return(shapefile)
  }, error = function(e) {
    message(paste("Erro na verificação de projeção:", e$message))
    return(shapefile)
  })
}

verificar_intersecoes <- function(input_dir, buffer_shp, session) {
  tryCatch({
    uts <- sf::st_read(buffer_shp, quiet = TRUE)
    las_files <- fs::dir_ls(input_dir, regexp = "\\.las$", recurse = FALSE)
    n_files <- length(las_files)
    if (n_files == 0) return(list(intersecoes = NULL, mensagem = "Nenhum arquivo LAS encontrado"))
    intersecoes <- list()
    withProgress(message = 'Verificando interseções', value = 0, {
      for (i in seq_along(las_files)) {
        las_file <- las_files[i]
        filename <- fs::path_file(las_file)
        base_name <- fs::path_ext_remove(filename)
        incProgress(1/n_files, detail = paste("Verificando:", base_name))
        tryCatch({
          las_header <- lidR::readLASheader(las_file)
          las_crs <- sf::st_crs(las_header)
          shp_crs <- sf::st_crs(uts)
          if (!is.na(las_crs$epsg) && !is.na(shp_crs$epsg) && las_crs != shp_crs) {
            uts_transformed <- sf::st_transform(uts, crs = las_crs)
          } else {
            uts_transformed <- uts
          }
          las_bbox <- sf::st_bbox(c(
            xmin = las_header@PHB[["Min X"]], ymin = las_header@PHB[["Min Y"]],
            xmax = las_header@PHB[["Max X"]], ymax = las_header@PHB[["Max Y"]]
          ), crs = las_crs)
          las_polygon <- sf::st_as_sfc(las_bbox)
          for (j in seq_len(nrow(uts_transformed))) {
            polygon <- uts_transformed[j, ]
            intersects <- sf::st_intersects(las_polygon, polygon, sparse = FALSE)[1, 1]
            if (intersects) {
              intersecoes[[length(intersecoes) + 1]] <- list(
                las_file = las_file, polygon_index = j,
                polygon_id = ifelse("Field" %in% names(uts), uts$Field[j], j),
                polygon_geom = polygon,
                original_polygon_geom = uts[j, ]
              )
            }
          }
        }, error = function(e) {
          message(paste("Erro ao verificar interseção para", base_name, ":", e$message))
        })
      }
    })
    return(list(intersecoes = intersecoes,
                mensagem = paste("Verificação concluída. Encontradas", length(intersecoes), "interseções")))
  }, error = function(e) {
    return(list(intersecoes = NULL, mensagem = paste("Erro na verificação de interseções:", e$message)))
  })
}

processar_corte_buffer <- function(input_dir, output_dir, buffer_shp, session) {
  tryCatch({
    verificacao <- verificar_intersecoes(input_dir, buffer_shp, session)
    if (is.null(verificacao$intersecoes) || length(verificacao$intersecoes) == 0) return(verificacao$mensagem)
    intersecoes <- verificacao$intersecoes
    n_intersecoes <- length(intersecoes)
    arquivos_processados <- 0; features_processadas <- 0; arquivos_ignorados <- 0
    withProgress(message = 'Corte por Shapefile', value = 0, {
      for (i in seq_along(intersecoes)) {
        interseccao <- intersecoes[[i]]
        las_file <- interseccao$las_file
        filename <- fs::path_file(las_file)
        base_name <- fs::path_ext_remove(filename)
        polygon_index <- interseccao$polygon_index
        polygon_id <- interseccao$polygon_id
        incProgress(1/n_intersecoes, detail = paste("Processando:", base_name, "- Polígono", polygon_id))
        tryCatch({
          las <- lidR::readLAS(las_file)
          if (lidR::is.empty(las)) { arquivos_ignorados <- arquivos_ignorados + 1; next }
          uts_corrigido <- interseccao$polygon_geom
          output_las <- fs::path(output_dir, paste0(base_name, "_", polygon_id, ".las"))
          uts_corrigido_2d <- sf::st_zm(uts_corrigido)
          uts_sp <- as(uts_corrigido_2d, "Spatial")
          las_crop <- lidR::clip_roi(las, uts_sp)
          if (is.null(las_crop) || lidR::is.empty(las_crop)) {
            coords <- cbind(las$X, las$Y)
            points_sf <- sf::st_as_sf(data.frame(coords), coords = c("X1", "X2"), crs = sf::st_crs(las))
            inside <- sf::st_intersects(points_sf, uts_corrigido_2d, sparse = FALSE)
            inside <- as.logical(inside[,1])
            if (sum(inside, na.rm = TRUE) > 0) las_crop <- las[inside]
          }
          if (is.null(las_crop) || lidR::is.empty(las_crop) || lidR::npoints(las_crop) == 0) next
          lidR::writeLAS(las_crop, output_las)
          features_processadas <- features_processadas + 1
          arquivos_processados <- length(unique(sapply(intersecoes[1:i], function(x) x$las_file)))
          rm(las, las_crop); gc()
        }, error = function(e) {
          message(paste("Erro ao processar arquivo", base_name, "com polígono", polygon_id, ":", e$message))
          arquivos_ignorados <- arquivos_ignorados + 1
        })
      }
    })
    return(paste("Corte por shapefile concluído! Arquivos processados:", arquivos_processados,
                 "| Features criadas:", features_processadas,
                 "| Interseções ignoradas:", arquivos_ignorados))
  }, error = function(e) {
    return(paste("Erro no corte por shapefile:", e$message))
  })
}

processar_georreferenciamento <- function(input_dir, output_dir, session) {
  tryCatch({
    if (!lidR_available) return("lidR não disponível para georreferenciamento")
    las_files <- fs::dir_ls(input_dir, regexp = "\\.las$", recurse = FALSE)
    n_files <- length(las_files)
    if (n_files == 0) return("Nenhum arquivo LAS encontrado na pasta de entrada.")
    arquivos_processados <- 0; arquivos_ignorados <- 0; arquivos_com_erro <- 0
    withProgress(message = 'Georreferenciamento', value = 0, {
      for (i in seq_along(las_files)) {
        las_file <- las_files[i]
        filename <- fs::path_file(las_file)
        output_las <- fs::path(output_dir, gsub("\\.las$", "_ref.las", filename))
        incProgress(1/n_files, detail = paste("Processando", filename))
        success <- FALSE
        if (!success) {
          tryCatch({
            las <- lidR::readLAS(las_file, filter = "-drop_z_below 0")
            if (!lidR::is.empty(las)) {
              if (any(is.na(las@data$gpstime))) las@data$gpstime[is.na(las@data$gpstime)] <- 0
              sf::st_crs(las) <- 31983
              lidR::writeLAS(las, output_las)
              arquivos_processados <- arquivos_processados + 1
              success <- TRUE
            }
          }, error = function(e) message(paste("Método 1 falhou para", filename, ":", e$message)))
        }
        if (!success) {
          tryCatch({
            las <- lidR::readLAS(las_file, filter = "-drop_z_below 0 -drop_gpstime_below 0")
            if (!lidR::is.empty(las)) {
              sf::st_crs(las) <- 31983; lidR::writeLAS(las, output_las)
              arquivos_processados <- arquivos_processados + 1; success <- TRUE
            }
          }, error = function(e) message(paste("Método 2 falhou:", e$message)))
        }
        if (!success) {
          tryCatch({
            header <- lidR::readLASheader(las_file)
            las <- lidR::readLAS(las_file, filter = "-drop_z_below 0 -drop_gpstime_below 0 -keep_class 1 2 3 4 5")
            if (!lidR::is.empty(las)) {
              sf::st_crs(las) <- 31983; lidR::writeLAS(las, output_las)
              arquivos_processados <- arquivos_processados + 1; success <- TRUE
            }
          }, error = function(e) message(paste("Método 3 falhou:", e$message)))
        }
        if (!success && requireNamespace("rlas", quietly = TRUE)) {
          tryCatch({
            las_data <- rlas::read.las(las_file)
            if (nrow(las_data) > 0) {
              las <- lidR::LAS(data.frame(
                X = las_data$X, Y = las_data$Y, Z = las_data$Z,
                gpstime = if (!is.null(las_data$gpstime)) las_data$gpstime else rep(0, nrow(las_data)),
                Intensity = if (!is.null(las_data$Intensity)) las_data$Intensity else rep(0, nrow(las_data)),
                ReturnNumber = if (!is.null(las_data$ReturnNumber)) las_data$ReturnNumber else rep(1, nrow(las_data)),
                NumberOfReturns = if (!is.null(las_data$NumberOfReturns)) las_data$NumberOfReturns else rep(1, nrow(las_data)),
                ScanDirectionFlag = if (!is.null(las_data$ScanDirectionFlag)) las_data$ScanDirectionFlag else rep(1, nrow(las_data)),
                EdgeOfFlightline = if (!is.null(las_data$EdgeOfFlightline)) las_data$EdgeOfFlightline else rep(0, nrow(las_data)),
                Classification = if (!is.null(las_data$Classification)) las_data$Classification else rep(1, nrow(las_data)),
                ScanAngleRank = if (!is.null(las_data$ScanAngleRank)) las_data$ScanAngleRank else rep(0, nrow(las_data)),
                UserData = if (!is.null(las_data$UserData)) las_data$UserData else rep(0, nrow(las_data)),
                PointSourceID = if (!is.null(las_data$PointSourceID)) las_data$PointSourceID else rep(0, nrow(las_data))
              ))
              las@header <- header
              sf::st_crs(las) <- 31983; lidR::writeLAS(las, output_las)
              arquivos_processados <- arquivos_processados + 1; success <- TRUE
            }
          }, error = function(e) message(paste("Método 4 falhou:", e$message)))
        }
        if (!success) {
          tryCatch({
            dummy_data <- data.frame(
              X = c(0, 1), Y = c(0, 1), Z = c(0, 0), gpstime = c(0, 1),
              Intensity = c(0, 0), ReturnNumber = c(1, 1), NumberOfReturns = c(1, 1),
              Classification = c(1, 1)
            )
            las <- lidR::LAS(dummy_data)
            sf::st_crs(las) <- 31983
            output_placeholder <- fs::path(output_dir, gsub("\\.las$", "_PROBLEM.las", filename))
            lidR::writeLAS(las, output_placeholder)
            arquivos_ignorados <- arquivos_ignorados + 1
          }, error = function(e) { arquivos_com_erro <- arquivos_com_erro + 1 })
        }
        if (exists("las")) rm(las); gc()
      }
    })
    return(paste("Georreferenciamento concluído! Processados:", arquivos_processados,
                 "| Ignorados:", arquivos_ignorados, "| Com erro:", arquivos_com_erro))
  }, error = function(e) {
    return(paste("Erro crítico no georreferenciamento:", e$message))
  })
}

verificar_arquivo_las <- function(las_file) {
  tryCatch({
    if (!file.exists(las_file)) return("Arquivo não existe")
    if (file.size(las_file) == 0) return("Arquivo vazio")
    header <- lidR::readLASheader(las_file)
    if (is.null(header)) return("Falha na leitura do cabeçalho")
    n_points <- header@PHB[["Number of point records"]]
    if (n_points == 0) return("Zero pontos no cabeçalho")
    return("OK")
  }, error = function(e) { return(paste("Erro na verificação:", e$message)) })
}

processar_chm <- function(input_dir, output_dir_dtm_dsm, output_dir_chm, resolucao,
                          suavizar_dsm, percentil_clip, buffer_shp, session) {
  tryCatch({
    if (!lidR_available) return(list(status = "lidR não disponível para processamento CHM", relatorio = NULL))
    uts <- sf::st_read(buffer_shp, quiet = TRUE)
    las_files <- fs::dir_ls(input_dir, glob = "*.las")
    n_files <- length(las_files)
    if (n_files == 0) return(list(status = "Nenhum arquivo LAS encontrado para processar CHM.", relatorio = NULL))
    relatorio_csv <- file.path(output_dir_dtm_dsm, "CHM_stats_report.csv")
    write.csv(data.frame(UT = character(), Arquivo_CHM = character(),
                         Min = numeric(), Max = numeric(), Media = numeric(), Mediana = numeric(),
                         P1 = numeric(), P50 = numeric(), P99 = numeric()),
              relatorio_csv, row.names = FALSE)
    arquivos_processados <- 0; arquivos_ignorados <- 0
    withProgress(message = 'Geração de CHM', value = 0, {
      for (i in seq_along(las_files)) {
        las_file <- las_files[i]
        base <- fs::path_ext_remove(fs::path_file(las_file))
        incProgress(1/n_files, detail = paste("Processando", base))
        tryCatch({
          las <- lidR::readLAS(las_file)
          if (is.null(las) || lidR::is.empty(las) || lidR::npoints(las) == 0) {
            arquivos_ignorados <- arquivos_ignorados + 1; next
          }
          polygon_id <- stringr::str_extract(base, "[0-9]+$")
          if (is.na(polygon_id)) { polygon_id <- 1 } else { polygon_id <- as.numeric(polygon_id) }
          if (polygon_id > nrow(uts)) {
            polygon_selected <- uts[1, ]
          } else {
            polygon_selected <- uts[polygon_id, ]
          }
          polygon_selected <- verificar_projecao(las, polygon_selected)
          if (sum(las$Classification == 2, na.rm = TRUE) == 0) las <- classificar_solo_csf(las)
          lidR::projection(las) <- "EPSG:31983"
          las <- tratar_ruido(las)
          if (lidR::npoints(las) == 0) { arquivos_ignorados <- arquivos_ignorados + 1; next }
          dtm <- gerar_dtm(las, res = resolucao)
          if (is.null(dtm)) { arquivos_ignorados <- arquivos_ignorados + 1; next }
          dsm <- gerar_dsm(las, res = resolucao, suavizar = suavizar_dsm, clip_percentil = percentil_clip)
          if (is.null(dsm)) { arquivos_ignorados <- arquivos_ignorados + 1; next }
          dsm <- alinhar_para(dsm, dtm)
          polygon_vect <- terra::vect(polygon_selected)
          polygon_bbox <- terra::ext(polygon_vect)
          dtm_cropped <- terra::crop(dtm, polygon_bbox)
          dsm_cropped <- terra::crop(dsm, polygon_bbox)
          mask_cropped <- terra::rasterize(polygon_vect, dtm_cropped)
          dtm_final <- terra::mask(dtm_cropped, mask_cropped)
          dsm_final <- terra::mask(dsm_cropped, mask_cropped)
          chm_final <- dsm_final - dtm_final
          chm_final[chm_final < 0] <- 0
          chm_final[is.na(chm_final)] <- NA
          dtm_tif <- file.path(output_dir_dtm_dsm, paste0(base, "_DTM.tif"))
          dsm_tif <- file.path(output_dir_dtm_dsm, paste0(base, "_DSM.tif"))
          chm_tif <- file.path(output_dir_chm, paste0(base, "_CHM.tif"))
          terra::writeRaster(dtm_final, dtm_tif, overwrite = TRUE,
                             gdal = c("COMPRESS=LZW", "BIGTIFF=IF_SAFER"), NAflag = -9999)
          terra::writeRaster(dsm_final, dsm_tif, overwrite = TRUE,
                             gdal = c("COMPRESS=LZW", "BIGTIFF=IF_SAFER"), NAflag = -9999)
          terra::writeRaster(chm_final, chm_tif, overwrite = TRUE,
                             gdal = c("COMPRESS=LZW", "BIGTIFF=IF_SAFER"), NAflag = -9999)
          stats <- estatisticas(chm_final)
          write.table(data.frame(
            UT = base, Arquivo_CHM = chm_tif,
            Min = stats$min, Max = stats$max, Media = stats$mean, Mediana = stats$median,
            P1 = stats$p1, P50 = stats$p50, P99 = stats$p99
          ), relatorio_csv, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
          arquivos_processados <- arquivos_processados + 1
        }, error = function(e) {
          message(paste("Erro ao processar", base, ":", e$message))
          arquivos_ignorados <- arquivos_ignorados + 1
        })
      }
    })
    return(list(
      status = paste("Processamento DTM/DSM/CHM concluído! Processados:", arquivos_processados,
                     "| Ignorados:", arquivos_ignorados),
      relatorio = if (arquivos_processados > 0) relatorio_csv else NULL
    ))
  }, error = function(e) {
    return(list(status = paste("Erro no processamento CHM:", e$message), relatorio = NULL))
  })
}


library(shiny)
library(shinyFiles)
library(DT)


ui <- fluidPage(
  
  tags$head(
    tags$style(HTML('
      @import url("https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=DM+Sans:wght@300;400;500;600&display=swap");

      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: #f0f4f8;
        color: #2d3748;
        font-family: "DM Sans", sans-serif;
        font-size: 14px;
      }

      /* ── HEADER ── */
      .chm-header {
        background: #ffffff;
        border-bottom: 1px solid #dde3ea;
        padding: 0 28px;
        height: 64px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        position: sticky; top: 0; z-index: 100;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      .chm-header-left { display: flex; align-items: center; gap: 12px; }
      .chm-brand-text { display: flex; flex-direction: column; line-height: 1.1; }
      .chm-brand-title { font-family: "Space Mono", monospace; font-size: 18px; font-weight: 700; color: #12395b; }
      .chm-brand-subtitle { font-size: 11px; color: #6b7a8d; margin-top: 3px; }
      .chm-title {
        font-family: "Space Mono", monospace;
        font-size: 20px; font-weight: 700; color: #0f2d4a; letter-spacing: -0.8px;
      }
      .chm-subtitle { font-size: 12px; color: #6b7a8d; margin-top: 1px; }

      /* ── LAYOUT ── */
      .chm-layout {
        display: grid;
        grid-template-columns: 340px 1fr;
        grid-template-rows: 1fr auto;
        min-height: calc(100vh - 64px);
      }

      /* ── SIDEBAR ── */
      .chm-sidebar {
        background: #ffffff;
        border-right: 1px solid #dde3ea;
        padding: 20px 18px;
        display: flex; flex-direction: column; gap: 8px;
        overflow-y: auto;
      }
      .chm-section-label {
        font-family: "Space Mono", monospace;
        font-size: 10px; font-weight: 700; color: #6b7a8d;
        letter-spacing: 1.5px; text-transform: uppercase;
        margin: 8px 0 4px;
      }

      /* Steps */
      .chm-step {
        background: #f8fafc; border: 1px solid #dde3ea;
        border-radius: 10px; padding: 14px 16px;
        transition: border-color 0.2s;
      }
      .chm-step.active { border-color: #1a6baa; }
      .chm-step-header { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
      .chm-step-num {
        width: 22px; height: 22px; border-radius: 50%;
        background: #e8eef4; border: 1px solid #c5d0dc;
        font-family: "Space Mono", monospace; font-size: 10px; font-weight: 700; color: #4a5568;
        display: flex; align-items: center; justify-content: center; flex-shrink: 0;
      }
      .chm-step.active .chm-step-num {
        background: #1a6baa; border-color: #1a6baa; color: #fff;
      }
      .chm-step-title { font-size: 13px; font-weight: 600; color: #1a202c; }

      /* File picker */
      .chm-filepick { display: flex; flex-direction: column; gap: 6px; }
      .chm-filepick .shiny-input-container { margin: 0 !important; }
      .chm-path {
        background: #f0f4f8; border: 1px solid #c5d0dc;
        border-radius: 6px; padding: 7px 10px;
        font-family: "Space Mono", monospace; font-size: 10px; color: #2d3748;
        min-height: 30px; word-break: break-all; line-height: 1.4;
      }
      .chm-path pre, .chm-path .shiny-text-output {
        background: transparent !important; border: none !important;
        padding: 0 !important; margin: 0 !important;
        color: #2d3748 !important;
        font-family: "Space Mono", monospace !important;
        font-size: 10px !important; word-break: break-all !important;
        white-space: pre-wrap !important;
      }
      .chm-btn-pick {
        background: #eef2f7 !important; border: 1px solid #c5d0dc !important;
        color: #2d3748 !important; border-radius: 6px !important;
        font-size: 12px !important; font-weight: 500 !important;
        padding: 6px 12px !important; width: 100% !important;
        transition: all 0.15s !important; font-family: "DM Sans", sans-serif !important;
        cursor: pointer !important;
      }
      .chm-btn-pick:hover { background: #dde3ea !important; border-color: #b0bcc8 !important; }
      .chm-btn-pick .btn { 
        background: transparent !important; border: none !important;
        color: inherit !important; font: inherit !important;
        padding: 0 !important; width: 100% !important; text-align: left !important;
        box-shadow: none !important;
      }

      /* Params */
      .chm-params { display: flex; flex-direction: column; gap: 10px; }
      .chm-param-row { display: flex; flex-direction: column; gap: 4px; }
      .chm-param-label { font-size: 11px; font-weight: 500; color: #6b7a8d; }
      .chm-param-row .form-group { margin: 0 !important; }
      .chm-param-row label.control-label { display: none !important; }
      .chm-param-row .form-control {
        background: #ffffff !important; border: 1px solid #c5d0dc !important;
        color: #2d3748 !important; border-radius: 6px !important;
        font-size: 13px !important; padding: 6px 10px !important; height: 34px !important;
        font-family: "Space Mono", monospace !important;
      }
      .chm-param-row .form-control:focus {
        border-color: #1a6baa !important;
        box-shadow: 0 0 0 3px rgba(26,107,170,0.12) !important; outline: none !important;
      }
      .chm-param-row .checkbox { margin: 0 !important; }
      .chm-param-row .checkbox label { font-size: 12px; color: #2d3748; cursor: pointer; }
      .chm-param-row input[type=checkbox] { accent-color: #1a6baa; }

      /* Execute */
      .chm-execute-wrap { margin-top: 4px; }
      #processar {
        width: 100%; background: linear-gradient(135deg, #1a6baa 0%, #0f4d80 100%) !important;
        border: none !important; border-radius: 8px !important; color: #fff !important;
        font-family: "Space Mono", monospace !important; font-size: 13px !important;
        font-weight: 700 !important; padding: 12px 16px !important; letter-spacing: 0.5px !important;
        transition: all 0.2s !important; box-shadow: 0 2px 8px rgba(26,107,170,0.25) !important;
      }
      #processar:hover {
        transform: translateY(-1px) !important;
        box-shadow: 0 4px 16px rgba(26,107,170,0.35) !important;
      }

      /* Pipeline */
      .chm-pipeline {
        background: #f8fafc; border: 1px solid #dde3ea;
        border-radius: 10px; padding: 14px 16px; margin-top: 4px;
      }
      .chm-pipeline-steps { display: flex; flex-direction: column; position: relative; }
      .chm-pipe-step {
        display: flex; align-items: flex-start; gap: 10px;
        padding: 6px 0; position: relative;
      }
      .chm-pipe-step:not(:last-child)::after {
        content: ""; position: absolute; left: 9px; top: 24px;
        width: 1px; height: calc(100% - 8px); background: #c5d0dc;
      }
      .chm-pipe-dot {
        width: 20px; height: 20px; border-radius: 50%;
        background: #e8eef4; border: 1px solid #c5d0dc;
        flex-shrink: 0; display: flex; align-items: center; justify-content: center;
        font-size: 9px; color: #6b7a8d; z-index: 1; transition: all 0.3s;
      }
      .chm-pipe-dot.running {
        background: #1a6baa; border-color: #1a6baa; color: #fff;
        animation: pulse 1.5s infinite;
      }
      .chm-pipe-dot.done { background: #1a6baa; border-color: #0f4d80; color: #fff; }
      .chm-pipe-dot.error { background: #e53e3e; border-color: #c53030; color: #fff; }
      @keyframes pulse {
        0%, 100% { box-shadow: 0 0 0 0 rgba(26,107,170,0.4); }
        50% { box-shadow: 0 0 0 6px rgba(26,107,170,0); }
      }
      .chm-pipe-name { font-size: 12px; font-weight: 500; color: #2d3748; line-height: 20px; }
      .chm-pipe-status {
        font-family: "Space Mono", monospace; font-size: 10px; color: #6b7a8d; margin-top: 1px;
      }

      /* ── MAIN / TABS via Shiny native tabsetPanel ── */
      .chm-main { background: #f0f4f8; }
      .chm-main .nav-tabs {
        background: #ffffff; border-bottom: 1px solid #dde3ea !important;
        padding: 0 20px; margin: 0 !important;
      }
      .chm-main .nav-tabs > li > a {
        font-family: "DM Sans", sans-serif; font-size: 13px; font-weight: 500;
        color: #6b7a8d !important; border: none !important;
        border-bottom: 2px solid transparent !important;
        border-radius: 0 !important; padding: 14px 18px !important;
        margin: 0 !important; background: transparent !important;
        transition: all 0.15s;
      }
      .chm-main .nav-tabs > li > a:hover { color: #2d3748 !important; }
      .chm-main .nav-tabs > li.active > a {
        color: #1a202c !important; border-bottom: 2px solid #1a6baa !important;
        background: transparent !important;
      }
      .chm-main .tab-content { padding: 24px; }

      /* Instructions */
      .chm-how-title {
        font-family: "Space Mono", monospace; font-size: 10px; font-weight: 700;
        color: #6b7a8d; letter-spacing: 1.5px; text-transform: uppercase; margin-bottom: 14px;
      }
      .chm-steps-list { display: flex; flex-direction: column; gap: 8px; margin-bottom: 20px; }
      .chm-instr-step {
        display: flex; align-items: flex-start; gap: 12px;
        padding: 12px 16px; background: #ffffff;
        border: 1px solid #dde3ea; border-radius: 8px;
      }
      .chm-instr-num {
        font-family: "Space Mono", monospace; font-size: 11px;
        font-weight: 700; color: #1a6baa; min-width: 20px; margin-top: 1px;
      }
      .chm-instr-text { font-size: 13px; color: #2d3748; line-height: 1.5; }
      .chm-info-box {
        background: #eef6ff; border: 1px solid #dde3ea;
        border-left: 3px solid #1a6baa;
        border-radius: 8px; padding: 14px 16px; margin-bottom: 20px;
      }
      .chm-info-box-title {
        font-size: 11px; font-weight: 600; color: #1a6baa;
        text-transform: uppercase; letter-spacing: 0.8px; margin-bottom: 10px;
      }
      .chm-info-list { display: flex; flex-direction: column; gap: 6px; }
      .chm-info-item {
        display: flex; align-items: flex-start; gap: 8px;
        font-size: 12px; color: #4a5568; line-height: 1.5;
      }
      .chm-info-item::before { content: "›"; color: #1a6baa; font-weight: 700; flex-shrink: 0; }

      /* Empty state */
      .chm-empty-state {
        display: flex; flex-direction: column; align-items: center;
        justify-content: center; padding: 64px 32px; text-align: center; gap: 12px;
      }
      .chm-empty-icon { font-size: 40px; line-height: 1; opacity: 0.25; }
      .chm-empty-title {
        font-family: "Space Mono", monospace; font-size: 13px;
        color: #a0aab4; font-weight: 700;
      }
      .chm-empty-sub { font-size: 12px; color: #c5d0dc; }

      /* Log */
      .chm-log {
        background: #ffffff; border: 1px solid #dde3ea; border-radius: 8px;
        padding: 16px; font-family: "Space Mono", monospace; font-size: 11px;
        color: #4a5568; line-height: 1.7; min-height: 300px;
        white-space: pre-wrap; word-break: break-all;
      }
      .chm-log pre, .chm-log .shiny-text-output {
        background: transparent !important; border: none !important;
        padding: 0 !important; margin: 0 !important;
        font-family: "Space Mono", monospace !important;
        font-size: 11px !important; color: #4a5568 !important;
        white-space: pre-wrap !important;
      }

      /* DT */
      .dataTables_wrapper { color: #2d3748 !important; }
      .dataTable { background: #ffffff !important; color: #2d3748 !important; border: none !important; }
      .dataTable thead th {
        background: #f0f4f8 !important; color: #6b7a8d !important;
        border-bottom: 1px solid #dde3ea !important;
        font-family: "Space Mono", monospace !important;
        font-size: 11px !important; letter-spacing: 0.5px !important;
        padding: 10px 12px !important; font-weight: 700 !important;
      }
      .dataTable tbody tr { border-bottom: 1px solid #dde3ea !important; }
      .dataTable tbody tr:hover td { background: #f8fafc !important; }
      .dataTable tbody td { padding: 9px 12px !important; font-size: 12px !important; }
      .dataTables_filter input, .dataTables_length select {
        background: #ffffff !important; border: 1px solid #c5d0dc !important;
        color: #2d3748 !important; border-radius: 6px !important;
        padding: 4px 8px !important; font-size: 12px !important;
      }
      .dataTables_info, .dataTables_paginate { color: #6b7a8d !important; font-size: 12px !important; }
      .paginate_button { color: #6b7a8d !important; }
      .paginate_button.current {
        background: #dde3ea !important; color: #2d3748 !important; border-radius: 4px !important;
      }


      /* ── FOOTER ── */
      .chm-footer {
        background: #ffffff;
        border-top: 1px solid #dde3ea;
        padding: 12px 28px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        grid-column: 1 / -1;
      }
      .chm-footer-sig {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .chm-footer-name {
        font-family: "Space Mono", monospace;
        font-size: 11px;
        font-weight: 700;
        color: #0f2d4a;
        letter-spacing: 0.3px;
      }
      .chm-footer-role {
        font-size: 11px;
        color: #6b7a8d;
      }
      .chm-footer-sep {
        width: 1px;
        height: 20px;
        background: #dde3ea;
      }
      .chm-footer-app {
        font-family: "Space Mono", monospace;
        font-size: 10px;
        color: #a0aab4;
        letter-spacing: 0.5px;
      }
      .chm-footer-dot {
        width: 6px; height: 6px;
        border-radius: 50%;
        background: #1a6baa;
        display: inline-block;
        margin-right: 6px;
      }

      /* Hide Shiny default title */
      .container-fluid { padding: 0 !important; }
      h1.shiny-title { display: none !important; }
      .well { background: transparent !important; border: none !important;
               box-shadow: none !important; padding: 0 !important; }
      .shiny-input-container { margin-bottom: 0 !important; }
    '))
  ),
  
  # ── HEADER ──
  div(class = "chm-header",
      div(class = "chm-header-left",
          div(class = "chm-brand-text",
              div(class = "chm-brand-title", "CHMaker"),
              div(class = "chm-brand-subtitle", "Processamento LiDAR em Lote")
          )
      )
  ),
  
  div(class = "chm-layout",
      
      # ── SIDEBAR ──
      div(class = "chm-sidebar",
          
          div(class = "chm-step active",
              div(class = "chm-step-header",
                  div(class = "chm-step-num", "1"),
                  div(class = "chm-step-title", "Pasta Base")
              ),
              div(class = "chm-filepick",
                  div(class = "chm-btn-pick",
                      shinyDirButton("base_dir", "Selecionar pasta com .las", "Selecionar pasta base")
                  ),
                  div(class = "chm-path", verbatimTextOutput("base_dir_text"))
              )
          ),
          
          div(class = "chm-step",
              div(class = "chm-step-header",
                  div(class = "chm-step-num", "2"),
                  div(class = "chm-step-title", "Shapefile de Recorte")
              ),
              div(class = "chm-filepick",
                  div(class = "chm-btn-pick",
                      shinyFilesButton("buffer_shp_file", "Selecionar arquivo .shp",
                                       "Selecionar shapefile", multiple = FALSE, filetypes = c("shp"))
                  ),
                  div(class = "chm-path", verbatimTextOutput("buffer_shp_text"))
              )
          ),
          
          div(class = "chm-step",
              div(class = "chm-step-header",
                  div(class = "chm-step-num", "3"),
                  div(class = "chm-step-title", "Parâmetros")
              ),
              div(class = "chm-params",
                  div(class = "chm-param-row",
                      div(class = "chm-param-label", "Resolução (metros)"),
                      numericInput("resolucao", NULL, value = 0.40, min = 0.1, max = 5, step = 0.1)
                  ),
                  div(class = "chm-param-row",
                      div(class = "chm-param-label", "Percentil clip do DSM"),
                      numericInput("percentil_clip", NULL, value = 99.5, min = 90, max = 100, step = 0.1)
                  ),
                  div(class = "chm-param-row",
                      checkboxInput("suavizar_dsm", "Aplicar suavização no DSM", value = TRUE)
                  )
              )
          ),
          
          div(class = "chm-execute-wrap",
              actionButton("processar", "▶  EXECUTAR PROCESSAMENTO")
          ),
          
          div(class = "chm-section-label", "Pipeline"),
          div(class = "chm-pipeline",
              div(class = "chm-pipeline-steps",
                  div(class = "chm-pipe-step",
                      div(class = "chm-pipe-dot", id = "dot-1", "①"),
                      div(div(class = "chm-pipe-name", "Georreferenciamento"),
                          div(class = "chm-pipe-status", id = "status-1", "Aguardando"))
                  ),
                  div(class = "chm-pipe-step",
                      div(class = "chm-pipe-dot", id = "dot-2", "②"),
                      div(div(class = "chm-pipe-name", "Corte por shapefile"),
                          div(class = "chm-pipe-status", id = "status-2", "Aguardando"))
                  ),
                  div(class = "chm-pipe-step",
                      div(class = "chm-pipe-dot", id = "dot-3", "③"),
                      div(div(class = "chm-pipe-name", "Geração DTM / DSM"),
                          div(class = "chm-pipe-status", id = "status-3", "Aguardando"))
                  ),
                  div(class = "chm-pipe-step",
                      div(class = "chm-pipe-dot", id = "dot-4", "④"),
                      div(div(class = "chm-pipe-name", "Geração CHM"),
                          div(class = "chm-pipe-status", id = "status-4", "Aguardando"))
                  )
              )
          )
      ),
      
      # ── MAIN — native tabsetPanel so Shiny renders outputs correctly ──
      div(class = "chm-main",
          tabsetPanel(id = "main_tabs",
                      tabPanel("Instruções",
                               div(class = "chm-how-title", "Como usar"),
                               div(class = "chm-steps-list",
                                   div(class = "chm-instr-step",
                                       span(class = "chm-instr-num", "01"),
                                       span(class = "chm-instr-text", "Selecione a pasta base com os arquivos .las originais")),
                                   div(class = "chm-instr-step",
                                       span(class = "chm-instr-num", "02"),
                                       span(class = "chm-instr-text", "Selecione o shapefile — cada polígono gera um arquivo LAS independente")),
                                   div(class = "chm-instr-step",
                                       span(class = "chm-instr-num", "03"),
                                       span(class = "chm-instr-text", "Ajuste resolução, suavização e percentil conforme a densidade da nuvem")),
                                   div(class = "chm-instr-step",
                                       span(class = "chm-instr-num", "04"),
                                       span(class = "chm-instr-text", "Clique em Executar e acompanhe o pipeline à esquerda")),
                                   div(class = "chm-instr-step",
                                       span(class = "chm-instr-num", "05"),
                                       span(class = "chm-instr-text", "DTM, DSM, CHM (.tif) e relatório CSV são salvos automaticamente nas subpastas"))
                               ),
                               div(class = "chm-info-box",
                                   div(class = "chm-info-box-title", "Informações importantes"),
                                   div(class = "chm-info-list",
                                       div(class = "chm-info-item", "Verificação prévia de interseções LAS × polígonos — só combinações válidas são processadas"),
                                       div(class = "chm-info-item", "Saída: arquivoLAS_identificadorPolígono.las"),
                                       div(class = "chm-info-item", "Campo \'Field\' no shapefile é usado como identificador, se existir"),
                                       div(class = "chm-info-item", "Falha na rasterização aciona fallback automático em 5 resoluções progressivas")
                                   )
                               )
                      ),
                      
                      tabPanel("Relatório",
                               uiOutput("relatorio_ui")
                      ),
                      
                      tabPanel("Log",
                               div(class = "chm-log", verbatimTextOutput("console_output"))
                      )
          )
      )
  ),
  
  
  # ── FOOTER ──
  tags$footer(class = "chm-footer",
              div(class = "chm-footer-sig",
                  div(class = "chm-footer-name",
                      tags$span(class = "chm-footer-dot"),
                      "Rebeca Diniz Moura"
                  ),
                  div(class = "chm-footer-sep"),
                  div(class = "chm-footer-role", "Desenvolvedora")
              ),
              div(class = "chm-footer-app",
                  "CHMaker v1.0 · Processamento LiDAR em Lote · 2025"
              )
  ),
  
  # ── Pipeline message handler ──
  tags$script(HTML('
    Shiny.addCustomMessageHandler("pipeline", function(msg) {
      var dot = document.getElementById("dot-" + msg.step);
      var st  = document.getElementById("status-" + msg.step);
      if (!dot) return;
      dot.className = "chm-pipe-dot " + msg.state;
      var labels = {running: "Processando...", done: "Concluído ✓", error: "Erro ✗"};
      if (st) st.innerText = labels[msg.state] || msg.state;
    });
  '))
)


# Server
server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "base_dir", roots = volumes, session = session)
  shinyFileChoose(input, "buffer_shp_file", roots = volumes, session = session, filetypes = c('shp'))
  paths <- reactiveValues(
    base_dir = "", input_dir = "", output_dir1 = "", output_dir2 = "",
    output_dir3 = "", output_dir4 = "", buffer_shp = ""
  )
  status <- reactiveValues(
    geral = "Aguardando início do processamento",
    detalhado = "Nenhuma etapa em execução",
    relatorio = NULL
  )
  observeEvent(input$base_dir, {
    if (!is.integer(input$base_dir)) {
      base_path <- parseDirPath(volumes, input$base_dir)
      paths$base_dir <- base_path
      paths$input_dir <- file.path(base_path, "Las_Originals")
      paths$output_dir1 <- file.path(base_path, "1_Las_Georeferencing")
      paths$output_dir2 <- file.path(base_path, "2_Las_Buffer_Crop")
      paths$output_dir3 <- file.path(base_path, "3_DTM_DSM")
      paths$output_dir4 <- file.path(base_path, "4_CHM")
    }
  })
  observeEvent(input$buffer_shp_file, {
    if (!is.integer(input$buffer_shp_file)) {
      paths$buffer_shp <- parseFilePaths(volumes, input$buffer_shp_file)$datapath
    }
  })
  output$base_dir_text <- renderText({ if (paths$base_dir != "") paths$base_dir else "Nenhuma pasta base com .las selecionada" })
  output$buffer_shp_text <- renderText({ if (paths$buffer_shp != "") paths$buffer_shp else "Nenhum shapefile máscara selecionado" })
  output$status_geral <- renderText({ status$geral })
  output$status_detalhado <- renderText({ status$detalhado })
  output$console_output <- renderText({ paste(status$geral, "\n", status$detalhado) })
  observeEvent(input$processar, {
    if (paths$base_dir == "") { status$geral <- "Erro: Selecione uma pasta base primeiro."; return() }
    if (paths$buffer_shp == "") { status$geral <- "Erro: Selecione o shapefile primeiro."; return() }
    if (!file.exists(paths$buffer_shp)) { status$geral <- "Erro: O arquivo shapefile não existe."; return() }
    if (!grepl("\\.shp$", paths$buffer_shp, ignore.case = TRUE)) { status$geral <- "Erro: Não é um shapefile (.shp)."; return() }
    if (!lidR_available) { status$geral <- "Erro: O pacote lidR não está disponível."; return() }
    uts <- try(sf::st_read(paths$buffer_shp, quiet = TRUE), silent = TRUE)
    if (inherits(uts, "try-error") || nrow(uts) == 0) { status$geral <- "Erro: Shapefile sem features válidas."; return() }
    dir.create(paths$output_dir1, showWarnings = FALSE, recursive = TRUE)
    dir.create(paths$output_dir2, showWarnings = FALSE, recursive = TRUE)
    dir.create(paths$output_dir3, showWarnings = FALSE, recursive = TRUE)
    dir.create(paths$output_dir4, showWarnings = FALSE, recursive = TRUE)
    status$geral <- "Iniciando georreferenciamento..."
    session$sendCustomMessage("pipeline", list(step=1, state="running"))
    resultado_geo <- processar_georreferenciamento(paths$input_dir, paths$output_dir1, session)
    status$geral <- resultado_geo
    session$sendCustomMessage("pipeline", list(step=1, state=if(grepl("Erro",resultado_geo)) "error" else "done"))
    if (grepl("Erro", resultado_geo)) return()
    status$geral <- "Processando corte por shapefile..."
    session$sendCustomMessage("pipeline", list(step=2, state="running"))
    resultado_buffer <- processar_corte_buffer(paths$output_dir1, paths$output_dir2, paths$buffer_shp, session)
    status$geral <- resultado_buffer
    session$sendCustomMessage("pipeline", list(step=2, state=if(grepl("Erro",resultado_buffer)) "error" else "done"))
    if (grepl("Erro", resultado_buffer)) return()
    status$geral <- "Gerando DTM/DSM/CHM..."
    session$sendCustomMessage("pipeline", list(step=3, state="running"))
    resultado_chm <- processar_chm(paths$output_dir2, paths$output_dir3, paths$output_dir4,
                                   input$resolucao, input$suavizar_dsm, input$percentil_clip,
                                   paths$buffer_shp, session)
    status$geral <- resultado_chm$status
    session$sendCustomMessage("pipeline", list(step=3, state=if(grepl("Erro",resultado_chm$status)) "error" else "done"))
    session$sendCustomMessage("pipeline", list(step=4, state=if(grepl("Erro",resultado_chm$status)) "error" else "done"))
    if (!is.null(resultado_chm$relatorio) && file.exists(resultado_chm$relatorio)) {
      status$relatorio <- read.csv(resultado_chm$relatorio)
    }
  })
  output$relatorio_ui <- renderUI({
    if (is.null(status$relatorio)) {
      div(class = "chm-empty-state",
          div(class = "chm-empty-icon", "📊"),
          div(class = "chm-empty-title", "SEM DADOS AINDA"),
          div(class = "chm-empty-sub", "Execute o processamento para ver as estatísticas dos CHMs gerados.")
      )
    } else {
      tagList(
        h4(style = "font-family: \'Space Mono\', monospace; font-size: 13px; color: #6b7a8d; margin-bottom: 16px;",
           "ESTATÍSTICAS DOS CHM GERADOS"),
        DTOutput("relatorio_table")
      )
    }
  })
  output$relatorio_table <- renderDT({
    req(status$relatorio)
    datatable(status$relatorio, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
