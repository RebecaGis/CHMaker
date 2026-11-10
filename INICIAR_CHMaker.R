# ============================================================
#   CHMaker v1.0 - Launcher
#   Execute este arquivo no RStudio para iniciar o aplicativo
# ============================================================

cat("\n========================================\n")
cat("   CHMaker v1.0 - Iniciando...\n")
cat("========================================\n\n")

# 1. Verificar versão do R
if (as.numeric(R.Version()$major) < 4) {
  stop("CHMaker requer R versão 4.0 ou superior. Atualize o R em https://cran.r-project.org")
}

# 2. Instalar pacotes ausentes
cat("Verificando pacotes necessários...\n")

pkgs <- c("shiny", "shinythemes", "shinyFiles", "fs", "lidR", "sf", "terra", "DT")
faltando <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(faltando) > 0) {
  cat("Instalando pacotes:", paste(faltando, collapse = ", "), "\n")
  cat("(Isso pode levar alguns minutos na primeira vez)\n\n")
  install.packages(faltando, repos = "https://cloud.r-project.org")
} else {
  cat("Todos os pacotes já estão instalados.\n\n")
}

# 3. Verificar se app.R existe na mesma pasta
app_path <- file.path(dirname(sys.frame(1)$ofile), "app.R")

if (!file.exists(app_path)) {
  # Tentar diretório de trabalho
  app_path <- file.path(getwd(), "app.R")
}

if (!file.exists(app_path)) {
  stop(
    "Arquivo app.R não encontrado.\n",
    "Certifique-se de que INICIAR_CHMaker.R e app.R estão na mesma pasta."
  )
}

# 4. Iniciar o app
cat("Abrindo CHMaker no navegador...\n\n")
shiny::runApp(appDir = dirname(app_path), launch.browser = TRUE)
