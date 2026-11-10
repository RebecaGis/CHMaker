# CHMaker

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20670342.svg)](https://doi.org/10.5281/zenodo.20670342)

<!-- project-badges:start -->
![Aplicação: R · Shiny](https://img.shields.io/static/v1?label=Aplica%C3%A7%C3%A3o&message=R%20%C2%B7%20Shiny&color=276DC3&style=flat)
![Processamento: LiDAR · lidR](https://img.shields.io/static/v1?label=Processamento&message=LiDAR%20%C2%B7%20lidR&color=forestgreen&style=flat)
![CRS no código: SIRGAS 2000 / UTM 23S](https://img.shields.io/static/v1?label=CRS%20no%20c%C3%B3digo&message=SIRGAS%202000%20%2F%20UTM%2023S&color=blue&style=flat)
[![Autoria: Rebeca Diniz Moura](https://img.shields.io/static/v1?label=Autoria&message=Rebeca%20Diniz%20Moura&color=8b5cf6&style=flat)](https://github.com/RebecaGis)
![Licença: Apache-2.0](https://img.shields.io/static/v1?label=Licen%C3%A7a&message=Apache-2.0&color=blue&style=flat)
<!-- project-badges:end -->

CHMaker e um aplicativo em R Shiny para processamento de dados LiDAR em lote. Ele automatiza o fluxo de preparacao de arquivos `.las`, recorte por shapefile e geracao de produtos raster relacionados ao Modelo de Altura do Dossel, incluindo DTM, DSM e CHM.

## Principais Recursos

- Selecao de uma pasta base com os arquivos `.las` na subpasta `Las_Originals`
- Selecao de shapefile `.shp` para recorte espacial
- Verificacao previa de intersecoes entre arquivos LAS e poligonos
- Georreferenciamento e padronizacao dos arquivos processados
- Corte dos arquivos LAS por poligono do shapefile
- Geracao automatica de DTM, DSM e CHM em formato `.tif`
- Relatorio CSV com estatisticas dos CHMs gerados
- Interface visual com acompanhamento do pipeline e painel de log

## Requisitos

- R 4.0 ou superior
- RStudio recomendado
- Pacotes R:
  - `shiny`
  - `shinythemes`
  - `shinyFiles`
  - `fs`
  - `lidR`
  - `sf`
  - `terra`
  - `DT`

O launcher `INICIAR_CHMaker.R` verifica e instala automaticamente os pacotes ausentes.

## Como Executar

Abra o arquivo `INICIAR_CHMaker.R` no RStudio e execute o script.

Tambem e possivel iniciar pelo console do R:

```r
source("INICIAR_CHMaker.R")
```

Na primeira execucao, a instalacao dos pacotes pode levar alguns minutos.

## Como Usar

1. Organize os arquivos `.las` em `sua_pasta_base/Las_Originals/` e selecione `sua_pasta_base` na interface.
2. Selecione o shapefile `.shp` usado para recorte.
3. Ajuste os parametros de processamento:
   - Resolucao em metros
   - Percentil de clip do DSM
   - Suavizacao do DSM
4. Clique em `EXECUTAR PROCESSAMENTO`.
5. Acompanhe o andamento pelo pipeline, pela aba de log e pela aba de relatorio.

## Saidas Geradas

O CHMaker cria subpastas automaticamente dentro da pasta base selecionada:

- `1_Las_Georeferencing`: arquivos LAS preparados/georreferenciados
- `2_Las_Buffer_Crop`: arquivos LAS recortados por poligono
- `3_DTM_DSM`: rasters DTM/DSM e relatorio CSV
- `4_CHM`: rasters CHM gerados

O relatorio `CHM_stats_report.csv` apresenta as estatisticas calculadas para os CHMs processados.

## Guia de Instalacao

O passo a passo completo esta disponivel em:

[CHMaker_Guia_Instalacao.pdf](CHMaker_Guia_Instalacao.pdf)

## Estrutura do Projeto

```text
CHMaker/
├── app.R
├── INICIAR_CHMaker.R
├── CHMaker_Guia_Instalacao.pdf
└── README.md
```

## Recuperação dos arquivos

Os fontes `app.R` e `INICIAR_CHMaker.R` foram recuperados da pasta do Drive fornecida pela autora em 10/11/2026, sem alterações no código. O histórico local `.Rhistory` não foi incorporado. Consulte [o registro da recuperação](docs/RECUPERACAO.md).

## Observacoes

- O shapefile deve estar acompanhado dos seus arquivos auxiliares, como `.dbf`, `.shx` e `.prj`, quando aplicavel.
- Se o shapefile possuir o campo `Field`, ele sera usado como identificador no nome dos arquivos recortados.
- O processamento pode ser demorado em bases LiDAR grandes.
- O código atribui EPSG:31983 (SIRGAS 2000 / UTM 23S) em etapas do pipeline. Atribuir um CRS não reprojeta coordenadas; confirme que os dados de entrada são compatíveis antes de processar.
- Os scripts passaram na verificação de sintaxe com R 4.5.1. O pipeline LiDAR completo não foi executado nesta recuperação.

## Como Citar

Se utilizar o CHMaker, cite:

Rebeca Diniz Moura. CHMaker: aplicativo Shiny para processamento LiDAR em lote. Zenodo. https://doi.org/10.5281/zenodo.20670342
