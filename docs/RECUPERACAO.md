# Recuperação dos fontes do CHMaker

## Origem

- Pasta fornecida pela autora: https://drive.google.com/drive/folders/1jfD8yXOoccZNGCr5rY2CLTh-lNjApUo2
- Data: 10/11/2026.
- Fontes recuperados sem mudanças: `app.R` (50.871 bytes) e `INICIAR_CHMaker.R` (1.549 bytes).
- O README e o guia PDF existentes no GitHub foram preservados como base; o README recebeu badges e correções de uso.
- O badge DOI, a citação Zenodo e a licença existente foram mantidos.
- `.Rhistory` foi excluído da publicação por ser histórico de sessão local.

## Estrutura de entrada e saída

Selecione a pasta base que contém `Las_Originals/`, com os arquivos LAS originais. O aplicativo cria as subpastas:

- `1_Las_Georeferencing/`
- `2_Las_Buffer_Crop/`
- `3_DTM_DSM/`
- `4_CHM/`

O shapefile deve manter seus arquivos auxiliares. Se houver campo `Field`, ele participa dos identificadores de recorte.

## Validação e limites

Os dois scripts foram analisados com `parse(file=...)` no R 4.5.1, sem executar a aplicação ou instalar pacotes. A sintaxe foi aceita. Não foi realizado processamento de LAS, validação de resultados raster ou teste visual da interface.

O código usa atribuições de EPSG:31983 (SIRGAS 2000 / UTM 23S). A atribuição de um CRS não transforma coordenadas; o usuário deve conferir o sistema de referência dos dados antes de usar o pipeline. A recuperação não altera essas operações.

O launcher e o app podem instalar pacotes ausentes. Essa instalação não foi executada durante a recuperação.

Esta atualização foi preparada em branch separada; nenhuma versão no Zenodo foi alterada.
