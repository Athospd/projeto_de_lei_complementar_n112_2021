library(tidyverse)
library(pdftools)

# livro
# titulo
# secao
# artigo
# paragrafo
# inciso
# alinea



paginas <- pdftools::pdf_text("DOC-Avulso inicial da matéria - SF212063437642-20210916.pdf")

livros <- paginas %>%
  limpeza_basica() %>%
  enframe(name = "pagina", value = "texto") %>%
  mutate(livro = str_extract(texto, "LIVRO [IVXDLCM]+")) %>%
  fill(livro) %>%
  group_by(livro) %>%
  nest(.key = "paginas") %>%
  mutate(paginas = setNames(paginas, livro))

# livros_com_titulos <-
  um_livro <- livros$paginas$`LIVRO I`
