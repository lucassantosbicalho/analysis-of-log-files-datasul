
### Análise dos arquivos de log gerados pelo ERP em erros de integração em cadastros de cliente
### Uma análise simples para estruturar as mensagens de erro em tabela com código do erro, mensagem e ajuda.
### Datasul
### Lucas Bicalho 28/02/2023
### lucassantosbicalho@gmail.com


# Load libraries
library(tm) # corpus
library(stringr) # str utils
library(dplyr) # utils

# Clear environment
rm(list = ls())

path = "C:/Temp/erro_cliente_financeiro"

setwd(path)

path = "C:/Temp/erro_cliente_financeiro/logfiles/202302"

# Obter arquivos como objeto corpus
logs <- VCorpus(DirSource(path), # encoding = "UTF-8",
                readerControl = list(language = "pt"))

# Estrura data.frame
logdf <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Arquivo", "Emitente", "NumMsg", "Mensagem", "Ajuda"))

# Estruturar dados - Data Mining
l = 1
while(l <= length(logs)){
  text <- logs[[l]]$content
  
  # Obtem numero da linha do arquivo em que cada TAG se encontra
  # Pode estar em mais de uma linha, porque o log txt do Datasul pode ter mais de uma pagina
  i1 <- which(grepl("NumMsg:", text) == TRUE)
  i2 <- which(grepl("Mensagem:", text) == TRUE)
  i3 <- which(grepl("Ajuda:", text) == TRUE)
  index_equals <- ((i1 == i2) && (i1 == i3) && (i2 == i3))
  ir <- i1 + 1
  ir2 <- i1 + 2
  ir3 <- i1 + 2
  if(index_equals) { 
    tl <- l
    # Split content
    lTemp <- strsplit(text[i1], ":")      
    resto <- text[ir]
    resto2 <- text[ir2]
    resto3 <- text[ir2]
    # Percorrendo diferentes paginas do log Datasul
    for(i in 1:length(lTemp)) {
      
      # Arquivo (Repete cabecalho)
      logdf[tl,1] <- logs[[l]]$meta$id
      # Emitente (Repete cabecalho) 
      logdf[tl,2] <- str_split(logs[[l]]$meta$id, "_")[[1]][2]
      # Tratando NumMsg de cada pagina
      logdf[tl,3] <- str_trim(gsub("Mensagem", "", lTemp[[i]][2]))
      # Tratando Mensagem de cada pagina
      logdf[tl,4] <- str_trim(gsub("Ajuda", "", lTemp[[i]][3]))
      # Tratando Ajuda de cada pagina
      logdf[tl,5] <- str_squish(str_trim(str_replace_all(paste0(lTemp[[i]][4], " ", resto[i], " ", resto2[i], " ", resto3[i]), "[\r\n]" , "")))
      
      tl <- tl + 1
    }
  } else {
    print(paste0("Indexes not equals for ", l))
  }
  
  l <- l + 1
}

View(logdf)

logdf %>%                    
  group_by(Mensagem) %>%     
  summarise(n = n()) %>%
  mutate(prop = n / sum(n) ) -> analisedf
  
View(analisedf)

write.csv(analisedf, "logdf_sumarised.csv")
write.csv(logdf, "logdf.csv")
