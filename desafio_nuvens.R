####################
#DESAFIO DAS NUVENS#
####################

# BIBLIOTECAS

library(tidyr)
library(dplyr)
library(rvest)
library(stringr)
library(wordcloud)
library(tm)
library(data.table)

# NUVEM 1

## Transcrição manual
## Fonte: https://youtu.be/dcuhHNiZW9k

url1<-"https://raw.githubusercontent.com/gabrielzanlorenssi/desafio_discursos/master/nuvem1.txt"
download.file(url1, "nuvem1.txt")
nuvem1<-read.csv("nuvem1.txt", header=FALSE)
nuvem1<-as.factor(nuvem1[1,1])

# NUVEM 2

url2<-"http://exame.abril.com.br/brasil/veja-a-integra-do-discurso-de-eduardo-cunha-na-camara/"
url2 <- read_html(url2)
nodes <- html_nodes(url2, ".article-content")
texto<-html_text(nodes)
split<-data.frame(strsplit(as.character(texto), "já teve."))
nuvem2<-split[2,1]

#NUVEM 3

url3<-"https://www.whitehouse.gov/inaugural-address"
url3<-read_html(url3)
nodes<- html_nodes(url3, "#content-start")
texto<-html_text(nodes)
split<-data.frame(strsplit(as.character(texto), "Delivery"))
nuvem3<-split[3,1]

#NUVEM 4

url4<-"http://painelpolitico.com/leia-transcricao-na-integra-da-conversa-entre-juca-e-sergio-machado/"
url4<-read_html(url4)
nodes<- html_nodes(url4, ".rd-post-content")
texto<-html_text(nodes)
split<-data.frame(strsplit(as.character(texto), "de dois trechos\nO PRIMEIRO"))
split<-data.frame(strsplit(as.character(split[2,1]), "\nAs informações são"))
nuvem4<-split[1,1]

# NUVEM 5
url5<-"http://www2.planalto.gov.br/acompanhe-planalto/discursos/discursos-do-presidente-da-republica/declaracao-a-imprensa-do-presidente-da-republica-michel-temer-brasilia-df-1"
url5<-read_html(url5)
nodes<-html_nodes(url5, "#parent-fieldname-text")
texto<-html_text(nodes)
split<-data.frame(strsplit(as.character(texto), "2017\n"))
split<-data.frame(strsplit(as.character(split[2,1]), "\nOuça"))
nuvem5<-split[1,1]

#NUVEM 6
url6<-"http://www2.planalto.gov.br/acompanhe-o-planalto/discursos/discursos-da-presidenta/discurso-da-presidenta-da-republica-dilma-rousseff-durante-solenidade-de-lancamento-dos-i-jogos-mundiais-dos-povos-indigenas-e-abertura-do-congresso-tecnico-brasilia-df"
url6<-read_html(url6)
nodes<-html_nodes(url6, "#parent-fieldname-text")
texto<-html_text(nodes)
split<-data.frame(strsplit(as.character(texto), "2015\n"))
split<-data.frame(strsplit(as.character(split[2,1]), "\nOuça"))
nuvem6<-split[1,1]

##############

#JUNTANDO TUDO

text<-c()
text<- c(as.character(nuvem1), as.character(nuvem2), as.character(nuvem3), as.character(nuvem4), 
         as.character(nuvem5), as.character(nuvem6))

## Passar tudo para minusculo
text<-tolower(text)
## Remover pontuação
text<-removePunctuation(text)
## Remover números
text<-removeNumbers(text)
## Remover palavras frequentes da lingua portuguesa
text2<-removeWords(text, stopwords("pt"))
## Remover espaços vazios em excesso
text2<-stripWhitespace(text2)

## Retirar acentos e caracteres especiais
text2<-str_replace_all(text2, "ú", "u")
text2<-str_replace_all(text2, "â", "a")
text2<-str_replace_all(text2, "á", "a")
text2<-str_replace_all(text2, "ã", "a")
text2<-str_replace_all(text2, "é", "e")
text2<-str_replace_all(text2, "ê", "e")
text2<-str_replace_all(text2, "ç", "c")
text2<-str_replace_all(text2, "í", "i")
text2<-str_replace_all(text2, "\n", "")
text2<-str_replace_all(text2, "û", "u")
text2<-str_replace_all(text2, "ú", "u")
text2<-str_replace_all(text2, "õ", "o")
text2<-str_replace_all(text2, "ó", "o")
text2<-str_replace_all(text2, "ô", "o")
text2<-str_replace_all(text2, "juca", "")
text2<-str_replace_all(text2, "machado", "")
text2<-str_replace_all(text2, "romero", "")
text2<-str_replace_all(text2, "sergio", "")
text2<-str_replace_all(text2, "trecho", "")
text2<-str_replace_all(text2, "incompreensivel", "")

#Salvando como CSV
x <- list(text2)
setDT(x)
write.csv(x, file="x.csv")

##Loop para nuvens no R
for (i in 1:6) {
  png(filename=paste("~/desafio", i, ".png", sep=""))
  wordcloud(x$V1[i], scale=c(5,0.5), max.words=20, random.order=FALSE, 
            rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  dev.off() 
}

##Nuvem do trump
trump<-removeWords(x$V1[3], stopwords("en"))

  png(filename=paste("~/desafio", 3, ".png", sep=""))
  wordcloud(trump, scale=c(5,0.5), max.words=20, random.order=FALSE, 
            rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  dev.off() 

















