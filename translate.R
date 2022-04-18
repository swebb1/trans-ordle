library(tidyverse)

t<-read_tsv("codon_usage_rna.tab",col_names = F)
names(t)<-c("codon","AA")

## Get trans and rev lists
gc<-setNames(as.list(t$AA), t$codon)
ga<-map(t$AA %>% unique(),~t %>% filter(AA==.x) %>% pull(codon))
names(ga)<-unique(t$AA)

## Translate and reverse translate codons
translate<-function(s){
  tolower(gc[s])
}

rev_translate<-function(s){
  ga[toupper(s)] %>% unlist()
}

## Keyboard
ck<-t$codon %>% sort()
codon_keys<-list(ck[1:8],ck[9:16],ck[17:24],ck[25:32],
                 ck[33:40],ck[41:48],ck[49:56],ck[57:64],c("Enter","Back"))

## Remove impossible words from wordlist
letters=t$AA %>% unique() %>% tolower()
remove = LETTERS[!LETTERS %>% tolower() %in% letters] %>% 
  tolower() %>% 
  paste0(.,collapse="")
words_all<-words_all[-grep(paste0("[",remove,"]"),words_all)]
words_common<-words_common[-grep(paste0("[",remove,"]"),words_common)]

