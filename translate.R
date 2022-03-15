library(tidyverse)


t<-read_tsv("codon_usage.tab",col_names = F)
names(t)<-c("codon","AA")
gc<-setNames(as.list(t$AA), t$codon)

ga<-map(t$AA %>% unique(),~t %>% filter(AA==.x) %>% pull(codon))
names(ga)<-unique(t$AA)

translate<-function(s){
  tolower(gc[s])
}

rev_translate<-function(s){
  ga[toupper(s)] %>% unlist()
}

ck<-t$codon %>% sort()
codon_keys<-list(ck[1:8],ck[9:16],ck[17:24],ck[25:32],
                 ck[33:40],ck[41:48],ck[49:56],ck[57:64],c("Enter","Back"))
