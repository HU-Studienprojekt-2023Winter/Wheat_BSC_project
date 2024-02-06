rm(list=ls())
pacman::p_load(ggplot2,deSolve,foreach,dplyr,ggpubr,scales,purrr)
cv <- read.csv("data/cultivar_info.csv")

df<- purrr::map_dfr(list.files("data_Eren/.xlsx"),~{
  batch <- .x%>% strsplit(.,"_") %>% unlist() %>% .[3] %>% gsub(".xlsx","",.)
  purrr::map_dfr(1:10,function(sid){
    file<- xlsx::read.xlsx(paste0("data_Eren/.xlsx/",.x),sheetIndex = sid)
    condi <- grepl("\\.",names(file)) 
    if(any(condi)){
      names(file)[which(condi)] <-  names(file)[which(condi)] %>% gsub("\\.","",.) 
    }
    
    file%>% 
      mutate(across(c(flower,small,spike,medium,large),as.character)) %>% 
      select(var:total) %>% 
      na.omit() %>% 
      mutate(rep=sid,
             batch=batch %>% as.numeric())
    
  })
}) %>%
  mutate(flower_abort=case_when(flower%in%c("aborted","2-aborted")~2,
                                flower=="1-aborted"~1,
                                T~0),
         flower=case_when(flower%in%c("aborted","2-aborted")~"0",
                          flower=="1-aborted"~"1",
                          T~flower),
         across(small:large,~gsub("( |\\,$)","",.x)),
         across(small:large,~gsub("ß","0",.x)),
         across(small:large,~gsub("\\.",",",.x)))%>% 
  tidyr::pivot_longer(small:large,
                      names_to = "kernel.type",
                      values_to = "floret.pos") %>% 
  tidyr::separate_rows(floret.pos, sep = ",") %>% 
  group_by(batch,plot_id,rep) %>% 
  mutate(across(c(spike:total,floret.pos),as.numeric),
         type=cut(spike,3) %>% as.numeric(),
         type=case_when(type==1~"basal",
                        type==2~"central",
                        T~"apical")) %>% 
  arrange(batch,plot_id,rep,spike,flower,floret.pos) %>% 
  left_join(.,cv)

write.csv(df,"data_Eren/merged_data.csv",row.names = F)
