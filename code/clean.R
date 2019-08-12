innlesid <- read_file(file = here::here("data", "chorales.lisp"))

milli_hreint <- innlesid %>% 
  str_replace_all(pattern = "\\n\\n", replacement = "\\\n") %>%
  str_remove(pattern = "\n$") %>% #Fjarlægjum aukalínubilið sem er aftast í strengnum
  str_split(pattern = "\\n") %>%  #Látum línubil skipta strengnum í nýja strengi
  unlist() %>% #Upp á lögun listans (viljum hafa top level lengdina jafna fjölda verka)
  map(~str_remove_all(., pattern = "\\(")) %>% 
  map(~str_remove_all(., pattern = "\\)\\)\\)")) %>% 
  map(~str_replace_all(., pattern = "\\)\\)", replacement = "\\) ")) %>%
  map(~str_remove_all(., pattern = "\\)"))

nafna_listi <- milli_hreint %>% 
  map(~str_extract(., pattern = "^.*? ")) %>% 
  map(~str_trim(., side = "both"))

gagnaramma_listi <- milli_hreint %>% 
  map(~str_remove(., pattern = "^.*? ")) %>%  #Fjarlægjum alla stafi frá byrjun til og með fyrsta bili
  map(~str_remove_all(., pattern = "[[:alpha:]]")) %>% #Fjarlægjum alla bókstafi
  map(~str_replace_all(., pattern = "  ", replacement = " ")) %>% #Öll tvöföld stafabil verða einföld
  map(~str_remove(., pattern = "^.*? ")) %>% #Fjarlægjum aftur alla stafi frá byrjun til og með fyrsta bili
  map(~read.table(text = ., 
                  col.names = c("start", "pitch", "duration", "key_signature", "time_signature", "fermata"))) %>% 
  map(~as_tibble(.))

names(gagnaramma_listi) <- unlist(nafna_listi)

gagnaramma_listi %>% 
  bind_rows(.id = "chorale")