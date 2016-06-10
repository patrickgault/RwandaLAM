uniqueVars = labels %>%
  filter(module != 'birth', module != 'couples') %>%
  group_by(var, varDescrip) %>% 
  summarise(num = n(), mod = first(module)) %>% 
  filter(num == 1)




uniqueVars %>% 
  group_by(mod) %>% 
  summarise(n())
