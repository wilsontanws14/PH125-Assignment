library(tidyverse)
head(esoph)

nrow(esoph)

length(levels(esoph$agegp)) *
length(levels(esoph$alcgp)) *
length(levels(esoph$tobgp))

all_cases <- sum(esoph$ncases)

all_controls <- sum(esoph$ncontrols)

esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncases)/(sum(ncases) + sum(ncontrols)))

esoph %>% filter(alcgp == "0-39g/day") %>% summarize(sum(ncases)/(sum(ncases) + sum(ncontrols)))

esoph %>% filter(tobgp != "0-9g/day") %>% summarize(sum(ncases))
esoph %>% filter(tobgp == "0-9g/day") %>% summarize(sum(ncases))


esoph %>% filter(tobgp != "0-9g/day") %>% summarize(sum(ncontrols))
esoph %>% filter(tobgp == "0-9g/day") %>% summarize(sum(ncontrols))
          
sum(esoph$ncases)                                        

esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncases))

esoph %>% filter(tobgp == "30+" | alcgp=="120+") %>% summarize(sum(ncases))

esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncontrols))

sum(esoph$ncontrols)

esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncontrols))

esoph %>% filter(tobgp == "30+" & alcgp=="120+") %>% summarize(sum(ncontrols))

esoph %>% filter(tobgp == "30+" | alcgp=="120+") %>% summarize(sum(ncontrols))
