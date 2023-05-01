library(tidyverse)

counties <- cross_df(list(c1=1:4,c2=1:4)) %>%
  mutate(district = case_when(c1==1 & c2<=3~ "1",
                              c1==2 & c2<=2 ~"1",
                              c1==3 & c2==1 ~ "1",
                              c1==4 & c2<=2 ~ "1",
                              T~"2"))


counties %>%
  ggplot(aes(x=c1, y=c2, fill=district)) +
  geom_tile(color="gray")
