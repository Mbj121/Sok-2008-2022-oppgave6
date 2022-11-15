library(patchwork)
library(tidyverse)
library(reshape2)
library(PxWebApiData)


df1 <- ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
               Kjonn=list('item', c("1", "2")), 
               Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
               NACE2007=FALSE, 
               Sykefraver2=FALSE, 
               ContentsCode=TRUE)

df1 <- as_tibble(df1[[1]])
df1 <- df1 %>% 
  select("kjønn", "år", "value") %>% 
  rename(sykefravær = value)

df2 <- ApiData("https://data.ssb.no/api/v0/no/table/05111/", 
                        ArbStyrkStatus=list('item', c("2")), 
                        Kjonn=list('item', c("1", "2")), 
                        ContentsCode=list('item', c("Prosent")), 
                        Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
                        Alder=FALSE)

df2 <- as_tibble(df2[[1]])
df2 <- df2 %>% 
  select("value", "år", "kjønn") %>% 
  rename(arbeidsledighet = value)

df <- left_join(df1, df2)
df$år <- df$år %>% 
  as.numeric()



menn <- df %>% 
  filter(kjønn == "Menn")


kvinner <- df %>% 
  filter(kjønn == "Kvinner")


ko1 <- 0.7

plott1<-
  ggplot(menn, aes(x = år)) +
  geom_line(aes(y = sykefravær), color = "green") + 
  geom_line( aes(y = arbeidsledighet/koeff1), color = "orange") +
  scale_y_continuous(
    name = "Prosentvis sykefravær",
    sec.axis = sec_axis(~.*ko1, name="Prosentvis arbeidsledighet")) + 
  theme(
    axis.title.y = element_text(color = "blue", size=10),
    axis.title.y.right = element_text(color = "red", size=10)) +
  ggtitle("Arbeidsledighet og sykefravær for menn")

ko <- 0.5

plott2 <-
  ggplot(kvinner, aes(x = år)) +
  geom_line(aes(y = sykefravær), color = "green") + 
  geom_line( aes(y = arbeidsledighet/koeff), color = "orange") +
  scale_y_continuous(
    name = "Prosentvis sykefravær",
    sec.axis = sec_axis(~.*ko, name="Prosentvis arbeidsledighet")) +
  theme(
    axis.title.y = element_text(color = "blue", size=10),
    axis.title.y.right = element_text(color = "red", size=10)) +
  ggtitle("Arbeidsledighet og sykefravær for kvinner")


plott1+plott2

