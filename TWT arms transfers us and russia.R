library(readr)
library(ggplot2) 
library(dplyr)
library(ggarrange)
library(ggforce)
library(plyr)
library(extrafont)
library(ggpubr)
library(countrycode)
library(ggflags)

loadfonts()

######## Load and clean data files from https://www.sipri.org/databases/armstransfers
# US
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-USA-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "USA"
d_usa <- d
# Russia
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-RUS-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Russia"
d_rus <- d
# France
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-FRA-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "France"
d_fra <- d
# China
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-CHI-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "China"
d_chi <- d
# Germany
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-FRG-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Germany"
d_ger <- d
# UK
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-UK-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "United Kingdom"
d_uk <- d
# Italy
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-ITA-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Italy"
d_ita <- d
# Spain
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-SPA-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Spain"
d_spa <- d
# Israel
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-ISR-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Israel"
d_isr <- d
# Netherlands
d <- read_csv("~/Desktop/Research non-PHD/Twitter Graphs/#7 arms exports/countries/TIV-Export-NET-2011-2021.csv")
d <- d[c(11:(nrow(d)-1)),c(1,13)]
colnames(d)<-c("recipient","arms")
d$sender <- "Netherlands"
d_net <- d
# Combine
d <- rbind.fill(d_usa,
                d_rus,
                d_fra,
                d_chi,
                d_ger,
                d_ita,
                d_spa,
                d_uk,
                d_isr,
                d_net)
d$arms <- as.numeric(d$arms)
d$continent <- countrycode(d$recipient,"country.name","continent")


##################### FIGURE ##################### 
# Prepare
d1 <- d %>% 
  group_by(sender) %>%
  arrange(-arms) %>%
  slice(1:50)
d1 <- d1 %>% 
  gather_set_data(c(1,3))
d1$country_code <- tolower(countrycode(d1$recipient,"country.name","iso2c"))
d1$region <- countrycode(d1$recipient,"country.name","region")
d1 %>% group_by(sender) %>%
  dplyr::summarize(sum=sum(arms,na.rm=T))

# world total 2011-2021: 308614
# US: 35% (107401/tot). 98.7% (105954/us tot) went to these 50 countries
# Rus: 22% 99.7% 

cols <- c("#8b1f57","#F1cd6e","#f18c5d","#df2e46","#dfdde4") # inferno style

# Plot RUSSIA
d_ru <- d1%>%filter(sender=="Russia")
d_ru$recipient <- car::recode(d_ru$recipient,"'Ukraine Rebels*'='Ukraine Rebels'")
d_ru$y <- car::recode(d_ru$y,"'Ukraine Rebels*'='Ukraine Rebels'")
d_ru$x <- factor(d_ru$x,levels(factor(d_ru$x))[c(2,1)]) # Mirror of US
aw <- 0.05
sp <- 0.05
fig<-ggplot(d_ru,
            aes(x=x,
                split=reorder(y,-arms),
                value=arms,
                id=id)) +
  geom_parallel_sets(aes(fill=continent),
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(fill = "grey30",
                          axis.width = aw, sep = sp) + 
  geom_parallel_sets_labels(angle=0,hjust=c(rep(1,1), rep(0, 50)),
                            axis.width = aw, sep = sp,
                            position = position_nudge(x = c(rep(-0.05,2), rep(.05, 200))))
# Extract label positions
df<-data.frame(ggplot_build(fig)$data[3]) %>% 
  mutate(country_code=tolower(countrycode(label,"country.name","iso2c")),
         lab_pos=y,
         x="recipient") %>%
  dplyr::select(country_code,lab_pos,x)
d_ru <- merge(d_ru,df,by=c("country_code","x"),all.x = T)
# plot again
fig_ru<-ggplot(d_ru,
               aes(x=x,
                   split=reorder(y,-arms),
                   value=arms,
                   id=id)) +
  geom_parallel_sets(aes(fill=continent),
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(fill = "grey40",
                          axis.width = aw, sep = sp) + 
  geom_parallel_sets_labels(angle=0,hjust=c(rep(1,1), rep(0, 50)),
                            axis.width = aw, sep = sp,
                            size=5.5,
                            position = position_nudge(x=0.125),
                            color="white",
                            family="Gill Sans MT") +
  scale_fill_manual(values=cols) +
  geom_flag(aes(country=country_code,y=lab_pos,size=arms),
            position = position_nudge(x=0.05)) +
  scale_size_continuous(range=c(5,22)) +
  scale_y_continuous(limits=c(0,270000)) +
  theme_void() +
  theme(panel.background = element_rect(fill="grey15",color="grey15"),
        plot.background = element_rect(fill="grey15",color="grey15"),
        legend.position = "none") +
  geom_point(aes(x="sender",y=34065),size=73) +
  geom_flag(aes(country="ru",x="sender",y=34065),size=63) +
  coord_cartesian(xlim=c(1.3,2)) +
  geom_text(aes(x=0.8,y=140000,
                label="Russia is the world's 
second largest arms 
exporter, accounting for 
22% of global exports 
over these 10 years 
(99% of which went to 
the countries listed here)"),color="grey70",hjust=0,family="Gill Sans MT",vjust=1) 

#### Plot US
d_us <- d1%>%filter(sender=="USA")
aw <- 0.05
sp <- 0.03
fig<-ggplot(d_us%>%filter(!is.na(continent)),
            aes(x=x,
                split=reorder(y,-arms),
                value=arms,
                id=id)) +
  geom_parallel_sets(aes(fill=continent),
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(fill = "grey40",
                          axis.width = aw, sep = sp) + 
  geom_parallel_sets_labels(angle=0,hjust=c(rep(0, 50)),
                            axis.width = aw, sep = sp,
                            position = position_nudge(x = -0.07)) +
  scale_fill_manual(values=cols) +
  theme(legend.background = element_rect(fill="grey15",color="grey50"),
        legend.text = element_text(color="white",size=11),
        legend.title = element_blank())
# Extract label positions
df<-data.frame(ggplot_build(fig)$data[3]) %>% 
  mutate(country_code=tolower(countrycode(label,"country.name","iso2c")),
         lab_pos=y,
         x="recipient") %>%
  dplyr::select(country_code,lab_pos,x)
d_us <- merge(d_us,df,by=c("country_code","x"),all.x = T)
# plot again
fig_us<-ggplot(d_us%>%filter(!is.na(continent)),
               aes(x=x,
                   split=reorder(y,-arms),
                   value=arms,
                   id=id)) +
  geom_parallel_sets(aes(fill=continent),
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(fill = "grey40",
                          axis.width = aw, sep = sp) + 
  geom_parallel_sets_labels(angle=0,hjust=1,
                            axis.width = aw, sep = sp,
                            position = position_nudge(x = -0.125),
                            size=5.5,
                            color="white",
                            family="Gill Sans MT") +
  scale_fill_manual(values=cols) +
  geom_flag(aes(country=country_code,y=lab_pos,size=arms),
            position = position_nudge(x=-0.05)) +
  scale_size_continuous(range=c(5,22)) +
  scale_y_continuous(limits=c(0,270000)) +
  theme_void() +
  theme(panel.background = element_rect(fill="grey15",color="grey15"),
        plot.background = element_rect(fill="grey15",color="grey15"),
        legend.position = "none") +
  geom_point(aes(x="sender",y=53000),size=73) +
  geom_flag(aes(country="us",x="sender",y=53000),size=63) +
  geom_text(aes(x=2.2,y=140000, family="Gill Sans MT",vjust=1,
                label="The US is the world's 
                largest arms exporter, 
                accounting for 35% of 
                global exports over 
                these 10 years (99% of 
                which went to the 
                countries listed here)"),color="grey70",hjust=1) +
  coord_cartesian(xlim=c(1,1.7))

leg<-get_legend(fig) # extract legend

fig_both<-ggarrange(fig_us,fig_ru) + 
  geom_text(aes(label="U.S. AND RUSSIAN
ARMS TRANSFERS",x=0.5,y=0.83),
            lineheight=0.9,
            family="Gill Sans MT",
            fontface="bold",
            size=21,
            color="grey80") +
  annotation_custom(leg,
                    xmin=0.5,
                    xmax=0.5,
                    ymin=0.6,
                    ymax=0.6) +
  geom_rect(aes(xmin=0.34,
                xmax=0.46,
                ymin=0.565,
                ymax=0.635),
            color="grey50",
            fill="grey15") +
  geom_text(aes(x=0.4,y=0.6,label="The width of each line 
represents the size 
of the arms transfer"),
            family="Gill Sans MT",
            size=6,
            color="grey70") +
  geom_rect(aes(xmin=0.54,
                xmax=0.64,
                ymin=0.565,
                ymax=0.635),
            color="grey50",
            fill="grey15") +
  geom_text(aes(x=0.59,y=0.6,label="Recipients are 
ranked from 
largest to smallest"),
            family="Gill Sans MT",
            size=6,
            color="grey70") +
  geom_text(aes(label="The graph shows the top 50 recipients of arms from the United States 
and Russia in the period 2011-2021. The size of transfers are measured 
in terms of military value. Source: SIPRI Arms Transfers Database",x=0.5,y=0.75),
            lineheight=1,
            family="Gill Sans MT",
            size=6,
            color="grey70") +
  geom_text(aes(label="How to read the graph:",x=0.5,y=0.65),
            lineheight=1,
            family="Gill Sans MT",
            size=6,
            color="grey70") +
  geom_text(aes(label="Twitter: @rubenbmathisen",x=0.75,y=0.03),
            lineheight=1,
            family="Gill Sans MT",
            size=6,
            color="grey70")

ggsave(file="TWT arms exports both 2.jpeg",
       plot = fig_both,
       width = 20, height = 20,
       dpi=300)
