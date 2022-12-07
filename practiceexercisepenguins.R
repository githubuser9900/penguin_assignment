# -- PENGUINS EXERCISE
# -- script created:05/12/2022
# -- author: -
# -- last edited:07/12/2022

# housekeeping ------------------------------------------------------------

library(ggplot2)
library(palmerpenguins)
library(dplyr)
library(janitor)
library(tidyr)
library(ggsignif)

# import and clean --------------------------------------------------------


cleaning2 <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)%>%
    drop_na()
}
#build a function to clean our data, drop na's, improve the naming convention

cleanpen<-cleaning2(penguins_raw)
#use it on our data

# stats test --------------------------------------------------------------

#want to use t test to see if the difference in mean body mass between two species of penguin is statistically significant

slimpen<-subset(cleanpen, select=c("species", "body_mass_g"))
slimpen <- slimpen[-c(265:333),]
#extracts just the species and body mass, removes all chinstrap

summary(slimpen)

ggplot(slimpen) +
  aes(x = species, y = body_mass_g, color = species) +
  geom_jitter() +
  theme(legend.position = "none")
#visual check - loooks good! 
#we have >30 samples in each group so no need to check for normal distribution

boxplot(body_mass_g ~ species,data = slimpen)
#checking the variances - relatively similar so should be fine to do a standard t

group_by(slimpen, species) %>%
  summarise(
    mean = mean(body_mass_g),
    sd = sd(body_mass_g)
  )
#gives us mean and sd for both - adelie and gentoo distant

test <-t.test(body_mass_g ~ species, data = slimpen, var.equal = TRUE)
test
#indicates statistically significant difference between means as p <0.05



# graphing the stats test -------------------------------------------------

#using scatter plots to graph the test

jpeg(file="violinplot.jpeg")
#opens a jpeg to save into

scplot<-ggplot(slimpen,aes(x=species,
                   y=body_mass_g,
                   fill=species))+
  geom_violin()+
  theme(legend.position="none")+
  geom_signif(comparisons = list(c("Gentoo penguin (Pygoscelis papua)", "Adelie Penguin (Pygoscelis adeliae)")), 
              map_signif_level=TRUE,
              test='t.test')+
  stat_summary(geom = "errorbar",
    fun.data = "mean_se")+
  scale_color_brewer(palette="Dark2")
scplot
#created violin plot with mean + se bars, significance comparison for difference!

dev.off()
#closes jpeg




