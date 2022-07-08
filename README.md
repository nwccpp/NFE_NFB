### Supplementary Code for:
### Of two minds? How consistency and belonging bias the need for evidence about COVID-19 
#### Authors: Camille Saucier, Erik Nisbet, Ayse Lokmanoglu, R. Kelley Garrett, Graham Dixon, Duane Wegener, Robert Bond, Shelley Hovick, & Kilhoe Na 


The code <https://github.com/nwccpp/NFE_NFB/blob/main/NFE_NFB_Regression_Figures_Code> includes all the steps of the methodology, and the visualizations. 

The data set accompanying the code: <https://github.com/nwccpp/NFE_NFB/blob/main/Covid_Misinfo_Panel_Data_032422.Rda>

For all questions on the paper please contact Camille Saucier camillesaucier2024 [at] u [dot] northwestern [dot] edu.\
For questions on the code please contact Ayse D. Lokmanoglu ayse [dot] lokmanoglu [at] nortwestern [dot] edu.
```{r include=FALSE}
#devtools::install_github("jacob-long/dpm")

library(haven)
library(tidyverse)        # for dplyr, ggplot, etc.
library(panelr)           # to work with panel data
library(here)             # for easy access to subfolders in a project
library(huxtable)         # to make tables (optional)
library(ggeffects)        # to get and plot predictions from models
library(broom)            # to get tidy() functions
library(broom.mixed)      # tidy functions for lme4 objects
library(lme4)             # for mixed models
library(plm)              # for FE models
library(survival)
library(lavaan)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(dpm)
library(modelsummary)
library(texreg)
library(jtools)
library(broom.mixed)
library(interactions)
```

Load the dataset
```{r include=FALSE}

load("Covid_Misinfo_Panel_Data_032422.Rda") # object name should be test
are_varying(test) #check for variation
```

### Random Effect Models
Controls: Age, Education, Gender & Race

```{r include=FALSE}
# Model 1: Accurate Information Endorsement
model_acc_cont<-wbm(accinfow ~  NFE + NFB + ideo + expaccw + 
                      male + reduc + rage + white, 
                    data = test, 
                    model = "random",
                    interaction.style = "demean",
                    use.wave = TRUE,
                    wave.factor = TRUE)
summary(model_acc_cont)

# Model 2: Misinformation Endorsement
model_mis_cont<-wbm(misinfow ~  NFE + NFB + ideo + expmisinfow +
                      male + reduc + rage + white, 
                    data = test, 
                    model = "random",
                    interaction.style = "demean",
                    use.wave = TRUE,
                    wave.factor = TRUE)
summary(model_mis_cont)
```

### Two-way Interaction Models:
Controls: Age, Education, Gender & Race\
Interaction: NFE and Ideology
```{r include=FALSE}
# Model 3: Accurate Information Endorsement: NFE*Ideology
model_acc_int_1<-wbm(accinfow ~  NFE + NFB + ideo + expaccw +
                       male + reduc + rage + white |
                       NFE*ideo, 
                     data = test, 
                     model = "random",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
summary(model_acc_int_1)

# Model 4: Misinformation Endorsement: NFE*Ideology
model_mis_int_1<-wbm(misinfow ~  NFE + NFB + ideo + expmisinfow +
                       male + reduc + rage + white |
                       NFE*ideo,
                     data = test, 
                     model = "random",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
summary(model_mis_int_1)
```


### Three way interaction models
Controls: Age, Education, Gender & Race\
Interaction: NFE, Ideology and NFB
```{r include=FALSE}
# Model 5: Accurate Information Endorsement: NFE*Ideology*NFB
model_acc_cont_int<-wbm(accinfow ~  NFE + NFB + ideo + expaccw + 
                          male + reduc + rage + white |
                          NFE*ideo + ideo*NFB + NFE*NFB + NFE*ideo*NFB, 
                        data = test, 
                        model = "random",
                        interaction.style = "demean",
                        use.wave = TRUE,
                        wave.factor = TRUE)
summary(model_acc_cont_int)

# Model 6: Misinformation Endorsement: NFE*Ideology*NFB
model_mis_cont_int<-wbm(misinfow ~  NFE + NFB + ideo + expmisinfow +
                          male + reduc + rage + white |
                          NFE*ideo + ideo*NFB + NFE*NFB + NFE*ideo*NFB, 
                        data = test, 
                        model = "random",
                        interaction.style = "demean",
                        use.wave = TRUE,
                        wave.factor = TRUE)
summary(model_mis_cont_int)
```

Print tables for publication
```{r include=FALSE}
# package modelsummary
# first make a list of all the models, and the relabel the models
models_all <- list('Misinformation Endorsement' = model_mis_cont,
                   'Accurate Information Endorsement' = model_acc_cont,
                   "Misinformation Endorsement Int" = model_mis_int_1,
                   "Accurate Information Endorsement Int" = model_acc_int_1,
                   'Misinformation Endorsement  3-Way Interaction' = model_mis_cont_int,
                   'Accurate Information Endorsement 3-Way Interaction' = model_acc_cont_int)

# relabel coefficients
coef_all<- c("rage" = "Age",
             "reduc" = "Education",
             "male" = "Gender",
             "white" = "Race",
             "expaccw" = "Exposure to Accurate Information",
             "expmisinfow" = "Exposure to Misinformation",
             "NFE" = "NFE",
             "ideo" = "Political Ideology",
             "NFB" = "NFB",
             "NFE:ideo" = "NFE * Ideology",
             "NFE:NFB" = "NFB * NFE",
             "ideo:NFB" = "Ideology * NFB",
             "NFE:ideo:NFB" = "NFE * Ideology * NFB")
# print for publication
modelsummary(models_all,
             coef_map = coef_all,
             fmt = 4,
             estimate  = c( "{estimate}{stars}"),
             statistic = c("p = {p.value}"),
             coef_omit = "Intercept",
             output = "Results/replication_all_41122.docx")

``` 
### Codes for Figures 2,3 and Supplement
Install Packages
```{r include=FALSE}
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(ggeffects)
library(lubridate)
library(scales)
options(dplyr.summarise.inform = FALSE)
dev.off()
```

Figure 2 and 3: Three-Way Interaction Models 
```{r include=FALSE}
##create the labeller
NFB_names <- list(
  'NFB = 1'="NFB (low)",
  'NFB = 2'="NFB (mean)",
  'NFB = 3'="NFB (high)")
# write labeler functions
nfb_labeller <- function(variable,value){
  return(NFB_names[value])
}
```
Figure 2: Accurate Information Endorsement: NFE x Ideology x NFB
```{r include=FALSE}
# use model 5
ACC_int <- ggpredict(model_acc_cont_int, terms = c("NFE", "ideo", "NFB"))
# factor the groups with labels
ACC_int$facet <- factor(ACC_int$facet, levels = c("1.27", "2.22", "3.17"), 
                        labels = c("Low", "Mean", "High"))
ACC_int$group <- factor(ACC_int$group, 
                        levels = c("1.93", "3.73", "5.53"), ###1 sd below and above
                        labels = c("Liberal", "Moderate", "Conservative"))
# set colors
ideo_col<-c("#5F5647", "#9B110E", "#3F5151")

# graph
ggplot(ACC_int, aes(x = x, y = predicted, colour = group, linetype = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet, ncol = 3, labeller=nfb_labeller) +
  ylab("Mean endorsement of\naccurate claims") +
  xlab("Need for Evidence") +
  scale_y_continuous(#limits = c(1, 6), 
    breaks = seq(1, 6, by = 0.05)) +
  scale_x_continuous(limits= c(3, 5),
                     breaks = c(3.307, 4.017, 4.727), 
                     labels = c("Low", "Mean", "High")) +
  scale_color_manual(values = ideo_col) +
  theme_wsj(color = "white")+
  theme(text=element_text(size=10,family="sans"),
        title=element_text(size=10,family="sans"),
        axis.text.x=element_text(angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(size=10, family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="sans"),
        legend.position="bottom", legend.box="horizontal", legend.margin=margin(),
        legend.key = element_rect(fill="white"), 
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        legend.text=element_text(size=10, family="sans")) 
ggsave("Figures_NFE_NFB/Figure3_NFE_NFB_Accurate_Endorsement.tiff", width=8.5, height=5, dpi=300)

```
Figure 3: Misinformation Endorsement: NFE x Ideology x NFB
```{r include=FALSE}
# use model 6
MIS_int <- ggpredict(model_mis_cont_int, terms = c("NFE", "ideo", "NFB"))
##factor the groups with labels
MIS_int$facet <- factor(MIS_int$facet, levels = c("1.27", "2.22", "3.17"), 
                        labels = c("Low", "Mean", "High"))
MIS_int$group <- factor(MIS_int$group, 
                        levels = c("1.93", "3.73", "5.53"),
                        labels = c("Liberal", "Moderate", "Conservative"))

# set colors
ideo_col<-c("#5F5647", "#9B110E", "#3F5151")

# plot it
ggplot(MIS_int, aes(x = x, y = predicted, colour = group, linetype = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet, ncol = 3, labeller=nfb_labeller) +
  ylab("Mean endorsement of\nfalse/misleading claims") +
  xlab("Need for Evidence") +
  scale_y_continuous(#limits = c(1, 6), 
    breaks = seq(1, 6, by = 0.2)) +
  scale_x_continuous(limits= c(3, 5),
                     breaks = c(3.307, 4.017, 4.727), ###1 sd below and above
                     labels = c("Low", "Mean", "High")) +
  scale_color_manual(values = ideo_col) +
  theme_wsj(color = "white")+
  theme(text=element_text(size=10,family="sans"),
        title=element_text(size=10,family="sans"),
        axis.text.x=element_text(angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(size=10, family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="sans"),
        legend.position="bottom", legend.box="horizontal", legend.margin=margin(),
        legend.key = element_rect(fill="white"), 
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        legend.text=element_text(size=10, family="sans"))
ggsave("Figures_NFE_NFB/Figure2_NFE_NFB.tiff", width=8.5, height=5, dpi=300)
``` 
Figure Supplement: Mean Endorsements
```{r include=FALSE}
# upload mean endorsements
all_long <- readxl::read_excel("CSV/Mean_Endorsements.xlsx", 
                       sheet = "Sheet3")
##Change it with lubridate
all_long$Date<-ymd(all_long$Date)
all_long<- all_long %>%
  mutate(Variable = recode(Variable,
                           'FALSE' = 'False'))
pd <- position_dodge(0.2)

ggplot(all_long, aes(x=Date, y=Mean, colour=Variable)) + 
  facet_wrap(~Group,
             nrow=2,
             scales = "free_y")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), 
                position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  geom_text(aes(label = round(Mean, 1)),
            vjust = "outward", hjust = "inward",
            size = 4,
            colour = "black",
            show.legend = FALSE) +
  scale_x_date(date_breaks="1 month", date_labels = "%b-%Y")+
  scale_color_manual(values = wes_palette('BottleRocket1')) +
  #scale_linetype_manual(values = c("solid", "F1")) +
  scale_y_continuous(breaks=pretty_breaks())+
  labs(x="Survey Wave", 
       y="Mean")+
  theme_wsj(color="white") +
  theme(axis.text.x=element_text(size=10, family="sans"),
        axis.text.y=element_text(family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=10, family="sans"),
        legend.position="bottom", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())

ggsave("Figures_NFE_NFB/Figure1_Mean_Endorsement.tiff", width=8.5, height=5, dpi=300)
```
