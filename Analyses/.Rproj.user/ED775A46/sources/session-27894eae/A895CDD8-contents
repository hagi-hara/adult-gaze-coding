#########################################################
####### Analyzing the Adult Gaze Coding Data ############
#########################################################

# resources (will be removed later)
# https://qiita.com/ocean_f/items/c0bdd1d73fc2a7a78963
# https://analysis-navi.com/?p=553

library(emmeans)   # ver 1.8.3
library(ggeffects) # ver 1.1.4
library(here)      # ver 1.0.1
library(irr)       # ver 0.84.1
library(lme4)      # ver 1.1.31
library(lmerTest)  # ver 3.1.3
library(tidyverse) # ver 1.3.2

# -----------------------------------------------------------------------------------------
# Inter-trial reliability check
d.r <- read.csv(here("Data", "reliability_check.csv"), header=TRUE) %>% 
  mutate(x1 = same_direction,
         x2 = case_when(same_direction=="1" ~ "1", same_direction=="0" ~ "1", T ~ "N/A"))

# Number of frames that were unable to detect face directions
noface <- d.r %>% subset(same_direction=="N/A") %>% nrow()
noface
noface / nrow(d.r) * 100 # 2/840 = 0.24%

# Agreement
d.r <- d.r %>% subset(same_direction!="N/A")
table(d.r$x1, d.r$x2)
agree(cbind(d.r$x1, d.r$x2))
# 836/840 = 99.5%

# -----------------------------------------------------------------------------------------
# Facial detection (iCatcher+)

# Preparation
d.i.n <- read.csv(here("Data", "SampledData", "_data_sampled_NonAnonymized_1.csv"), header=TRUE) %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         face=case_when(annotation==" noface" ~ 0, T~1)) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

d.i.a <- read.csv(here("Data", "SampledData", "_data_sampled_Anonymized_1.csv"), header=TRUE) %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         face=case_when(annotation==" noface" ~ 0, T~1)) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

# Descriptive stats
d.i.n %>% group_by(face) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(face==1) # Anonymisation(-)
d.i.a %>% group_by(face) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(face==1) # Anonymisation(+)

# Regression modeling (Anonymisation(-))
fit.f.i.n <- glmer(face ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.n, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.f.i.n)

res.f.i.n <- ggpredict(fit.f.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.f.i.n <- ggpredict(fit.f.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.f.i.n
res.f.i.n <- res.f.i.n %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.n %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(nface=sum(face),
            nface_prop=nface/1) %>% 
  ggplot(aes(x=Lighting, y=nface_prop))+
  geom_abline(slope=0, intercept=0.5, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.f.i.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of face detected")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
output_fig_dir <- here("Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive=TRUE)
}
ggsave(file=here("Figures", "Figure_3.png"), plot=gp, dpi=350, width=8, height=6.5)


# Regression modeling (Anonymisation(+))
fit.f.i.a <- glmer(face ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.a, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.f.i.a)

res.f.i.a <- ggpredict(fit.f.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.f.i.a <- ggpredict(fit.f.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.f.i.a
res.f.i.a <- res.f.i.a %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.a %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(nface=sum(face),
            nface_prop=nface/1) %>% 
  ggplot(aes(x=Lighting, y=nface_prop))+
  geom_abline(slope=0, intercept=0.5, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.f.i.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of face detected")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S1.png"), plot=gp, dpi=350, width=8, height=6.5)

## Comparison of the proportion of the faces detected for the Non-anonymized and Anonymized datasets
d.i.na <- d.i.n %>% dplyr::select(annotation, confidence, filename, face) %>% 
  left_join(d.i.a, by="filename", suffix=c(".n", ".a")) %>% 
  mutate(face.agr = case_when(face.n - face.a == 0 ~ "0", T~ "1"))

d.i.na %>% group_by(Country) %>% 
  summarize(N = n(),
            Prop.n = sum(face.n)/N,
            Prop.a = sum(face.a)/N)


# -----------------------------------------------------------------------------------------
# Gaze direction classification (iCatcher+)

# Screen size = SMALL (13.3") 

# Preparation
d.i.n <- d.i.n %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

d.i.a <- d.i.a %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

# Descriptive stats
d.i.n %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1)# Anonymisation(-)
d.i.a %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1)# Anonymisation(+)

# Regression modeling (Anonymisation(-))
fit.g.i.n <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.n, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.n)

res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.n
res.g.i.n <- res.g.i.n %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.n %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S2_NonAnonym_Small.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.n %>% subset(Lighting=="Front" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Center")
### Worst condition
d.spec <- d.i.n %>% subset(Lighting=="Left" & Distance=="Middle\n(60 cm)" & Position=="Center" & Rotation=="Left")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.n.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.n.mat

## Summary stats
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S2_NonAnonym_Small_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S2_NonAnonym_Small_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=3.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=7.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_NonAnonym_Small_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_NonAnonym_Small_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


## Confirmation of the chance leve (=33.3%)
# set.seed(123)
# tmp1 <- data.frame(Random=sample(1:3, nrow(tmp1), replace=TRUE)) %>% 
#   cbind(dplyr::select(d.i.n, Actual)) %>%  
#   mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", T~"away"),
#          Y=case_when(Actual==RandomPred~1, T~0)) 
# tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
# tmp1 %>% group_by(RandomPred) %>% summarize(N=n())
# tmp2
# tmp3 <- data.frame()
# for (i in 1:2000){
#   tmp1 <- data.frame(Random=sample(1:3, nrow(tmp1), replace=TRUE)) %>% 
#     cbind(dplyr::select(d.i.n, Actual)) %>%  
#     mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", T~"away"),
#            Y=case_when(Actual==RandomPred~1, T~0)) 
#   tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
#   tmp3 <- rbind(tmp3, c(i, tmp2$N))
# }
# colnames(tmp3) <- c("sim", "value")
# tmp3$value %>% summary()
# 
# tmp3 %>% ggplot(aes(x=value))+
#   geom_histogram(binwidth=0.2, color="white")+
#   geom_density(data=tmp3, eval(bquote(aes(y=..count..*.(0.2)))), color="blue", size=1)+
#   labs(y="Count", x="t value")+
#   theme_bw()


# Regression modeling (Anonymisation(+))
fit.g.i.a <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.a, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.a)

res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.a
res.g.i.a <- res.g.i.a %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.a %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S3_Anonym_Small.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.a %>% subset(Lighting=="Front" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Worst condition
d.spec <- d.i.a %>% subset(Lighting=="Left" & Distance=="Middle\n(60 cm)" & Position=="Center" & Rotation=="Center")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.a.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.a.mat

## Summary stats
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S3_Anonym_Small_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S3_Anonym_Small_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=3.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=7.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_Anonym_Small_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_Anonym_Small_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# -----------------------------------------------------------------------------------------
# Screen size = Medium (18.5") 

# Preparation
d.i.n <- d.i.n %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

d.i.a <- d.i.a %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

# Descriptive stats
d.i.n %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1) # Anonymisation(-)
d.i.a %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1) # Anonymisation(+)

# Regression modeling (Anonymisation(-))
fit.g.i.n <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.n, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.n)

res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.n
res.g.i.n <- res.g.i.n %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.n %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_4_NonAnonym_Medium.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.n %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Center")
### Worst condition
d.spec <- d.i.n %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.n.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.n.mat

## Summary stats
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_4_NonAnonym_Medium_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_4_NonAnonym_Medium_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=2.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=8.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_NonAnonym_Medium_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_NonAnonym_Medium_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# Regression modeling (Anonymisation(+))
fit.g.i.a <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.a, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.a)

res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.a
res.g.i.a <- res.g.i.a %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.a %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S3_Anonym_Medium.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.a %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Left (24 cm)" & Rotation=="Center")
### Worst condition
d.spec <- d.i.a %>% subset(Lighting=="Left" & Distance=="Far\n(90 cm)" & Position=="Center" & Rotation=="Right")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.a.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.a.mat

## Summary stats
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S3_Anonym_Medium_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S3_Anonym_Medium_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=2.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=8.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_Anonym_Medium_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_Anonym_Medium_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)



# -----------------------------------------------------------------------------------------
# Screen size = Large (27.0") 

# Preparation
d.i.n <- d.i.n %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

d.i.a <- d.i.a %>% mutate(tmp=str_replace_all(annotation, c("^ left$"="_right", "^ right$"="_left", "^ away$"="away", "^ noface$"="away")),
                          Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
                          Actual=case_when(trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9~"right", T~"away"),
                          Y=case_when(Actual==Prediction~1, T~0))

# Descriptive stats
d.i.n %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1) # Anonymisation(-)
d.i.a %>% group_by(Y) %>% summarize(N = n(), Prop = N/nrow(d.i.n)) %>% subset(Y==1) # Anonymisation(+)

# Regression modeling (Anonymisation(-))
fit.g.i.n <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.n, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.n)

res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.n <- ggpredict(fit.g.i.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.n
res.g.i.n <- res.g.i.n %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.n %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S2_NonAnonym__Large.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.n %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Center")
### Worst condition
d.spec <- d.i.n %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.n.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.n.mat

## Summary stats
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S2_NonAnonym_Large_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S2_NonAnonym_Large_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=1.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=9.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_NonAnonym_Large_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_NonAnonym_Large_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# Regression modeling (Anonymisation(+))
fit.g.i.a <- glmer(Y ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.i.a, family=binomial,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.i.a)

res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.i.a <- ggpredict(fit.g.i.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.i.a
res.g.i.a <- res.g.i.a %>% mutate(Lighting=x, Distance=group, Rotation=facet, Position=panel)

## Visualization
d.i.a %>% group_by(id, Lighting, Distance, Position, Rotation, trial, filename, Country) %>% 
  summarize(ncrr=sum(Y),
            ncrr_prop=ncrr/1) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(width=0.2, height=0.05, color="gray70", size=0.1, alpha=0.3)+
  geom_pointrange(data=res.g.i.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S3_Anonym_Large.png"), plot=gp, dpi=350, width=8, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.i.a %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Center")
### Worst condition
d.spec <- d.i.a %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Right (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
d.i.a.mat <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% 
  left_join(tmp1, by="Actual") %>% 
  left_join(tmp2, by="Prediction") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.i.a.mat

## Summary stats
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.i.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure)) # overall F measure (averaged)

## Visualization
d.i.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
  geom_tile(aes(fill=Recall))+
  geom_text(aes(label=round(Recall, 2)), size=6)+
  scale_fill_gradient(limits=c(0,1), low="white", high="red", name="Prediction/\nActual")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S3_Anonym_Large_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S3_Anonym_Large_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
d.spec %>% group_by(trial, Prediction) %>% summarize(Prop=round(n()/40, 2)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=1.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=9.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Number plates", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S4_Anonym_Large_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S4_Anonym_Large_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)

