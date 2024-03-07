#########################################################
####### 4. Gaze Direction (OWLET) #######################
#########################################################

coef.o <- data.frame()

# -----------------------------------------------------------------------------------------
# Screen size = SMALL (13") 
# -----------------------------------------------------------------------------------------

# Preparation
d.o.n.2 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.n.3 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

d.o.a.2 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.a.3 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==4|trial==5~"left", trial==6|trial==7~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

# Descriptive stats
d.o.n %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 13254
d.o.a %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 4874

d.o.n %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 40.1% excluded
d.o.a %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 15.1% excluded

d.o.n.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Non-Anonymized
d.o.a.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Anonymized


# -----------------------------------------------------------------------------------------
# Regression modeling (Non-Anonymized)

fit.g.o.n <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.n.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.n)
Anova(fit.g.o.n)
tmp <- summary(fit.g.o.n)$coefficients %>% as.data.frame() %>% mutate(Monitor="Small", Anonym="Non-anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.n
res.g.o.n <- as.data.frame(res.g.o.n) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Confirmation of the chance level (=SMALL)
set.seed(123)
tmp1 <- data.frame(Random=sample(1:3, nrow(d.o.n.2), replace=TRUE)) %>%
  cbind(dplyr::select(d.o.n.2, Actual)) %>%
  mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
         Y=case_when(Actual==RandomPred~1, T~0))
tmp1 %>% group_by(RandomPred) %>% summarize(Prop=n()/nrow(tmp1))
tmp1 %>% group_by(Actual) %>% summarize(Prop=n()/nrow(tmp1))
tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
tmp2
tmp3 <- data.frame()
for (i in 1:2000){
  tmp1 <- data.frame(Random=sample(1:3, nrow(tmp1), replace=TRUE)) %>%
    cbind(dplyr::select(d.o.n.2, Actual)) %>%
    mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
           Y=case_when(Actual==RandomPred~1, T~0))
  tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
  tmp3 <- rbind(tmp3, c(i, tmp2$N))
}
colnames(tmp3) <- c("sim", "value")
tmp3$value %>% summary() # chance level = 0.33

tmp3 %>% ggplot(aes(x=value))+
  geom_histogram(binwidth=0.02, bins=10, color="white")+
  geom_density(data=tmp3, eval(bquote(aes(y=..count..*.(0.006)))), color="blue", linewidth=1)+
  scale_x_continuous(limits=c(0,1))+
  labs(y="Count", x="t value")+
  theme_bw()

## Visualization
d.o.n.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Non-anonymized, Small monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Small.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.n.3 %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Right")
### Worst condition
d.spec <- d.o.n.3 %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Center")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.n.mat <- tmp3 %>%
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.n.mat

## Summary stats
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Small_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Small_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>% 
  rbind(data.frame(trial=1, Prediction="away", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=3.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=7.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Small_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Small_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# -----------------------------------------------------------------------------------------
# Regression modeling (Anonymized)

fit.g.o.a <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.a.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.a)
Anova(fit.g.o.a)
tmp <- summary(fit.g.o.a)$coefficients %>% as.data.frame() %>% mutate(Monitor="Small", Anonym="Anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.a
res.g.o.a <- as.data.frame(res.g.o.a) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Visualization
d.o.a.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Anonymized, Small monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Small.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.a.3 %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Right")
### Worst condition
d.spec <- d.o.a.3 %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Center")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.a.mat <- tmp3 %>%
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.a.mat

## Summary stats
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Small_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Small_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>% 
  rbind(data.frame(trial=1, Prediction="away", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=3.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=7.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Small_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Small_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)



# -----------------------------------------------------------------------------------------
# Screen size = Medium (16") 
# -----------------------------------------------------------------------------------------

# Preparation
d.o.n.2 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.n.3 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

d.o.a.2 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.a.3 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

# Descriptive stats
d.o.n %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 13254
d.o.a %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 4874

d.o.n %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 40.1% excluded
d.o.a %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 15.1% excluded

d.o.n.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Non-Anonymized
d.o.a.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Anonymized


# -----------------------------------------------------------------------------------------
# Regression modeling (Non-Anonymized)

fit.g.o.n <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.n.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.n)
Anova(fit.g.o.n)
tmp <- summary(fit.g.o.n)$coefficients %>% as.data.frame() %>% mutate(Monitor="Medium", Anonym="Non-anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.n
res.g.o.n <- as.data.frame(res.g.o.n) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Confirmation of the chance level (=MEDIUM)
set.seed(123)
tmp1 <- data.frame(Random=sample(1:3, nrow(d.o.n.2), replace=TRUE)) %>%
  cbind(dplyr::select(d.o.n.2, Actual)) %>%
  mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
         Y=case_when(Actual==RandomPred~1, T~0))
tmp1 %>% group_by(RandomPred) %>% summarize(Prop=n()/nrow(tmp1))
tmp1 %>% group_by(Actual) %>% summarize(Prop=n()/nrow(tmp1))
tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
tmp2
tmp3 <- data.frame()
for (i in 1:2000){
  tmp1 <- data.frame(Random=sample(1:3, nrow(tmp1), replace=TRUE)) %>%
    cbind(dplyr::select(d.o.n.2, Actual)) %>%
    mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
           Y=case_when(Actual==RandomPred~1, T~0))
  tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
  tmp3 <- rbind(tmp3, c(i, tmp2$N))
}
colnames(tmp3) <- c("sim", "value")
tmp3$value %>% summary() # chance level = 0.33

tmp3 %>% ggplot(aes(x=value))+
  geom_histogram(binwidth=0.02, bins=10, color="white")+
  geom_density(data=tmp3, eval(bquote(aes(y=..count..*.(0.006)))), color="blue", linewidth=1)+
  scale_x_continuous(limits=c(0,1))+
  labs(y="Count", x="t value")+
  theme_bw()

## Visualization
d.o.n.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Non-anonymized, Medium monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_4_OWLET_NonAnonym_Medium.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.n.3 %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Left")
### Worst condition
d.spec <- d.o.n.3 %>% subset(Lighting=="Right" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.n.mat <- tmp3 %>%
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.n.mat 

## Summary stats
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_4_OWLET_NonAnonym_Medium_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_4_OWLET_NonAnonym_Medium_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>%
  # rbind(data.frame(trial=1, Prediction="left", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=2.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=8.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Medium_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Medium_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# -----------------------------------------------------------------------------------------
# Regression modeling (Anonymized)

fit.g.o.a <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.a.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.a)
Anova(fit.g.o.a)
tmp <- summary(fit.g.o.a)$coefficients %>% as.data.frame() %>% mutate(Monitor="Medium", Anonym="Anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.a
res.g.o.a <- as.data.frame(res.g.o.a) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Visualization
d.o.a.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Anonymized, Medium monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Medium.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.a.3 %>% subset(Lighting=="Front" & Distance=="Close\n(30 cm)" & Position=="Center" & Rotation=="Left")
### Worst condition
d.spec <- d.o.a.3 %>% subset(Lighting=="Left" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.a.mat <- tmp3 %>%
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.a.mat

## Summary stats
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Medium_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Medium_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>% 
  rbind(data.frame(trial=1, Prediction="away", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=2.5, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=8.5, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Medium_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Medium_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)



# -----------------------------------------------------------------------------------------
# Screen size = Large (32") 
# -----------------------------------------------------------------------------------------

# Preparation
d.o.n.2 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==1|trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9|trial==10~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.n.3 <- d.o.n %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==1|trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9|trial==10~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

d.o.a.2 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(N.all=right+left+away) %>% 
  pivot_longer(c(away, right, left, noface), names_to="Category", values_to="N.correct") %>% 
  mutate(tmp=str_replace_all(Category, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==1|trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9|trial==10~"right", T~"away"),
         Prop=N.correct/N.all) %>% 
  subset(Actual==Prediction) %>% select(-tmp, -Category)

d.o.a.3 <- d.o.a %>% subset(!Prop.face==0) %>% 
  mutate(tmp=str_replace_all(Annotation, c("^left$"="_right", "^right$"="_left", "^away$"="away", "^noface$"="noface")),
         Prediction=str_replace_all(tmp, c("^_left$"="left", "^_right$"="right")),
         Actual=case_when(trial==1|trial==2|trial==3|trial==4|trial==5~"left", trial==6|trial==7|trial==8|trial==9|trial==10~"right", T~"away"),
         Y=case_when(Actual==Prediction~1, T~0)) %>% select(-tmp)

# Descriptive stats
d.o.n %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 13254
d.o.a %>% subset(Prop.face==0) %>% select(filename) %>% nrow() # 4874

d.o.n %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 40.1% excluded
d.o.a %>% mutate(Face=case_when(Prop.face==0 ~ "Failure", TRUE ~ "Success")) %>% select(Face) %>% table() # 15.1% excluded

d.o.n.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Non-Anonymized
d.o.a.2 %>% summarize(N = n(), M.Prop = mean(Prop), SD.Prop=sd(Prop)) # Anonymized


# -----------------------------------------------------------------------------------------
# Regression modeling (Non-Anonymized)

fit.g.o.n <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.n.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.n)
Anova(fit.g.o.n)
tmp <- summary(fit.g.o.n)$coefficients %>% as.data.frame() %>% mutate(Monitor="Large", Anonym="Non-anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.n <- ggpredict(fit.g.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.n
res.g.o.n <- as.data.frame(res.g.o.n) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Confirmation of the chance level (=LARGE)
set.seed(123)
tmp1 <- data.frame(Random=sample(1:3, nrow(d.o.n.2), replace=TRUE)) %>%
  cbind(dplyr::select(d.o.n.2, Actual)) %>%
  mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
         Y=case_when(Actual==RandomPred~1, T~0))
tmp1 %>% group_by(RandomPred) %>% summarize(Prop=n()/nrow(tmp1))
tmp1 %>% group_by(Actual) %>% summarize(Prop=n()/nrow(tmp1))
tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
tmp2
tmp3 <- data.frame()
for (i in 1:2000){
  tmp1 <- data.frame(Random=sample(1:3, nrow(tmp1), replace=TRUE)) %>%
    cbind(dplyr::select(d.o.n.2, Actual)) %>%
    mutate(RandomPred=case_when(Random=="1"~"right", Random=="2"~"left", TRUE~"away"),
           Y=case_when(Actual==RandomPred~1, T~0))
  tmp2 <- tmp1 %>% summarize(N=sum(Y)/nrow(tmp1))
  tmp3 <- rbind(tmp3, c(i, tmp2$N))
}
colnames(tmp3) <- c("sim", "value")
tmp3$value %>% summary() # chance level = 0.33

tmp3 %>% ggplot(aes(x=value))+
  geom_histogram(binwidth=0.02, bins=10, color="white")+
  geom_density(data=tmp3, eval(bquote(aes(y=..count..*.(0.006)))), color="blue", linewidth=1)+
  scale_x_continuous(limits=c(0,1))+
  labs(y="Count", x="t value")+
  theme_bw()

## Visualization
d.o.n.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Non-anonymized, Large monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Large.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.n.3 %>% subset(Lighting=="Front" & Distance=="Middle\n(60 cm)" & Position=="Center" & Rotation=="Center")
### Worst condition
d.spec <- d.o.n.3 %>% subset(Lighting=="Left" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.n.mat <- tmp3 %>% subset(Actual!="away") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.n.mat

## Summary stats
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.n.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.n.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Large_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S4_OWLET_NonAnonym_Large_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>% 
  rbind(data.frame(trial=1, Prediction="away", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=1, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=10, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Large_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_NonAnonym_Large_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)


# -----------------------------------------------------------------------------------------
# Regression modeling (Anonymized)

fit.g.o.a <- glmer(cbind(N.correct, N.all-N.correct) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.a.2, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.g.o.a)
Anova(fit.g.o.a)
tmp <- summary(fit.g.o.a)$coefficients %>% as.data.frame() %>% mutate(Monitor="Large", Anonym="Anonymized") %>% 
  rownames_to_column(var="Term")
coef.o <- coef.o %>% rbind(tmp)

res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.g.o.a <- ggpredict(fit.g.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.g.o.a
res.g.o.a <- as.data.frame(res.g.o.a) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Visualization
d.o.a.2 %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(ncrr_prop=mean(Prop)) %>% ungroup() %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=ncrr_prop))+
  geom_abline(slope=0, intercept=1/3, linetype="dashed", color="gray30", lwd=0.5)+
  # geom_abline(slope=0, intercept=0.33, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.g.o.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Anonymized, Large monitor size)", x="Lighting", y="Proportion of correct prediction")+
  theme_bw()+
  facet_grid(Distance~Position)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Large.png"), plot=gp, dpi=350, width=7.7, height=6.5)

## Confusion matrix
### Best condition 
d.spec <- d.o.a.3 %>% subset(Lighting=="Front" & Distance=="Middle\n(60 cm)" & Position=="Center" & Rotation=="Center")
### Worst condition
d.spec <- d.o.a.3 %>% subset(Lighting=="Left" & Distance=="Far\n(90 cm)" & Position=="Left (24 cm)" & Rotation=="Right")
### Creating Confusion matrix
tmp0 <- d.spec %>% group_by(Actual, Prediction) %>% summarize(N=n()) %>% ungroup() %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>% select(-Actual, -Prediction)
tmp1 <- d.spec %>% group_by(Actual) %>% summarize(Nact=n())
tmp2 <- d.spec %>% group_by(Prediction) %>% summarize(Npred=n())
tmp3 <- expand.grid(Actual=c("away", "left", "right"), Prediction=c("away", "left", "right")) %>% 
  mutate(Tag=paste(Actual, Prediction, sep="_")) %>%  
  left_join(tmp0, by="Tag") %>% select(-Tag) %>% left_join(tmp1, by="Actual") %>% left_join(tmp2, by="Prediction")
tmp3[is.na(tmp3)] <- 0
d.o.a.mat <- tmp3 %>% subset(Actual!="away") %>% 
  mutate(Recall=N/Nact,
         Precision=N/Npred,
         Fmeasure=(2*Recall*Precision)/(Recall+Precision))
d.o.a.mat

## Summary stats
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Accuracy=sum(N)/sum(Nact)) # overall accuracy
d.o.a.mat %>% subset(Actual==Prediction) %>% ungroup() %>% summarize(Fmean=mean(Fmeasure, na.rm=TRUE)) # overall F measure (averaged)

## Visualization
d.o.a.mat %>% ggplot(aes(x=Prediction, y=Actual))+
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
# ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Large_CM_Best.png"), plot=gp, dpi=350, width=4.2, height=3)
ggsave(file=here("Figures", "Figure_S5_OWLET_Anonym_Large_CM_Worst.png"), plot=gp, dpi=350, width=4.2, height=3)

## Visualization: Prediction-Number correspondence
tmp <- d.spec %>% group_by(trial, Actual) %>% summarize(N=n())
d.spec %>% group_by(trial, Prediction) %>% summarize(Y.sum=n()) %>% left_join(tmp, by="trial") %>% 
  mutate(Prop=round(Y.sum/N, 2)) %>% 
  rbind(data.frame(trial=1, Prediction="away", Y.sum=0, Actual="NA", N=0, Prop=-0.5)) %>% 
  ggplot(aes(x=trial, y=Prop, color=Prediction, shape=Prediction))+
  annotate("rect", xmin=1, xmax=5.5, ymin=0, ymax=1, fill="#358688", alpha=0.2, color="white")+
  annotate("rect", xmin=5.5, xmax=10, ymin=0, ymax=1, fill="#B73E35", alpha=0.2, color="white")+
  geom_abline(slope=0, intercept=0.333, linetype="dashed", color="gray30", lwd=0.5)+
  geom_point(size=3)+
  geom_line(aes(group=Prediction), size=1)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  labs(x="Numbered discs", y="Proportion of prediction")+
  theme_bw()+
  theme(axis.ticks=element_line(color = "black"),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Large_Best.png"), plot=gp, dpi=350, width=5.2, height=4)
ggsave(file=here("Figures", "Figure_S7_OWLET_Anonym_Large_Worst.png"), plot=gp, dpi=350, width=5.2, height=4)



# -----------------------------------------------------------------------------------------
# Summary figure
# -----------------------------------------------------------------------------------------

coef.o %>% mutate(Term=gsub(Term, pattern="\n", replacement="", ignore.case=TRUE),
                  Term=gsub(Term, pattern=" ", replacement="", ignore.case=TRUE),
                  Term=gsub(Term, pattern="\\(", replacement="", ignore.case=TRUE),
                  Term=gsub(Term, pattern="\\)", replacement="", ignore.case=TRUE),
                  Term=gsub(Term, pattern="LightingLeft", replacement="Lighting Left", ignore.case=TRUE),
                  Term=gsub(Term, pattern="LightingRight", replacement="Lighting Right", ignore.case=TRUE),
                  Term=gsub(Term, pattern="DistanceMiddle60cm", replacement="Distance Middle", ignore.case=TRUE),
                  Term=gsub(Term, pattern="DistanceFar90cm", replacement="Distance Far", ignore.case=TRUE),
                  Term=gsub(Term, pattern="PositionLeft24cm", replacement="Offset Left", ignore.case=TRUE),
                  Term=gsub(Term, pattern="PositionRight24cm", replacement="Offset Right", ignore.case=TRUE),
                  Term=gsub(Term, pattern="RotationLeft", replacement="Rotation Left", ignore.case=TRUE),
                  Term=gsub(Term, pattern="RotationRight", replacement="Rotation Right", ignore.case=TRUE)) %>% 
  subset(Term!="Intercept" & Term!="CountryJapan") %>% 
  separate(Term, c("Factor", "Condition"), sep=" ") %>% 
  mutate(Monitor=factor(Monitor, levels=c("Small", "Medium", "Large")),
         Anonym=factor(Anonym, levels=c("Non-anonymized", "Anonymized")),
         Factor=factor(Factor, levels=c("Offset", "Distance", "Rotation", "Lighting")),
         Condition=factor(Condition, levels=c("Left", "Right", "Middle", "Far"))) %>% 
  ggplot(aes(x=Condition, fill=Anonym))+
  geom_bar(aes(y=Estimate), stat="identity", position=position_dodge(0.4), width=0.4)+
  geom_abline(slope=0, intercept=0, color="gray30", lwd=0.5)+
  scale_y_continuous(limits=c(-0.8,0.8), breaks=seq(-0.6,0.6,0.3))+
  labs(title="OWLET", x="")+
  theme_bw()+
  scale_fill_manual(values=c("#336666", "#99CCCC"))+
  facet_grid(Monitor~Factor, scale="free")+
  theme(plot.title=element_text(size=18,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_blank(),
        legend.text=element_text(size=12, color="black"),
        legend.position="bottom") -> gp
print(gp)
ggsave(file=here("Figures", "Figure_5_OWLET.png"), plot=gp, dpi=350, width=5, height=7)

coef <- coef.i %>% mutate(Algorithm="iCatcher+") %>% 
  rbind(coef.o %>% mutate(Algorithm="OWLET"))

coef %>% mutate(Term=gsub(Term, pattern="\n", replacement="", ignore.case=TRUE),
                Term=gsub(Term, pattern=" ", replacement="", ignore.case=TRUE),
                Term=gsub(Term, pattern="\\(", replacement="", ignore.case=TRUE),
                Term=gsub(Term, pattern="\\)", replacement="", ignore.case=TRUE),
                Term=gsub(Term, pattern="LightingLeft", replacement="Lighting (Left)", ignore.case=TRUE),
                Term=gsub(Term, pattern="LightingRight", replacement="Lighting (Right)", ignore.case=TRUE),
                Term=gsub(Term, pattern="DistanceMiddle60cm", replacement="Distance (Middle)", ignore.case=TRUE),
                Term=gsub(Term, pattern="DistanceFar90cm", replacement="Distance (Far)", ignore.case=TRUE),
                Term=gsub(Term, pattern="PositionLeft24cm", replacement="Offset (Left)", ignore.case=TRUE),
                Term=gsub(Term, pattern="PositionRight24cm", replacement="Offset (Right)", ignore.case=TRUE),
                Term=gsub(Term, pattern="RotationLeft", replacement="Rotation (Left)", ignore.case=TRUE),
                Term=gsub(Term, pattern="RotationRight", replacement="Rotation (Right)", ignore.case=TRUE)) %>% 
  subset(Term!="Intercept" & Term!="CountryJapan") %>% 
  mutate(Monitor=factor(Monitor, levels=c("Small", "Medium", "Large")),
         Anonym=factor(Anonym, levels=c("Non-anonymized", "Anonymized")),
         Term=factor(Term, levels=c("Offset (Left)", "Offset (Right)", "Distance (Middle)", "Distance (Far)", "Rotation (Left)", "Rotation (Right)", "Lighting (Left)", "Lighting (Right)"))) %>% 
  ggplot(aes(x=Term, fill=Anonym))+
  geom_bar(aes(y=Estimate), stat="identity", position=position_dodge(0.4), width=0.4)+
  geom_abline(slope=0, intercept=0, color="gray30", lwd=0.5)+
  scale_y_continuous(limits=c(-0.8,0.8), breaks=seq(-0.6,0.6,0.3))+
  labs(title="", x="")+
  theme_bw()+
  scale_fill_manual(values=c("#336666", "#99CCCC"))+
  facet_grid(Monitor~Algorithm)+
  theme(plot.title=element_text(size=16,face="bold", color="black"),
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_blank(),
        legend.text=element_text(size=12, color="black"),
        legend.position="bottom") -> gp
print(gp)
# ggsave(file=here("Figures", "Figure_5.png"), plot=gp, dpi=350, width=7.7, height=7)



