#########################################################
####### 2. Face Detection ###############################
#########################################################


# -----------------------------------------------------------------------------------------
# iCatchet+

## Descriptive stats
(sum(d.i.n$N.frame) - sum(d.i.n$noface)) / sum(d.i.n$N.frame)
(sum(d.i.a$N.frame) - sum(d.i.a$noface)) / sum(d.i.a$N.frame)

d.i.n$Prop.face %>% summary()
d.i.a$Prop.face %>% summary()

d.i.n %>% subset(Prop.face<1) #%>% nrow() # / nrow(d.i.n)   # Only 1 trial
d.i.a %>% subset(Prop.face<1) #%>% nrow() # / nrow(d.i.a)   # Only 1 trial


# -----------------------------------------------------------------------------------------
# OWLET

## Descriptive stats
(sum(d.o.n$N.frame) - sum(d.o.n$noface)) / sum(d.o.n$N.frame)
(sum(d.o.a$N.frame) - sum(d.o.a$noface)) / sum(d.o.a$N.frame)

d.o.n$Prop.face %>% summary()
d.o.a$Prop.face %>% summary()
d.o.n$Prop.face %>% sd()
d.o.a$Prop.face %>% sd()


d.o.n %>% subset(Prop.face<1) %>% nrow() # / nrow(d.o.n)   # 23213 trials (= 71.7%)
d.o.a %>% subset(Prop.face<1) %>% nrow() # / nrow(d.o.a)   # 19951 trials (= 61.8%)

## Regression modeling (Non-Anonymized)
fit.f.o.n <- glmer(cbind(N.frame-noface, noface) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.n, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit.f.o.n)
Anova(fit.f.o.n)

res.f.o.n <- ggpredict(fit.f.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.f.o.n <- ggpredict(fit.f.o.n, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.f.o.n 
res.f.o.n <- as.data.frame(res.f.o.n) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Visualization
d.o.n %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(nface_prop = mean(Prop.face)) %>%
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=nface_prop))+
  geom_abline(slope=0, intercept=0.5, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.f.o.n, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Non-anonymized)", x="Lighting", y="Proportion of face detected")+
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
output_fig_dir <- here("Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive=TRUE)
}
ggsave(file=here("Figures", "Figure_S1_OWLET_NonAnonym.png"), plot=gp, dpi=350, width=8, height=6.5)

## Regression modeling (Anonymized)
fit.f.o.a <- glmer(cbind(N.frame-noface, noface) ~ Lighting + Distance + Position + Rotation + Country + (1|id), data=d.o.a, family=binomial,
                   # control=glmerControl(optimizer="nlminbwrap", calc.derivs = FALSE, optCtrl = list(maxfun = 10000, maxiter=100000)))
                   control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5)))
summary(fit.f.o.a)
Anova(fit.f.o.a)

res.f.o.a <- ggpredict(fit.f.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Ireland"), type="fixed")
# res.f.o.a <- ggpredict(fit.f.o.a, terms=c("Lighting", "Distance", "Rotation", "Position"), condition=c(Country="Japan"), type="fixed")
res.f.o.a 
res.f.o.a <- as.data.frame(res.f.o.a) %>% rename(Lighting=x, Distance=group, Rotation=facet, Position=panel) %>% 
  mutate(Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right")))

## Visualization
d.o.a %>% group_by(id, Lighting, Distance, Position, Rotation, Country) %>% 
  summarize(nface_prop = mean(Prop.face)) %>% 
  mutate(Position=factor(Position, levels=c("Sl24"="Left (24 cm)", "Sc00"="Center", "Sr24"="Right (24 cm)")),
         Lighting=factor(Lighting, levels=c("Left", "Front", "Right")),
         Rotation=str_replace_all(Rotation, c("^Center$"="Upright")),
         Rotation=factor(Rotation, levels=c("Left", "Upright", "Right"))) %>% 
  ggplot(aes(x=Lighting, y=nface_prop))+
  geom_abline(slope=0, intercept=0.5, linetype="dashed", color="gray30", lwd=0.5)+
  geom_jitter(aes(color=Rotation), position=position_jitterdodge(jitter.width=0.3, jitter.height=0.05), size=0.5, alpha=0.15)+
  geom_pointrange(data=res.f.o.a, aes(y=predicted, ymin=conf.low, ymax=conf.high, color=Rotation, shape=Rotation), position=position_dodge(0.7), size=0.5, linewidth=0.8)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET (Anonymized)", x="Lighting", y="Proportion of face detected")+
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
output_fig_dir <- here("Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive=TRUE)
}
ggsave(file=here("Figures", "Figure_S1_OWLET_Anonym.png"), plot=gp, dpi=350, width=8, height=6.5)

## Comparison of the proportion of the faces detected for the Non-Anonymized and Anonymized datasets
d.o.na <- d.o.n %>% dplyr::select(Prop.face, filename, Country) %>% 
  left_join(d.o.a, by="filename", suffix=c(".n", ".a")) %>% 
  mutate(face.diff = Prop.face.n - Prop.face.a,
         face.agr = case_when(face.diff== 0 ~ "0", TRUE~ "1"))

d.o.na %>% group_by(Country.n) %>% 
  summarize(N=n(),
            Mean.face.diff = mean(face.diff, na.rm=TRUE),
            SD.face.diff = sd(face.diff, na.rm=TRUE))

d.o.na %>% group_by(Country.n) %>% 
  summarize(N = n(),
            Prop.n = sum(face.agr=="0")/N)

