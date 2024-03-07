#########################################################
####### 1. Reliability Check and Data Shaping ###########
#########################################################

# resources (will be removed later)
# https://qiita.com/ocean_f/items/c0bdd1d73fc2a7a78963
# https://analysis-navi.com/?p=553
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

library(emmeans)   # ver 1.8.3
library(ggeffects) # ver 1.2.3
library(here)      # ver 1.0.1
library(irr)       # ver 0.84.1
library(lme4)      # ver 1.1.31
library(lmerTest)  # ver 3.1.3
library(tidyverse) # ver 1.3.2
library(car)       # ver 3.1.2

rm(list = ls())

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
# Marge all the data and perform the majority vote coding

## iCatcher+ (NonAnonymized)
filenames <- dir(here("Data", "iCatcher+", "RawData_NonAnonymized"))
d.i.n.all <- data.frame()
for (i in 1:length(filenames)) {
  tmp <- read.csv(here("Data", "iCatcher+", "RawData_NonAnonymized", filenames[i]), header=TRUE)
  d.i.n.all <- d.i.n.all %>% rbind(tmp)
}
d.i.n <- d.i.n.all %>% select(-frame, -confidence) %>% group_by_all() %>% summarize(N=n()) %>% 
  pivot_wider(names_from=annotation, values_from=N) %>% ungroup()
d.i.n[is.na(d.i.n)] <- 0
d.i.n <- d.i.n %>% rename(away=` away`, right=` right`, left=` left`, noface=` noface`) %>% 
  mutate(N.frame = away+right+left+noface,
         Prop.face = (N.frame-noface)/N.frame)

## iCatcher+ (Anonymized)
filenames <- dir(here("Data", "iCatcher+", "RawData_Anonymized"))
d.i.a.all <- data.frame()
for (i in 1:length(filenames)) {
  tmp <- read.csv(here("Data", "iCatcher+", "RawData_Anonymized", filenames[i]), header=TRUE)
  d.i.a.all <- d.i.a.all %>% rbind(tmp)
}
d.i.a <- d.i.a.all %>% select(-frame, -confidence) %>% group_by_all() %>% summarize(N=n()) %>% 
  pivot_wider(names_from=annotation, values_from=N) %>% ungroup()
d.i.a[is.na(d.i.a)] <- 0
d.i.a <- d.i.a %>% rename(away=` away`, right=` right`, left=` left`, noface=` noface`) %>% 
  mutate(N.frame = away+right+left+noface,
         Prop.face = (N.frame-noface)/N.frame)

## OWLET (NonAnonymized)
filenames <- dir(here("Data", "OWLET", "RawData_NonAnonymized"))
d.o.n.all <- data.frame()
for (i in 1:length(filenames)) {
  tmp <- read.csv(here("Data", "OWLET", "RawData_NonAnonymized", filenames[i]), header=TRUE)
  d.o.n.all <- d.o.n.all %>% rbind(tmp)
}
d.o.n.all <- d.o.n.all %>% subset(!is.na(frame)) %>% 
  mutate(face = case_when(x=="" | y=="" ~ "noface", TRUE ~ "yes"),
         new_tag = case_when(face=="noface" ~ "noface", TRUE ~ new_tag))
d.o.n <- d.o.n.all %>% select(-frame, -time2, -frame2, -x, -y, -saccade, -tag, -face) %>% group_by_all() %>% summarize(N=n()) %>% 
  pivot_wider(names_from=new_tag, values_from=N) %>% ungroup()
d.o.n[is.na(d.o.n)] <- 0
d.o.n <- d.o.n %>% mutate(N.frame = away+right+left+noface,
                          Prop.face = (N.frame-noface)/N.frame)

## OWLET (Anonymized)
filenames <- dir(here("Data", "OWLET", "RawData_Anonymized"))
d.o.a.all <- data.frame()
for (i in 1:length(filenames)) {
  tmp <- read.csv(here("Data", "OWLET", "RawData_Anonymized", filenames[i]), header=TRUE)
  d.o.a.all <- d.o.a.all %>% rbind(tmp)
}
d.o.a.all <- d.o.a.all %>% subset(!is.na(frame)) %>% 
  mutate(face = case_when(x=="" | y=="" ~ "noface", TRUE ~ "yes"),
         new_tag = case_when(face=="noface" ~ "noface", TRUE ~ new_tag))
d.o.a <- d.o.a.all %>% select(-frame, -time2, -frame2, -x, -y, -saccade, -tag, -face) %>% group_by_all() %>% summarize(N=n()) %>% 
  pivot_wider(names_from=new_tag, values_from=N) %>% ungroup()
d.o.a[is.na(d.o.a)] <- 0
d.o.a <- d.o.a %>% mutate(N.frame = away+right+left+noface,
                          Prop.face = (N.frame-noface)/N.frame)


# -----------------------------------------------------------------------------------------
# Missing files

d.files <- read.csv(here("Data", "iCatcher+", "_last_frame_NonAnonymized.csv"), header=TRUE) %>% select(-annotation, -confidence) %>% 
  mutate(filename=gsub(filename,pattern=".txt",replacement = "", ignore.case = TRUE))
tmp1 <- d.i.n %>% select(filename) %>% mutate(filename=gsub(filename,pattern=".txt",replacement = "", ignore.case = TRUE),
                                              iCatcher_NonAnonymized="Yes")
tmp2 <- d.i.a %>% select(filename) %>% mutate(filename=gsub(filename,pattern=".txt",replacement = "", ignore.case = TRUE),
                                              iCatcher_Anonymized="Yes")
tmp3 <- d.o.n %>% select(subject) %>% rename(filename=subject) %>% mutate(OWLET_NonAnonymized="Yes")
tmp4 <- d.o.a %>% select(subject) %>% rename(filename=subject) %>% mutate(OWLET_Anonymized="Yes")
d.files <- d.files %>% left_join(tmp1, by="filename") %>% left_join(tmp2, by="filename") %>% left_join(tmp3, by="filename") %>% left_join(tmp4, by="filename")
d.files[is.na(d.files)] <- "No"
rm(tmp1, tmp2, tmp3, tmp4)

d.files %>% mutate(Failure = case_when(iCatcher_NonAnonymized=="No" ~ 1, TRUE ~ 0)) %>% group_by(id) %>% 
  summarize(N.Trial=sum(Failure)) %>% ungroup() %>% summary()
d.files %>% mutate(Failure = case_when(iCatcher_Anonymized=="No" ~ 1, TRUE ~ 0)) %>% group_by(id) %>% 
  summarize(N.Trial=sum(Failure)) %>% ungroup() %>% summary()
d.files %>% mutate(Failure = case_when(OWLET_NonAnonymized=="No" ~ 1, TRUE ~ 0)) %>% group_by(id) %>% 
  summarize(N.Trial=sum(Failure)) %>% ungroup() %>% summary()
d.files %>% mutate(Failure = case_when(OWLET_Anonymized=="No" ~ 1, TRUE ~ 0)) %>% group_by(id) %>% 
  summarize(N.Trial=sum(Failure)) %>% ungroup() %>% summary()

d.files %>% subset(OWLET_NonAnonymized=="No") %>% nrow() # 42 / 540*60 = 0.0013
d.files %>% subset(OWLET_Anonymized=="No") %>% nrow()    # 101 / 540*60 = 0.0031

# write.csv(d.files, here("Data", "files_correspondence.csv"))


# -----------------------------------------------------------------------------------------
# Confirmation of the annotation consistency across frames

## iCatcher+ (NonAnonymized)
res.i.n <- d.i.n.all %>% group_by(filename, annotation) %>% summarize(N=n()) %>% ungroup %>% 
  group_by(filename) %>% summarize(Max=max(N)) %>% 
  left_join(d.i.n %>% select(filename, N.frame), by="filename") %>% 
  mutate(Prop = Max/N.frame, Dataset = "Non-anonymized")
res.i.n$Prop %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3333  0.8261  1.0000  0.8973  1.0000  1.0000 
res.i.n$Prop %>% sd() # 0.156308

## iCatcher+ (Anonymized)
res.i.a <- d.i.a.all %>% group_by(filename, annotation) %>% summarize(N=n()) %>% ungroup %>% 
  group_by(filename) %>% summarize(Max=max(N)) %>% 
  left_join(d.i.a %>% select(filename, N.frame), by="filename") %>% 
  mutate(Prop = Max/N.frame, Dataset = "Anonymized")
res.i.a$Prop %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3333  0.8000  1.0000  0.8956  1.0000  1.0000 
res.i.a$Prop %>% sd() # 0.1562877

## OWLET (NonAnonymized)
res.o.n <- d.o.n.all %>% group_by(filename, new_tag) %>% summarize(N=n()) %>% ungroup %>% 
  group_by(filename) %>% summarize(Max=max(N)) %>% 
  left_join(d.o.n %>% select(filename, N.frame), by="filename") %>% 
  mutate(Prop = Max/N.frame, Dataset = "Non-anonymized")
res.o.n$Prop %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3125  0.8710  1.0000  0.9116  1.0000  1.0000
res.o.n$Prop %>% sd() # 0.1460847

## OWLET (Anonymized)
res.o.a <- d.o.a.all %>% group_by(filename, new_tag) %>% summarize(N=n()) %>% ungroup %>% 
  group_by(filename) %>% summarize(Max=max(N)) %>% 
  left_join(d.o.a %>% select(filename, N.frame), by="filename") %>% 
  mutate(Prop = Max/N.frame, Dataset = "Anonymized")
res.o.a$Prop %>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3125  0.7742  0.9375  0.8683  1.0000  1.0000 
res.o.a$Prop %>% sd() # 0.1612747

## Visualization
res.i.n %>% rbind(res.i.a) %>% 
  mutate(Dataset=factor(Dataset, levels=c("Non-anonymized", "Anonymized"))) %>% 
  ggplot(aes(x=Dataset, y=Prop, color=Dataset, fill=Dataset))+
  geom_jitter(width=0.2, height=0.01, size=0.1, alpha=0.05)+
  # geom_violin(color="white", alpha=0.6)+
  geom_boxplot(fill="white", color="black", alpha=0.6, outlier.shape=NA)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="iCatcher+", x="Dataset", y="Proportion of the most dominant prediction")+
  theme_bw()+
  theme(legend.position="none",
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)

res.o.n %>% rbind(res.o.a) %>% 
  mutate(Dataset=factor(Dataset, levels=c("Non-anonymized", "Anonymized"))) %>% 
  ggplot(aes(x=Dataset, y=Prop, color=Dataset, fill=Dataset))+
  geom_jitter(width=0.2, height=0.01, size=0.1, alpha=0.05)+
  # geom_violin(color="white", alpha=0.6)+
  geom_boxplot(fill="white", color="black", alpha=0.6, outlier.shape=NA)+
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  labs(title="OWLET", x="Dataset", y="Proportion of the most dominant prediction")+
  theme_bw()+
  theme(legend.position="none",
        axis.ticks=element_line(color = "black"),
        axis.text.x=element_text(size=14, color = "black", angle=45, hjust=1),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text=element_text(size=14),
        legend.title=element_text(size=14, color="black", hjust=0.2),
        legend.text=element_text(size=12, color="black")) -> gp
print(gp)


# -----------------------------------------------------------------------------------------
# Rename the conditions

d.i.n <- d.i.n %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         Annotation=case_when(Prop.face==0 ~ "noface", away > left & away > right ~ "away", left > right ~ "left", right >= left ~ "right", TRUE ~ "other")) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

d.i.a <- d.i.a %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         Annotation=case_when(Prop.face==0 ~ "noface", away > left & away > right ~ "away", left > right ~ "left", right >= left ~ "right", TRUE ~ "other")) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

d.o.n <- d.o.n %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         Annotation=case_when(Prop.face==0 ~ "noface", away > left & away > right ~ "away", left > right ~ "left", right >= left ~ "right", TRUE ~ "other")) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

d.o.a <- d.o.a %>% 
  mutate(Lighting=str_replace_all(lighting, c("Lc"="Front", "Ll"="Left", "Lr"="Right")),
         Distance=str_replace_all(distance, c("D30"="Close\n(30 cm)", "D60"="Middle\n(60 cm)", "D90"="Far\n(90 cm)")),
         Distance=factor(Distance, levels=c("Close\n(30 cm)", "Middle\n(60 cm)", "Far\n(90 cm)")),
         Position=str_replace_all(side, c("Sc00"="Center", "Sl24"="Left (24 cm)", "Sr24"="Right (24 cm)")),
         Rotation=str_replace_all(rotation, c("Rc"="Center", "Rl"="Left", "Rr"="Right")),
         Country=substr(id,1,1),
         Country=str_replace_all(Country, c("c"="Ireland", "t"="Japan")),
         Annotation=case_when(Prop.face==0 ~ "noface", away > left & away > right ~ "away", left > right ~ "left", right >= left ~ "right", TRUE ~ "other")) %>% 
  dplyr::select(-lighting, -distance, -side, -rotation)

d.i.n %>% group_by(Annotation) %>% summarize(N=n())
d.i.a %>% group_by(Annotation) %>% summarize(N=n())
d.o.n %>% group_by(Annotation) %>% summarize(N=n())
d.o.a %>% group_by(Annotation) %>% summarize(N=n())

