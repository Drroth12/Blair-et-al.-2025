####Package load in####
library(ggplot2)
library(tidyverse)
library(readxl)
library(adehabitatHS)
library(gridExtra)
library(ggpubr)

####Main data file####
track.dat <- read_excel("C:/Users/blair/Desktop/Grad School/Thesis/Tracking_Data.xlsx", 
                        sheet = "Final Data", col_types = c("text", 
                                                            "date", "text", "date", "text", "text", 
                                                            "text", "text", "text", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "text", "text", 
                                                            "text", "text", "text"))
#Confirmed ID silvers
track_confirmed <- filter(track.dat, Confirmed == "y")

#Independent detections
ind.dat <- track_confirmed %>%
  group_by(`Tag #`, `Date`) %>%
  slice(1)


####Habitat data files####
#Data from point surveys
Hab.points <- read_excel("C:/Users/blair/Desktop/Grad School/Thesis/Habitat Sampling Data.xlsx", 
                        sheet = "Point Samples", col_types = c("date", 
                                                               "text", "numeric", "numeric", "text", 
                                                               "text", "numeric", "text", "numeric", 
                                                               "text", "numeric", "text"))
#Shoreline habitat data from both sampling periods
Shore.avg <- read_excel("C:/Users/blair/Desktop/Grad School/Thesis/Habitat Sampling Data.xlsx", 
                       sheet = "Shoreline Avgs Combined")

####Macro-Habitat use - Whole study area####
all.macro <- ind.dat %>%
  group_by(`Tag #`,`Date`) %>%
  count(Habitat)

macro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Habitat`,all.macro))

#Removing TRB and zeros
macro.table2 <- subset(macro.table, select = -c(TRB))

#Removing tags that were only found once
macro.table3 <- filter(macro.table2, (CBO + OB + IB) > 1 )

all.numb <- reframe(macro.table3, total = CBO + OB + IB)
all.numb2 <- reframe(all.numb, sum(total))

#Extracting only the average across all river sections - without trb
all.macro.est <- subset(Shore.avg, select = -c(TWG, LGJ, RUN, RPR, Section)) 

all.macro.est2 <- all.macro.est[,c(3,2,1)]

all.macro.est3 <- unlist(all.macro.est2[4,])

#Running manly selectivity model
wi.macro <- widesII(macro.table3, all.macro.est3 ,avknown = FALSE, alpha = 0.05)

plot(wi.macro)

wi.macro

#Converting to usable form in ggplot

z.score <- abs(qnorm(wi.macro$alpha/length(wi.macro$wi)))
ci = wi.macro$se.wi*z.score
selection.ratio = wi.macro$wi
df.1 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(all.macro.est3))
df.1.1 <- df.1 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#Making the plot
macro <- ggplot(data=df.1.1, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 3)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.09, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(text = element_text(size = 20, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3)) +
  xlab("") +
  ylab("Selection ratio (+/- CI)")

macro

####Micro-habitat use - Whole study area ####
all.micro <- ind.dat %>%
  group_by(`Tag #`,`Date`) %>%
  count(`Micro-habitat`)

micro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Micro-habitat`,all.micro))

#Removing fish only found once
micro.table2 <- filter(micro.table, (LGJ + RPR + RUN + TWG) >= 1)

#Extracting only the average across all river sections 
all.micro.est <- subset(Shore.avg, select = c(TWG, LGJ, RUN, RPR)) 

all.micro.est2 <- all.micro.est[,c(2,4,3,1)]

all.micro.est3 <- unlist(all.micro.est2[4,])

#Running manly selectivity model
wi.micro <- widesII(micro.table2, all.micro.est3 ,avknown = FALSE, alpha = 0.05)

plot(wi.micro) 

#Converting to useable form
z.score <- abs(qnorm(wi.micro$alpha/length(wi.micro$wi)))
ci = wi.micro$se.wi*z.score
selection.ratio = wi.micro$wi
df.2 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(all.micro.est3))
df.2.2 <- df.2 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#Plotting
micro <- ggplot(data=df.2.2, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 3)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.14, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(text = element_text(size = 20, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0,1,2,3,4)) +
  #scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ylab("Selection ratio (+/- CI)")
micro

####Upper Wabash####

#Upper wabash macro
UPW.dat <- filter(ind.dat, `River Section`=="UPW")

#Removing multiple detections from the same day - Only keeping the first
UPW.dat2 <- UPW.dat %>%
  group_by(`Tag #`,`Date`) %>%
  slice(1)

#Habitat use by individual fish
UPW.macro <- UPW.dat2 %>%
  group_by(`Tag #`) %>%
  count(Habitat)

#Covnerting to a table with each fish as a sampling unit
UPW.macro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Habitat`,UPW.macro))
UPW.macro.table2 <- subset(UPW.macro.table, select = -c(TRB))
#Removing tags that were only found once

UPW.macro.table3 <- filter(UPW.macro.table2, (CBO + OB + IB) >= 1 )

#Finding number of detections in this section - 118 w/ singles removed, 153 with all
UPW.numb <- reframe(UPW.macro.table3, total = CBO + OB + IB)
UPW.numb2 <- reframe(UPW.numb, sum(total))

#Estimated habitat vector
UPW.macro.est <- subset(Shore.avg, select = -c(TWG, LGJ, RUN, RPR, Section)) 

UPW.macro.est2 <- UPW.macro.est[,c(3,2,1)]

UPW.macro.est3 <- unlist(UPW.macro.est2[1,])


#Using the wi function with fish matrix then habitat estimates vector
wi.macro.UPW <- widesII(UPW.macro.table3, UPW.macro.est3 ,avknown = FALSE, alpha = 0.05)

wi.macro.UPW

plot(wi.macro.UPW)

z.score <- abs(qnorm(wi.macro.UPW$alpha/length(wi.macro.UPW$wi)))
ci = wi.macro.UPW$se.wi*z.score
selection.ratio = wi.macro.UPW$wi
df.3 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(UPW.macro.est3))
df.3.3 <- df.3 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

up.macro.plot <- ggplot(data=df.3.3, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 4)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0,1,2,3,4)) +
  xlab("") +
  ggtitle("Upper Wabash") +
  ylab("Selection ratio (+/- CI)")#

up.macro.plot

#Micro habitat use upper wabash

#Removing multiple detections from the same day - Only keeping the first
UPW.dat2 <- UPW.dat %>%
  group_by(`Tag #`,`Date`) %>%
  slice(1)

UPW.micro <- UPW.dat2 %>%
  group_by(`Tag #`,`Date`) %>%
  count(`Micro-habitat`)

UPW.micro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Micro-habitat`,UPW.micro))

#Removing fish only found once
UPW.micro.table2 <- filter(UPW.micro.table, (LGJ + RPR + RUN + TWG) >= 1)

#Extracting only the average across all river sections 
UPW.micro.est <- subset(Shore.avg, select = c(TWG, LGJ, RUN, RPR)) 

UPW.micro.est2 <- UPW.micro.est[,c(2,4,3,1)]

UPW.micro.est3 <- unlist(UPW.micro.est2[1,])

#Running manly selectivity model
wi.micro.UPW <- widesII(UPW.micro.table2, UPW.micro.est3 ,avknown = FALSE, alpha = 0.05)

wi.micro.UPW
#Converting to useable form
z.score <- abs(qnorm(wi.micro.UPW$alpha/length(wi.micro.UPW$wi)))
ci = wi.micro.UPW$se.wi*z.score
selection.ratio = wi.micro.UPW$wi
df.5 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(UPW.micro.est3))
df.5.5 <- df.5 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#Plotting
up.micro.plot <- ggplot(data=df.5.5, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 5)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
  #scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ggtitle("Upper Wabash") +
  ylab("Selection ratio (+/- CI)")
up.micro.plot

####Lower Wabash####
#Lower wabash macro
LOW.dat <- filter(ind.dat, `River Section`=="LOW")

#Removing multiple detections from the same day - Only keeping the first
LOW.dat2 <- LOW.dat %>%
  group_by(`Tag #`,`Date`) %>%
  slice(1)

#Habitat use by individual fish
LOW.macro <- LOW.dat2 %>%
  group_by(`Tag #`) %>%
  count(Habitat)

#Covnerting to a table with each fish as a sampling unit
LOW.macro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Habitat`,LOW.macro))

#Removing tags that were only found once
LOW.macro.table2 <- filter(LOW.macro.table, (CBO + OB + IB) >= 1 )
LOW.macro.table3 <- subset(LOW.macro.table, select = -c(TRB))
#Finding number of detections in this section - 55 w/ singles removed, 69 with all
LOW.numb <- reframe(LOW.macro.table2, total = CBO + OB + IB)
LOW.numb2 <- reframe(LOW.numb, sum(total))

#Estimated habitat vector
LOW.macro.est <- subset(Shore.avg, select = -c(TWG, LGJ, RUN, RPR, Section)) 

LOW.macro.est2 <- LOW.macro.est[,c(3,2,1)]
#Pulling out the row that is needed
LOW.macro.est3 <- unlist(LOW.macro.est2[2,])


#Using the wi function with fish matrix then habitat estimates vector
wi.macro.LOW <- widesII(LOW.macro.table3, LOW.macro.est3 ,avknown = FALSE, alpha = 0.05)

wi.macro.LOW

plot(wi.macro.LOW)

#Converting to useable form
z.score <- abs(qnorm(wi.macro.LOW$alpha/length(wi.macro.LOW$wi)))
ci = wi.macro.LOW$se.wi*z.score
selection.ratio = wi.macro.LOW$wi
df.4 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(UPW.macro.est3))
#Dataframe
df.4.4 <- df.4 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#plotting
low.macro.plot <- ggplot(data=df.4.4, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 4)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0:6.0)) +
  #scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ggtitle("Lower Wabash") +
  ylab("")#Selection ratio (+/- CI)
low.macro.plot

#Micro habitat use by individual fish lower river
LOW.dat <- filter(ind.dat, `River Section`=="LOW")

#Removing multiple detections from the same day - Only keeping the first
LOW.dat2 <- LOW.dat %>%
  group_by(`Tag #`,`Date`) %>%
  slice(1)

LOW.micro <- LOW.dat2 %>%
  group_by(`Tag #`,`Date`) %>%
  count(`Micro-habitat`)

LOW.micro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Micro-habitat`,LOW.micro))

#Removing fish only found once
LOW.micro.table2 <- filter(LOW.micro.table, (LGJ + RPR + RUN + TWG) >= 1)

#Extracting only the average across all river sections 
LOW.micro.est <- subset(Shore.avg, select = c(TWG, LGJ, RUN, RPR)) 

LOW.micro.est2 <- LOW.micro.est[,c(2,4,3,1)]

LOW.micro.est3 <- unlist(LOW.micro.est2[2,])

#Running manly selectivity model
wi.micro.LOW <- widesII(LOW.micro.table2, LOW.micro.est3 ,avknown = FALSE, alpha = 0.05)

wi.micro.LOW

plot(wi.micro)

#Converting to useable form
z.score <- abs(qnorm(wi.micro.LOW$alpha/length(wi.micro.LOW$wi)))
ci = wi.micro.LOW$se.wi*z.score
selection.ratio = wi.micro.LOW$wi
df.6 = data.frame(selection.ratio = selection.ratio,
                  selection.ratio.ci = ci,
                  habitat = names(LOW.micro.est3))
df.6.6 <- df.6 %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#Plotting
low.micro.plot <- ggplot(data=df.6.6, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 5)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0, 1 ,2 ,3, 4, 5), ) +
  #scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ggtitle("Lower Wabash") +
  ylab("")#Selection ratio (+/- CI)

low.micro.plot

####White River####
#White macro
WHT.dat <- filter(ind.dat, `River Section`=="WHT")

#Removing multiple detections from the same day - Only keeping the first
WHT.dat2 <- WHT.dat %>%
  group_by(`Tag #`,`Date`) %>%
  slice(1)

#Habitat use by individual fish
WHT.macro <- WHT.dat2 %>%
  group_by(`Tag #`) %>%
  count(Habitat)

#Covnerting to a table with each fish as a sampling unit
WHT.macro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Habitat`,WHT.macro))
#Removing tags that were only found once
WHT.macro.table2 <- filter(WHT.macro.table, (CBO + OB + IB) >= 1 )
WHT.macro.table3 <- subset(WHT.macro.table2, select = -c(TRB))
#Finding total # of detections - 64
WHT.macro.numb <- reframe(WHT.macro.table3, total = CBO + OB + IB)
WHT.macro.numb2 <- reframe(WHT.macro.numb, sum(total))

#Estimated habitat vector
WHT.macro.est <- subset(Shore.avg, select = -c(TWG, LGJ, RUN, RPR, Section)) 

#Reordering columns
WHT.macro.est2 <- WHT.macro.est[,c(3,2,1)]
#Selecting only white estimate
WHT.macro.est3 <- unlist(WHT.macro.est2[3,])

#Using the wi function with fish matrix then habitat estimates vector
wi.macro.WHT <- widesII(WHT.macro.table3, WHT.macro.est3 ,avknown = FALSE, alpha = 0.05)

wi.macro.WHT

plot(wi.macro.WHT)

#Converting to useable form
z.score <- abs(qnorm(wi.macro.WHT$alpha/length(wi.macro.WHT$wi)))
ci = wi.macro.WHT$se.wi*z.score
selection.ratio = wi.macro.WHT$wi
WHT.wi.macro = data.frame(selection.ratio = selection.ratio,
                          selection.ratio.ci = ci,
                          habitat = names(WHT.macro.est3))
WHT.wi.macro <- WHT.wi.macro %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#plot
wht.macro.plot <- ggplot(data=WHT.wi.macro, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 4)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), 
        axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0:6)) +
  xlab("") +
  ggtitle("White River") +
  ylab("")#Selection ratio (+/- CI)

wht.macro.plot

#Using dataframe made for macro habitat

WHT.micro <- WHT.dat2 %>%
  group_by(`Tag #`,`Date`) %>%
  count(`Micro-habitat`)

WHT.micro.table <- as.data.frame.matrix(xtabs(`n`~`Tag #`+`Micro-habitat`,WHT.micro))

#Removing fish only found once
WHT.micro.table2 <- filter(WHT.micro.table, (LGJ + RPR + RUN + TWG) >= 1)

#Finding the number of detections for the section - 64 w/ singles removed, 91 total
WHT.numb <- reframe(WHT.micro.table, total = LGJ + RPR + RUN + TWG)
WHT.numb2 <- reframe(WHT.numb, sum(total))

#Extracting only the average across all river sections 
WHT.micro.est <- subset(Shore.avg, select = c(TWG, LGJ, RUN, RPR)) 

WHT.micro.est2 <- WHT.micro.est[,c(2,4,3,1)]

WHT.micro.est3 <- unlist(WHT.micro.est2[3,])

#Running manly selectivity model
wi.micro.WHT <- widesII(WHT.micro.table2, WHT.micro.est3 ,avknown = FALSE, alpha = 0.05)

wi.micro.WHT
#Converting to useable form
z.score <- abs(qnorm(wi.micro.WHT$alpha/length(wi.micro.WHT$wi)))
ci = wi.micro.WHT$se.wi*z.score
selection.ratio = wi.micro.WHT$wi
WHT.wi.micro = data.frame(selection.ratio = selection.ratio,
                          selection.ratio.ci = ci,
                          habitat = names(WHT.micro.est3))
WHT.wi.micro <- WHT.wi.micro %>%
  arrange(desc(selection.ratio)) %>%
  mutate(species=factor(habitat, levels=habitat))

#Plotting
wht.micro.plot <- ggplot(data=WHT.wi.micro, aes(x=habitat,y=selection.ratio, ymin = 0, ymax = 5)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.12, linewidth = 0.7, aes(ymin = selection.ratio-selection.ratio.ci,
                                                   ymax=selection.ratio+selection.ratio.ci)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),text = element_text(size = 15, color = "black"), axis.text = element_text(color = "black")) +
  scale_x_discrete(expand = c(0.2,0)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
  #scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ggtitle("White River") +
  ylab("")#Selection ratio (+/- CI)

wht.micro.plot

####Combining selection ratio plots####
#Macro plots
macro.plots <- grid.arrange(up.macro.plot, low.macro.plot, wht.macro.plot, ncol = 3)

#Micro plots
micro.plots <- grid.arrange(up.micro.plot, low.micro.plot, wht.micro.plot, ncol = 3)
ggsave("micro.plots.jpeg", height = 6, width = 8, micro.plots, dpi = 700)

####Tracked individuals demographics####
tags.dat <-read_excel("C:/Users/blair/Desktop/Grad School/Thesis/Tracking_Data.xlsx", 
                                        sheet = "All tags")
#Removing duplicate column
tags.dat <- tags.dat[,-1]

#Adding individual data to detections
demo.dat <- merge(ind.dat, tags.dat, by = "Tag #")

#Counting number of unique males and females in data
sex.dat <- demo.dat %>%
  group_by(`Tag #`) %>%
  count(Sex)

#Number of independent detections = 322
sum(sex.dat$n)

#Number of unique tags = 111
nrow(sex.dat)

#sex ratio of detected fish
#71 males
nrow(sex.dat[sex.dat$Sex == "M", ] )
#40 females
nrow(sex.dat[sex.dat$Sex == "F", ] )

#percent m = 63.96369
71/111*100

#percent f = 36.03604
40/111*100

#Creating dataframe with only info of distinct tags detected
distinct.det <-  demo.dat %>%
  relocate(`Tag #`) %>%
  distinct(`Tag #`, .keep_all = TRUE) 

#Length mean and std error
mean(distinct.det$`Total Length (mm)`, na.rm = TRUE)
#660.2596
print(sd(distinct.det$`Total Length (mm)`, na.rm = TRUE)/sqrt(length(distinct.det$`Total Length (mm)`)))
#4.5

median(distinct.det$`Total Length (mm)`, na.rm = TRUE)
#661

#Weight mean and std error
mean(distinct.det$`Weight (g)`, na.rm = TRUE)
#3039.712
print(sd(distinct.det$`Weight (g)`, na.rm = TRUE)/sqrt(length(distinct.det$`Weight (g)`)))
#60.57416

#Length distribution histogram
distinct.det.hist <- 
  ggplot(data = distinct.det, aes(x = `Total Length (mm)`)) +
  geom_histogram(aes(fill = Sex, y = after_stat(count / sum(count))), binwidth = 10,
                 color = "black", position="stack") +
  scale_fill_manual(values=c("black","black")) +
  scale_y_continuous(name="Proportion", limits = c(0, 0.15)) +
  scale_x_continuous(name="Total Length (mm)", limits = c(500, 920)) +
  theme_classic() +
  ggtitle("Detected") +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.45),
        text = (element_text(size = 25,)), axis.text = element_text(color = "black"))

distinct.det.hist

####Tagged individual characteristics####

#Number of tagged individuals = 538
nrow(tags.dat)

#Number of male tagged = 316
nrow(tags.dat[tags.dat$Sex == "M",])

#Number of females tagged = 222
nrow(tags.dat[tags.dat$Sex == "F",])

#percent m = 58.73606
316/538*100

#percent f = 41.2634
222/538*100

#length mean and std error
mean(tags.dat$`Total Length (mm)`, na.rm = TRUE)
#671.6721
print(sd(tags.dat$`Total Length (mm)`, na.rm = TRUE)/sqrt(length(tags.dat$`Total Length (mm)`)))
#10.215

median(tags.dat$`Total Length (mm)`, na.rm = TRUE)
#660
ks.test(tags.dat$`Total Length (mm)`, distinct.det$`Total Length (mm)`)

#Weight mean and std error
mean(tags.dat$`Weight (g)`, na.rm = TRUE)
#3281.537
print(sd(tags.dat$`Weight (g)`, na.rm = TRUE)/sqrt(length(tags.dat$`Weight (g)`)))
#35.5977

ks.test(tags.dat$`Weight (g)`, distinct.det$`Weight (g)`)

tagged.hist <- 
  ggplot(data = tags.dat, aes(x = `Total Length (mm)`)) +
  geom_histogram(aes(fill = Sex, y = after_stat(count / sum(count))), binwidth = 10,
                 color = "black", position="stack") +
  scale_fill_manual(values=c("black","black")) +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.15)) +
  scale_x_continuous(name="", limits = c(500, 920)) +
  theme_classic() +
  ggtitle("Tagged") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.45),
        text = element_text(size = 25), axis.text = element_text(color = "black"))

tagged.hist

#Combining tagged and detected histograms
combined.hist <- grid.arrange(tagged.hist, distinct.det.hist, nrow = 2)

####Habitat metrics####
#percent estimations of available macrohabitat
macro.hab.est <- subset(Shore.avg, select = -c(TWG, LGJ, RUN, RPR))

#percent estimations of available microhabitat
micro.hab.est <- subset(Shore.avg, select = c(Section, LGJ, RPR, RUN, TWG))

#Macrohabitat depth by section
macro.stats <- Hab.points %>%
  group_by(Section, Macro) %>%
  summarise(mean = mean(Depth), SE = sd(Depth) / sqrt(length(Depth)))

#All macrohabitat depth
macro.all.stats <- Hab.points %>%
  group_by(Macro) %>%
  summarise(mean = mean(Depth), SE = sd(Depth) / sqrt(length(Depth)))

#Microhabitat depth by section
micro.stats <- Hab.points %>%
  group_by(Section, Micro) %>%
  summarise(mean = mean(Depth), SE = sd(Depth) / sqrt(length(Depth)))

#All microhabitat depth
micro.all.stats <- Hab.points %>%
  group_by(Micro) %>%
  summarise(mean = mean(Depth), SE = sd(Depth) / sqrt(length(Depth)))

#All habitat substrate
macro.substrate <- Hab.points %>%
  group_by(Substrate) %>%
  summarise(count = length(Substrate)) 

#All microhabitat substrate
micro.substrate <- Hab.points %>%
  group_by(Micro, Substrate) %>%
  summarise(count = length(Substrate))

#All macrohabitat velocity
macro.velo <- Hab.points %>%
  group_by(Macro) %>%
  summarise(mean = mean(Flow, na.rm = TRUE))

#All microhabitat velocity
micro.velo <- Hab.points %>%
  group_by(Micro) %>%
  summarise(mean = mean(Flow, na.rm = TRUE), SE = sd(Flow, na.rm = TRUE) / sqrt(length(Flow)))
####Logistic Regression####
ind.dat

#Discharge data
discharge <- read_excel("C:/Users/blair/Desktop/Grad School/Thesis/Tracking_Data.xlsx", 
                                sheet = "Discharge", col_types = c("text", 
                                "numeric", "date", "numeric", "text", "numeric"))

#Adding discharge data to main file
discharge.dat <- left_join(ind.dat, discharge, by = c('Date','River Section'))

#Using macro habitat
#Predicted with discharge
OB.dis <- glm(OB ~ discharge.dat$Percmax, family = "binomial")
summary(OB.dis)
IB.dis <- glm(IB ~ discharge.dat$Percmax, family = "binomial")
summary(IB.dis)
CBO.dis <- glm(CBO ~ discharge.dat$Percmax , family = "binomial")
summary(CBO.dis)
#Creating values into visual form
discharge.dat$OB.dis <- predict(OB.dis, type = "response")
discharge.dat$IB.dis <- predict(IB.dis, type = "response")
discharge.dat$CBO.dis <- predict(CBO.dis, type = "response")

#Using ggplot to create regression plot 
macro.dis <- ggplot(discharge.dat, aes(x = Percmax * 100)) +
  geom_line(aes(y = OB.dis, color = "OB"), linetype = 3, linewidth = 2.5) + 
  geom_line(aes(y = IB.dis, color = "IB"), linetype = 2, linewidth = 2.5) + 
  geom_line(aes(y = CBO.dis, color = "CBO"), linetype = 1, linewidth = 2.5) +   
  scale_y_continuous(expand = c(0,0), name = "Probability of Use",  limits = c(0, 0.5), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5)) +
  scale_x_continuous(expand = c(0.02,0), name = "Percent Maximum Discharge", limits = c(0,55), breaks = c(0, 10, 20,30, 40, 50)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black")) +
  theme(axis.text = element_text(color = "black"), legend.position = "top", 
        legend.title = element_blank(), legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(5.5,"line")) 
macro.dis

#Using temp.dat
temp.dat <- ind.dat

#Simple logistic regressions
OB <- as.numeric(temp.dat$Habitat == "OB")
IB <- as.numeric(temp.dat$Habitat == "IB")
CBO <- as.numeric(temp.dat$Habitat == "CBO")

OB.temp <- glm(OB ~ temp.dat$`Water temp (°C)`, family = "binomial")
summary(OB.temp)
IB.temp <- glm(IB ~ temp.dat$`Water temp (°C)`, family = "binomial")
summary(IB.temp)
CBO.temp <- glm(CBO ~ temp.dat$`Water temp (°C)` , family = "binomial")
summary(CBO.temp)
#Creating values into visual form
temp.dat$OB.temp <- predict(OB.temp, type = "response")
temp.dat$IB.temp <- predict(IB.temp, type = "response")
temp.dat$CBO.temp <- predict(CBO.temp, type = "response")

#Using ggplot to create regression plot 
macro.temp <- ggplot(temp.dat, aes(x = `Water temp (°C)`)) +
  geom_line(aes(y = OB.temp, color = "OB"), linetype = 3, linewidth = 2.5) + 
  geom_line(aes(y = IB.temp, color = "IB"), linetype = 2, linewidth = 2.5) + 
  geom_line(aes(y = CBO.temp, color = "CBO"), linetype = 1, linewidth = 2.5) +   
  scale_y_continuous(expand = c(0,0), name = "Probability of Use",  limits = c(0, 0.5), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5)) +
  scale_x_continuous(expand = c(0.02,0), name = "Temperature (°C)", limits = c(0,30.2), breaks = c(0,5,10,15,20,25,30)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black")) +
  theme(axis.text = element_text(color = "black"), legend.position = "top", 
        legend.title = element_blank(), legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(5.5,"line")) 
macro.temp
#ggsave("macro.temp.jpg", height = 6, width = 8, macro.temp, dpi = 700)

#combining macro prediction plots using ggarrange
macro.pred <- ggarrange(macro.dis, macro.temp, common.legend = TRUE, nrow = 2)
macro.pred

ggsave("macro.predictions.jpeg", height = 8, width = 8, dpi = 700, macro.pred)

#Now using micro habitat and discharge
LGJ <- as.numeric(discharge.dat$`Micro-habitat` == "LGJ")
RUN <- as.numeric(discharge.dat$`Micro-habitat` == "RUN")
RPR <- as.numeric(discharge.dat$`Micro-habitat` == "RPR")
TWG <- as.numeric(discharge.dat$`Micro-habitat` == "TWG")

#models
LGJ.dis <- glm(LGJ ~ discharge.dat$Percmax, family = "binomial")
summary(LGJ.dis)
RUN.dis <- glm(RUN ~ discharge.dat$Percmax, family = "binomial")
summary(RUN.dis)
RPR.dis <- glm(RPR ~ discharge.dat$Percmax , family = "binomial")
summary(RPR.dis)
TWG.dis <- glm(TWG ~ discharge.dat$Percmax, family = "binomial")
summary(TWG.dis)

table(fitted(TWG.dis)>.5, TWG)
#Creating values into visual form
discharge.dat$LGJ.dis <- predict(LGJ.dis, type = "response")
discharge.dat$RUN.dis <- predict(RUN.dis, type = "response")
discharge.dat$RPR.dis <- predict(RPR.dis, type = "response")
discharge.dat$TWG.dis <- predict(TWG.dis, type = "response")
#Using ggplot to create regression plot 
micro.dis <- ggplot(discharge.dat, aes(x = Percmax *100 )) +
  geom_line(aes(y = LGJ.dis, color = "LGJ"), linetype = 1, linewidth = 2.5) + 
  geom_line(aes(y = RUN.dis, color = "RUN"), linetype = 2, linewidth = 2.5) + 
  geom_line(aes(y = RPR.dis, color = "RPR"), linetype = 3, linewidth = 2.5) +   
  geom_line(aes(y = TWG.dis, color = "TWG"), linetype = 4, linewidth = 2.5) + 
  scale_y_continuous(expand = c(0,0), name = "Probability of Use", limits = c(0, 0.72), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(expand = c(0.02,0), name = "Percent Maximum Discharge", breaks = c(0, 10,20, 30, 40, 50), limits = c(0,42)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(5.5,"line"),
        axis.text = element_text(color = "black")) 
micro.dis

#Temperature
LGJ <- as.numeric(temp.dat$`Micro-habitat` == "LGJ")
RUN <- as.numeric(temp.dat$`Micro-habitat` == "RUN")
RPR <- as.numeric(temp.dat$`Micro-habitat` == "RPR")
TWG <- as.numeric(temp.dat$`Micro-habitat` == "TWG")

#models
LGJ.temp <- glm(LGJ ~ temp.dat$`Water temp (°C)`, family = "binomial")
summary(LGJ.temp)
RUN.temp <- glm(RUN ~ temp.dat$`Water temp (°C)`, family = "binomial")
summary(RUN.temp)
RPR.temp <- glm(RPR ~ temp.dat$`Water temp (°C)` , family = "binomial")
summary(RPR.temp)
TWG.temp <- glm(TWG ~ temp.dat$`Water temp (°C)`, family = "binomial")
summary(TWG.temp)

table(fitted(RPR.temp)>.5, RPR)
#Creating values into visual form
temp.dat$LGJ.temp <- predict(LGJ.temp, type = "response")
temp.dat$RUN.temp <- predict(RUN.temp, type = "response")
temp.dat$RPR.temp <- predict(RPR.temp, type = "response")
temp.dat$TWG.temp <- predict(TWG.temp, type = "response")

#Using ggplot to create regression plot 
micro.temp <- ggplot(temp.dat, aes(x = `Water temp (°C)` )) +
  geom_line(aes(y = LGJ.temp, color = "LGJ"), linetype = 1, linewidth = 2.5) + 
  geom_line(aes(y = RUN.temp, color = "RUN"), linetype = 2, linewidth = 2.5) + 
  geom_line(aes(y = RPR.temp, color = "RPR"), linetype = 3, linewidth = 2.5) +   
  geom_line(aes(y = TWG.temp, color = "TWG"), linetype = 4, linewidth = 2.5) + 
  scale_y_continuous(expand = c(0,0), name = "Probability of Use", limits = c(0, 0.72), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(expand = c(0.02,0), name = "Temperature (°C)", limits = c(0,30.2), breaks = c(0,5,10,15,20,25,30)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(5.5,"line"),
        axis.text = element_text(color = "black"))
micro.temp

plot(discharge.dat$Percmax)

#Combining plots
micro.pred <- ggarrange(micro.dis, micro.temp, common.legend = TRUE, nrow = 2)

####Regression by reach section####
#UPW macro
OB.UPW <- as.numeric(UPW.dat$Habitat == "OB")
IB.UPW <- as.numeric(UPW.dat$Habitat == "IB")
CBO.UPW <- as.numeric(UPW.dat$Habitat == "CBO")

OB.UPW.temp <- glm(OB.UPW ~ UPW.dat$`Water temp (°C)`, family = "binomial")
summary(OB.UPW.temp)
IB.UPW.temp <- glm(IB.UPW ~ UPW.dat$`Water temp (°C)`, family = "binomial")
summary(IB.temp)
CBO.UPW.temp <- glm(CBO.UPW ~ UPW.dat$`Water temp (°C)` , family = "binomial")
summary(CBO.temp)
#Creating values into visual form
UPW.dat$OB.temp <- predict(OB.UPW.temp, type = "response")
UPW.dat$IB.temp <- predict(IB.UPW.temp, type = "response")
UPW.dat$CBO.temp <- predict(CBO.UPW.temp, type = "response")

#Using ggplot to create regression plot 
macro.UPW.temp <- ggplot(UPW.dat, aes(x = `Water temp (°C)`)) +
  geom_line(aes(y = OB.temp, color = "OB"), linetype = 3, linewidth = 2.5) + 
  geom_line(aes(y = IB.temp, color = "IB"), linetype = 2, linewidth = 2.5) + 
  geom_line(aes(y = CBO.temp, color = "CBO"), linetype = 1, linewidth = 2.5) +   
  scale_y_continuous(expand = c(0,0), name = "Probability of Use",  limits = c(0, 0.6), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(expand = c(0.02,0), name = "Temperature (°C)", limits = c(0,30.2), breaks = c(0,5,10,15,20,25,30)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black")) +
  theme(axis.text = element_text(color = "black"), legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(3,"line"),
        axis.text = element_text(color = "black")) 

macro.UPW.temp
ggsave("macro.UPW.temp.jpg", height = 4.5, width = 8, macro.UPW.temp, dpi = 700)

#LOW macro
OB.LOW <- as.numeric(LOW.dat$Habitat == "OB")
IB.LOW <- as.numeric(LOW.dat$Habitat == "IB")
CBO.LOW <- as.numeric(LOW.dat$Habitat == "CBO")

OB.LOW.temp <- glm(OB.LOW ~ LOW.dat$`Water temp (°C)`, family = "binomial")
IB.LOW.temp <- glm(IB.LOW ~ LOW.dat$`Water temp (°C)`, family = "binomial")
CBO.LOW.temp <- glm(CBO.LOW ~ LOW.dat$`Water temp (°C)` , family = "binomial")
summary(CBO.temp)
#Creating values into visual form
LOW.dat$OB.temp <- predict(OB.LOW.temp, type = "response")
LOW.dat$IB.temp <- predict(IB.LOW.temp, type = "response")
LOW.dat$CBO.temp <- predict(CBO.LOW.temp, type = "response")

#Using ggplot to create regression plot 
macro.LOW.temp <- ggplot(LOW.dat, aes(x = `Water temp (°C)`)) +
  geom_line(aes(y = OB.temp, color = "OB"), linetype = 3, linewidth = 2) + 
  geom_line(aes(y = IB.temp, color = "IB"), linetype = 2, linewidth = 2) + 
  geom_line(aes(y = CBO.temp, color = "CBO"), linetype = 1, linewidth = 2) +   
  scale_y_continuous(expand = c(0,0), name = "Probability of Use",  limits = c(0, 0.6), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(expand = c(0.02,0), name = "Temperature (°C)", limits = c(0,30.2), breaks = c(0,5,10,15,20,25,30)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black")) +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(3,"line")) 

macro.LOW.temp
ggsave("macro.UPW.temp.jpg", height = 4.5, width = 8, macro.UPW.temp, dpi = 700)

#WHT
OB.WHT <- as.numeric(WHT.dat$Habitat == "OB")
IB.WHT <- as.numeric(WHT.dat$Habitat == "IB")
CBO.WHT <- as.numeric(WHT.dat$Habitat == "CBO")

OB.WHT.temp <- glm(OB.WHT ~ WHT.dat$`Water temp (°C)`, family = "binomial")
IB.WHT.temp <- glm(IB.WHT ~ WHT.dat$`Water temp (°C)`, family = "binomial")
CBO.WHT.temp <- glm(CBO.WHT ~ WHT.dat$`Water temp (°C)` , family = "binomial")
summary(CBO.temp)
#Creating values into visual form
WHT.dat$OB.temp <- predict(OB.WHT.temp, type = "response")
WHT.dat$IB.temp <- predict(IB.WHT.temp, type = "response")
WHT.dat$CBO.temp <- predict(CBO.WHT.temp, type = "response")

#Using ggplot to create regression plot 
macro.WHT.temp <- ggplot(WHT.dat, aes(x = `Water temp (°C)`)) +
  geom_line(aes(y = OB.temp, color = "OB"), linetype = 3, linewidth = 2) + 
  geom_line(aes(y = IB.temp, color = "IB"), linetype = 2, linewidth = 2) + 
  geom_line(aes(y = CBO.temp, color = "CBO"), linetype = 1, linewidth = 2) +   
  scale_y_continuous(expand = c(0,0), name = "Probability of Use",  limits = c(0, 0.6), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  scale_x_continuous(expand = c(0.02,0), name = "Temperature (°C)", limits = c(0,30.2), breaks = c(0,5,10,15,20,25,30)) +
  theme_classic() +
  scale_color_manual(values = c("black", "black", "black")) +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 20), text = element_text(size = 23),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"), legend.key.size = unit(3,"line")) 

macro.WHT.temp
ggsave("macro.UPW.temp.jpg", height = 4.5, width = 8, macro.UPW.temp, dpi = 700)
