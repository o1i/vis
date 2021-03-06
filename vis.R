library(tidyverse)
library(RColorBrewer)
library(colorspace)


# --- Data Prep ----------------------------------------------------------------
# Read
cdc <- read.csv("data/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv")
colnames(cdc) <- tolower(colnames(cdc))
pop <- read.csv("data/pop.csv")

party <- read.csv("data/party.csv", strip.white = T)

# Prepare
used <- cdc %>% 
  filter(sex %in% c("Male", "Female"),
         !is.na(covid.19.deaths)) %>% 
  select(state, sex, age.group, covid.19.deaths, total.deaths)
  
table(used$age.group, is.na(used$covid.19.deaths))
# Sensible age groups:
# https://www.worldlifeexpectancy.com/usa-cause-of-death-by-age-and-gender
# 15-34 (competing against accidents, poisonings, suicide, homicide)
# 35-54 (mostly poisoning and suicide, starting with coronary heart disease)
# 55- (coronary heart disease and increasingly cancer)

used$age = NA
used[used$age.group %in% c("15-24 years", "18-29 years", "25-34 years"), 
     "age"] <- "young"
used[used$age.group %in% c("30-49 years", "35-44 years", "45-54 years", 
                           "50-64 years"), "age"] <- "middle"
used[used$age.group %in% c("55-64 years", "65-74 years", "75-84 years", 
                           "85 years and over"), "age"] <- "older"
vis <- used %>% 
  filter(!is.na(age)) %>% 
  group_by(state, sex, age) %>% 
  summarise(covid=sum(covid.19.deaths), tot_deaths=sum(total.deaths))

vis <- merge(vis, pop[, c("pop", "state")], on="state")
vis$covid_f <- vis$covid / vis$pop
vis$tot_f <- vis$tot_deaths / vis$pop

vis <- merge(vis, party, on="state")


large_states <- (pop %>% arrange(desc(pop)) %>% top_n(9))$state
sample_states <-c("California", "New York", "Florida",
                  "South Dakota", "Oregon", "Tennessee",
                  "Georgia", "South Carolina", "Maine" )



theme_set(theme_grey(base_size = 20)) 
use_cols = brewer.pal(8, "Set1")[c(1, 3)]
# --- Vis 1.0: Pie chart -------------------------------------------------------
used_states <- sample_states
order <- (vis %>% 
            filter(state %in% used_states) %>% 
            group_by(state) %>%  
            summarise(covid=sum(covid)) %>% 
            arrange(covid))$state
png("fig/01_pie.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, age) %>% 
  mutate(covid=sum(covid)) %>% 
ggplot(aes(x="", 
           y=covid, 
           alpha=factor(age, levels=c("young", "middle", "older")), 
           fill=factor(state, levels=order))) + 
  geom_col() + 
  coord_polar(theta="y") + 
  scale_fill_brewer(palette="Set1", direction=-1) +
  ggtitle("Covid Deaths in 9 states by Age group and state") +
  guides(fill=guide_legend("State"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("")
dev.off()


# --- Vis 2.0 Bars -------------------------------------------------------------
order <- (vis %>% 
            filter(state %in% used_states) %>% 
            group_by(state) %>%  
            summarise(covid=sum(covid)) %>% 
            arrange(desc(covid)))$state
png("fig/02_bar_01.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, age, sex) %>% 
  summarise(covid=sum(covid)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=covid, 
             alpha=factor(age, levels=c("young", "middle", "older")), 
             fill=factor(sex))) + 
  geom_col() + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Covid Deaths in 9 states by Age group, sex and state") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("")
dev.off()

png("fig/02_bar_02.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, age, sex) %>% 
  mutate(covid=sum(covid)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=covid, 
             alpha=factor(age, levels=c("young", "middle", "older")), 
             fill=factor(sex))) + 
  geom_col(position=position_dodge(width = 0.9)) + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Covid Deaths in 9 states by Age group, sex and state") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("") 
dev.off()

png("fig/02_bar_03.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, age) %>% 
  mutate(covid=sum(covid)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=covid, 
             alpha=factor(age, levels=c("young", "middle", "older")))) + 
  geom_col(position=position_dodge(width = 0.9)) + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Covid Deaths in 9 states by Age group and state") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("") 
dev.off()

png("fig/02_bar_04.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, sex) %>% 
  mutate(covid=sum(covid)) %>% 
  arrange(desc(covid)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=covid, 
             fill=factor(sex))) + 
  geom_col(position=position_dodge(width = 0.9)) + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Covid Deaths in 9 states by Sex and state") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("") 
dev.off()


# --- Vis 3.0 Towards Points ---------------------------------------------------
used_states <- c("North Carolina", "Idaho", "South Dakota", "North Dakota",
                 "Missouri", "Nebraska", "Oklahoma", "Tennessee", "Utah")
order <- (vis %>% 
            filter(state %in% used_states) %>% 
            group_by(state) %>%  
            summarise(frac=sum(covid)/sum(tot_deaths)) %>% 
            arrange(desc(frac)))$state
png("fig/03_point_01.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state) %>% 
  summarise(fraction=sum(covid)/sum(tot_deaths)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=fraction)) + 
  geom_col() + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Fraction of deaths due to covid") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("") 
dev.off()

png("fig/03_point_02.png", width=900, height=800)
vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state) %>% 
  summarise(fraction=sum(covid)/sum(tot_deaths)) %>% 
  ggplot(aes(x=factor(state, levels=order), 
             y=fraction)) + 
  geom_point(size=3) + 
  scale_fill_manual(values=use_cols) +
  ggtitle("Fraction of deaths due to covid") +
  guides(fill=guide_legend("Sex"), alpha=guide_legend("Age Group")) +
  xlab("") + ylab("") 
dev.off()


# --- Vis 4.0 ease of access, info to ink --------------------------------------
used_states <- sample_states
data <- vis %>% 
  filter(state %in% used_states) %>% 
  group_by(state, sex) %>% 
  summarise(rate=sum(covid) / sum(pop)) %>% 
  arrange(desc(rate))
state_order <- unique(data$state)

# # comparison
# data %>% 
#   ggplot(aes(x=factor(state, levels=state_order), y = rate, fill=factor(sex, levels = c("Male", "Female")))) + 
#   geom_col(position=position_dodge(width = 0.9)) +
# scale_fill_manual(values=use_cols) +
#   xlab("Sex") + ylab("Covid Death Rate") +
#   ggtitle("Covid death rate for a sample of states")

# --- Step 0: very bad

n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_x <- 1:n - 0.5
grid_y <- seq(0, 0.00015, l=13)
grid_lwd <- 1.7
labels_x <- 1:n - 0.5
labels_y <- seq(0, 0.00015, l=13)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")[c(1, 3)]
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_00.png", width=900, height=800)
par(mar=c(7.1, 7.1, 4.1, 2.1))
plot(NULL, xlim = c(-0.1, n + 0.1), xaxs="i", ylim = c(0, max(data$rate)), xlab="", 
     ylab="", axes=F, main = "Covid death rate for a sample of states",
     cex.main=cex_title, cex.lab=cex_lab)
mtext(side=2, "Covid Death Rate", line=5.5, cex=cex_lab)
mtext(side=1, "State", line=6, cex=cex_lab)
usr <- par()$usr
rect(usr[1], usr[3], usr[2], usr[4], col=light_gray, border=NA)
abline(v = grid_x, h = grid_y, col = "white", lwd =grid_lwd)
text(labels_x + 0.25, -diff(labels_y)[1]/6*4, unique(data$state), srt=35, xpd=T, adj=1, cex=cex_labels)
mtext(formatC(labels_y, format="f", digits=5, drop0trailing = TRUE), side=2, at=labels_y, las=1, line=0.3, cex=cex_labels)
legend("right", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(i - 1 + large_bar_space / 2, 0, i - 0.5 - small_bar_space / 2, m, col=cols[2], border=NA)
  rect(i - 0.5 + small_bar_space / 2, 0, i - large_bar_space / 2, f, col=cols[1], border=NA)
}
dev.off()

# --- Step 0a: colors

n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_x <- 1:n - 0.5
grid_y <- seq(0, 0.00015, l=13)
grid_lwd <- 1.7
labels_x <- 1:n - 0.5
labels_y <- seq(0, 0.00015, l=13)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_00a.png", width=900, height=800)
par(mar=c(7.1, 7.1, 4.1, 2.1))
plot(NULL, xlim = c(-0.1, n + 0.1), xaxs="i", ylim = c(0, max(data$rate)), xlab="", 
     ylab="", axes=F, main = "Covid death rate for a sample of states",
     cex.main=cex_title, cex.lab=cex_lab)
mtext(side=2, "Covid Death Rate", line=5.5, cex=cex_lab)
mtext(side=1, "State", line=6, cex=cex_lab)
usr <- par()$usr
rect(usr[1], usr[3], usr[2], usr[4], col=light_gray, border=NA)
abline(v = grid_x, h = grid_y, col = "white", lwd =grid_lwd)
text(labels_x + 0.25, -diff(labels_y)[1]/6*4, unique(data$state), srt=35, xpd=T, adj=1, cex=cex_labels)
mtext(formatC(labels_y, format="f", digits=5, drop0trailing = TRUE), side=2, at=labels_y, las=1, line=0.3, cex=cex_labels)
legend("right", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(i - 1 + large_bar_space / 2, 0, i - 0.5 - small_bar_space / 2, m, col=cols[2], border=NA)
  rect(i - 0.5 + small_bar_space / 2, 0, i - large_bar_space / 2, f, col=cols[1], border=NA)
}
dev.off()

# --- Step 1: number of gridlines

n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_x <- 1:n - 0.5
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_x <- 1:n - 0.5
labels_y <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_01.png", width=900, height=800)
par(mar=c(7.1, 7.1, 4.1, 2.1))
plot(NULL, xlim = c(-0.1, n + 0.1), xaxs="i", ylim = c(0, max(data$rate)), xlab="", 
     ylab="", axes=F, main = "Covid death rate for a sample of states",
     cex.main=cex_title, cex.lab=cex_lab)
mtext(side=2, "Covid Death Rate", line=5.5, cex=cex_lab)
mtext(side=1, "State", line=6, cex=cex_lab)
usr <- par()$usr
rect(usr[1], usr[3], usr[2], usr[4], col=light_gray, border=NA)
abline(v = grid_x, h = grid_y, col = "white", lwd =grid_lwd)
text(labels_x + 0.25, -diff(labels_y)[1]/6, unique(data$state), srt=35, xpd=T, adj=1, cex=cex_labels)
mtext(formatC(labels_y, format="f", digits=5, drop0trailing = TRUE), side=2, at=labels_y, las=1, line=0.3, cex=cex_labels)
legend("right", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(i - 1 + large_bar_space / 2, 0, i - 0.5 - small_bar_space / 2, m, col=cols[2], border=NA)
  rect(i - 0.5 + small_bar_space / 2, 0, i - large_bar_space / 2, f, col=cols[1], border=NA)
}
dev.off()

# --- Step 2 Unclutter Background
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_x <- 1:n - 0.5
labels_y <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_02.png", width=900, height=800)
par(mar=c(7.1, 7.1, 4.1, 2.1))
plot(NULL, xlim = c(-0.1, n + 0.1), xaxs="i", ylim = c(0, max(data$rate)), xlab="", 
     ylab="", axes=F, main = "Covid death rate for a sample of states",
     cex.main=cex_title, cex.lab=cex_lab)
mtext(side=2, "Covid Death Rate", line=5.5, cex=cex_lab)
mtext(side=1, "State", line=6, cex=cex_lab)
abline(h = grid_y, col = light_gray, lwd =grid_lwd)
text(labels_x + 0.25, -diff(labels_y)[1]/6, unique(data$state), srt=35, xpd=T, adj=1, cex=cex_labels)
mtext(formatC(labels_y, format="f", digits=5, drop0trailing = TRUE), side=2, at=labels_y, las=1, line = 0.3, cex=cex_labels)
legend("right", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(i - 1 + large_bar_space / 2, 0, i - 0.5 - small_bar_space / 2, m, col=cols[2], border=NA)
  rect(i - 0.5 + small_bar_space / 2, 0, i - large_bar_space / 2, f, col=cols[1], border=NA)
}
dev.off()

# --- Step 3, Flip bar chart
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_03.png", width=900, height=800)
par(mar=c(4.1, 10, 4.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "Covid death rate for a sample of states",
     cex.main=cex_title, cex.lab=cex_lab)
mtext(side=2, "State", line=8.5, cex=cex_lab)
mtext(side=1, "Covid Death Rate", line=3, cex=cex_lab)
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(side=2, at = n+0.25,  "State", las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x, format="f", digits=5, drop0trailing = TRUE), side=1, at=labels_x, las=1, line = 0.3, cex=cex_labels)
legend("bottom", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
dev.off()

# --- Step 4, Align font sizes and locations
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_04.png", width=900, height=800)
par(mar=c(4.1, 10, 4.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x, format="f", digits=5, drop0trailing = TRUE), side=1, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
mtext(at=max(labels_x), side=1, adj=1, "Covid death rate", line = 2, cex=legend_cex)
legend("bottomright", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title)
dev.off()

# --- Step 5, Move x axis
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1


png("fig/04_levers_05.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
mtext(at=max(labels_x), side=3, adj=1, "Covid death rate", line = 2, cex=legend_cex)
legend("bottomright", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", line = 3, cex=cex_title)
dev.off()

# --- Step 6, Move x axis
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1

cex_main <- 1.9


png("fig/04_levers_06.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
legend("bottomright", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
dev.off()

# --- Step 7, Readable units
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.00
large_bar_space <- 0.1

cex_main <- 1.9
cex_sub <- 1.3


png("fig/04_levers_07.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
legend("bottomright", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
dev.off()

# --- Step 8, Split bars
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.03
large_bar_space <- 0.15

cex_main <- 1.9
cex_sub <- 1.3


png("fig/04_levers_08.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
legend("bottomright", legend=c("Male", "Female"), fill = cols[2:1], cex=legend_cex, border=NA, box.lwd = 0)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
dev.off()

# --- Step 8b, Integrate legend
n <- length(unique(data$state))
light_gray <- grey(0.92)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- brewer.pal(8, "Set1")
small_bar_space <- 0.03
large_bar_space <- 0.15

cex_main <- 1.9
cex_sub <- 1.3

legend_cols <- darken(desaturate(brewer.pal(8, "Set1"), 0.15), 0.75)
legend_y <- c(
  ((n - large_bar_space/2) + (n - 0.5 + small_bar_space/2))/2,
  ((n - 1 + large_bar_space/2) + (n - 0.5 - small_bar_space/2))/2
)


png("fig/04_levers_08b.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), cex=cex_labels)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
text(legend_x, legend_y, labels=c("Male", "Female"), col = legend_cols[2:1], adj=0, font=2, cex=legend_cex)
dev.off()

# --- Step 9, Mute colors
n <- length(unique(data$state))
light_gray <- grey(0.92)
dark_gray <- grey(0.5)
almost_black <- grey(0.3)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- darken(rev(c(rgb(173, 209, 191, m=256), rgb(213, 197, 229, m=256))), 0.225)
small_bar_space <- 0.02
large_bar_space <- 0.15

cex_main <- 1.9
cex_sub <- 1.3


legend_cols <- darken(desaturate(cols, 0.3),0.5)
legend_y <- c(
  ((n - large_bar_space/2) + (n - 0.5 + small_bar_space/2))/2,
  ((n - 1 + large_bar_space/2) + (n - 0.5 - small_bar_space/2))/2
)


png("fig/04_levers_09.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, col=almost_black, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), col=dark_gray, cex=cex_labels)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
legend_x <- diff(par()$usr)[1] * 0.02
text(legend_x, legend_y, labels=c("Male", "Female"), col = legend_cols[2:1], adj=0, font=2, cex=legend_cex)
dev.off()


# --- Step 9b, Add numbers
n <- length(unique(data$state))
light_gray <- grey(0.92)
dark_gray <- grey(0.5)
almost_black <- grey(0.3)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
cols <- darken(rev(c(rgb(173, 209, 191, m=256), rgb(213, 197, 229, m=256))), 0.225)
small_bar_space <- 0.02
large_bar_space <- 0.15

cex_main <- 1.9
cex_sub <- 1.3


legend_cols <- darken(desaturate(cols, 0.3),0.5)
legend_y <- c(
  ((n - large_bar_space/2) + (n - 0.5 + small_bar_space/2))/2,
  ((n - 1 + large_bar_space/2) + (n - 0.5 - small_bar_space/2))/2
)


png("fig/04_levers_09b.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, col=almost_black, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), col=dark_gray, cex=cex_labels)
legend_x <- diff(par()$usr)[1] * 0.02
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  text(f + legend_x/2, ((n+1- i - 1 + large_bar_space / 2) + ( n+1-i - 0.5 - small_bar_space / 2) )/2, adj=0, labels=floor(f*1e6), col=almost_black, cex=cex_labels)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
  text(m + legend_x/2, ((n+1-i - 0.5 + small_bar_space / 2) + (n+1-i - large_bar_space / 2) )/2, adj=0, labels=floor(m*1e6), col=almost_black, cex=cex_labels)
}
mtext(side=3, at = 0, adj = 0, "Covid death rate for sample states", cex=cex_title, line = 4)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab)
legend_x <- diff(par()$usr)[1] * 0.02
text(legend_x, legend_y, labels=c("Male", "Female"), col = legend_cols[2:1], adj=0, font=2, cex=legend_cex)
dev.off()

# --- Step 10, Tell story
n <- length(unique(data$state))
light_gray <- grey(0.92)
dark_gray <- grey(0.5)
almost_black <- grey(0.3)
grid_y <- seq(0, 0.00015, l=7)
grid_lwd <- 1.7
labels_y <- 1:n - 0.5
labels_x <- seq(0, 0.00015, l=4)
legend_cex = 1.5
cex_title <- 2.5
cex_labels <- 1.5
cex_lab <- 1.5
pre_cols <- darken(rev(c(rgb(173, 209, 191, m=256), rgb(213, 197, 229, m=256))), 0.225)
cols <- lighten(desaturate(pre_cols, 0.1), c(0.35, 0.45))
small_bar_space <- 0.02
large_bar_space <- 0.15

cex_main <- 1.9
cex_sub <- 1.3


legend_cols <- darken(desaturate(pre_cols, 0.3),0.3)
legend_y <- c(
  ((n - large_bar_space/2) + (n - 0.5 + small_bar_space/2))/2,
  ((n - 1 + large_bar_space/2) + (n - 0.5 - small_bar_space/2))/2
)

sc_women_rect_pos <- c(
  (data %>% filter(state=="South Carolina", sex=="Male"))$rate[1],
  (n - 1 + large_bar_space/2),
  (data %>% filter(state=="South Carolina", sex=="Female"))$rate[1],
  (n - 0.5 - small_bar_space/2)
)
sc_col <-  darken(pre_cols[1], 0.35)


png("fig/04_levers_10.png", width=900, height=800)
par(mar=c(2.1, 10, 6.1, 2.1))
plot(NULL, ylim = c(-0.1, n + 0.1), yaxs="i", xlim = c(0, max(data$rate)*1.05), xaxs="i", ylab="", 
     xlab="", axes=F, main = "")
abline(v = grid_y, col = light_gray, lwd =grid_lwd)
mtext(side=2, at = rev(labels_y),  unique(data$state), las=1, adj = 1, line=0.3, col=almost_black, cex=cex_labels)
mtext(formatC(labels_x*1e6, format="f", digits=5, drop0trailing = TRUE), side=3, at=labels_x, line = 0.1, adj= c(0, rep(0.5, l=length(labels_x) - 2), 1), col=dark_gray, cex=cex_labels)
for (st in state_order){
  i <- which(state_order == st)
  m <- subset(data, (data$state == st)&(data$sex=="Male"))$rate[1]
  f <- subset(data, (data$state == st)&(data$sex=="Female"))$rate[1]
  rect(0, n+1- i - 1 + large_bar_space / 2, f, n+1-i - 0.5 - small_bar_space / 2, col=cols[1], border=NA)
  rect(0, n+1-i - 0.5 + small_bar_space / 2, m, n+1-i - large_bar_space / 2, col=cols[2], border=NA)
}
mtext(side=3, at = 0, adj = 0, "Large surplus death rate in South Carolina amongst women", cex=cex_title, line = 4, col = almost_black)
mtext(side=3, at = 0, adj = 0, "Covid death rate as number of deaths since the outbreak per million inhabitants by sex", line = 2, cex=cex_lab, col = dark_gray)
legend_x <- diff(par()$usr)[1] * 0.02
text(legend_x, legend_y, labels=c("Male", "Female"), col = legend_cols[2:1], adj=0, font=2, cex=legend_cex)
rect(sc_women_rect_pos[1], sc_women_rect_pos[2], sc_women_rect_pos[3], sc_women_rect_pos[4],
     col=sc_col, border=NA)
dev.off()




