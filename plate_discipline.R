install.packages("ggplot2")
install.packages("GGally")
install.packages("ggrepel")
library(ggplot2)
library(GGally)
library(ggrepel)

install.packages("readxl")
library(readxl)

# colors
green = "#228B22"

plate.discipline.data = read_excel("discipline.xlsx")
attach(plate.discipline.data)

al.teams = c("Angels", "Astros", "Rangers", "Athletics", "Mariners",
             "Indians", "White Sox", "Tigers", "Twins", "Royals",
             "Yankees", "Red Sox", "Rays", "Blue Jays", "Orioles")

nl.teams = c("Dodgers", "Rockies", "Padres", "Giants", "Diamondbacks",
             "Cubs", "Cardinals", "Brewers", "Reds", "Pirates",
             "Nationals", "Mets", "Marlins", "Phillies", "Braves")


##### K and BB #####

# K hist
qplot(K, main = "Frequency of K%")
summary(K)

# BB hist
qplot(BB, main = "Frequency of BB%")
summary(BB)

# (K, BB) plot with linear regression line
qplot(K, BB, main = "K%  v. BB%") +
  geom_smooth(method=lm)

k.bb.reg = lm(BB~K)
summary(k.bb.reg)


# Compare by subtraction
ggplot(plate.discipline.data, aes(x = K - BB)) +
  geom_histogram() +
  ggtitle("Frequency of K% - BB%")

qplot(K, BB, main = "K%  v. BB%, with K% - BB% tails labeled") +
  geom_text_repel(aes(label=ifelse((K - BB)>0.20,as.character(Name),'')), color="red") +
  geom_text_repel(aes(label=ifelse((K - BB)<0.00,as.character(Name),'')), color=green) + 
  geom_smooth(method=lm)

k.bb.m.high = ifelse((K - BB)>0.20,as.character(Name),'')
k.bb.m.low = ifelse((K - BB)<0.00,as.character(Name),'')


qqnorm(K - BB)
qqline(K - BB)
summary(K - BB)

# Compare by ratio
ggplot(plate.discipline.data, aes(x = K / BB)) +
  geom_histogram() +
  ggtitle("Frequency of K% / BB%")

qplot(K, BB, main = "K%  v. BB%") +
  geom_text_repel(aes(label=ifelse((K / BB)>6,as.character(Name),'')), color="red") +
  geom_text_repel(aes(label=ifelse((K / BB)<1,as.character(Name),'')), color=green) + 
  geom_smooth(method=lm)

k_bb_r_low = ifelse((K / BB)>6,as.character(Name),'')
k_bb_r_high = ifelse((K / BB)<1,as.character(Name),'') 



qqnorm(K / BB)
qqline(K / BB)

# Compare hitters by League
ggplot(plate.discipline.data, aes(x = K, y = BB, color = Team %in% nl.teams)) + 
  geom_point() + 
  ggtitle("K%  v. BB% by League") + 
  scale_color_discrete(name = "League", labels = c("AL", "NL"))

al.hitters = subset(plate.discipline.data, Team %in% al.teams)
nl.hitters = subset(plate.discipline.data, Team %in% nl.teams)

# T test K vs. BB
t.test(al.hitters$K / al.hitters$BB, nl.hitters$K / nl.hitters$BB)
t.test(al.hitters$K - al.hitters$BB, nl.hitters$K - nl.hitters$BB)


##### Pairs #####
ggpairs(plate.discipline.data[-c(1,2,14)])


# K ~ Contact
contact.k.reg = lm(K ~ Contact)
summary(contact.k.reg)

ggplot(plate.discipline.data, aes(x = Contact, y = K)) + 
  geom_point() + 
  ggtitle("K% ~ Contact%") +
  labs(x="Contact%",y="K%") + 
  geom_text_repel(aes(label=ifelse((contact.k.reg$residuals)>0.05,as.character(Name),'')), color = "blue") +
  geom_text_repel(aes(label=ifelse((contact.k.reg$residuals)<(-0.05),as.character(Name),'')), color = "orange") +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_smooth(method=lm)

pos_contact_k_res = ifelse((contact.k.reg$residuals)>0.05,as.character(Name),'')
neg_contact_K_res = ifelse((contact.k.reg$residuals)<(-0.05),as.character(Name),'')




# BB ~ Contact
contact.bb.reg = lm(BB ~ Contact)
summary(contact.bb.reg)

ggplot(plate.discipline.data, aes(x = Contact, y = BB)) + 
  geom_point() + 
  ggtitle("BB%  ~ Contact %") +
  labs(x="Contact",y="BB") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_text_repel(aes(label=ifelse(Name %in% pos_contact_k_res,as.character(Name),'')), color = "blue") + 
  geom_text_repel(aes(label=ifelse(Name %in% neg_contact_K_res,as.character(Name),'')), color = "orange") +
  geom_smooth(method=lm)


# (K-BB) ~ Contact

contact.k.bb.reg = lm((K-BB) ~ (Contact))
summary(contact.k.bb.reg)

ggplot(plate.discipline.data, aes(x = Contact, y = K - BB)) + 
  geom_point() + 
  ggtitle("(K% - BB%)  ~ Contact %") +
  labs(x = "Contact%", y="K% - BB%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_text_repel(aes(label=ifelse(Name %in% pos_contact_k_res,as.character(Name),'')), color = "blue") + 
  geom_text_repel(aes(label=ifelse(Name %in% neg_contact_K_res,as.character(Name),'')), color = "orange") +
  geom_text_repel(aes(label=ifelse(abs(contact.k.bb.reg$residuals)>0.07,as.character(Name),''))) + 
  geom_smooth(method=lm)

pos.contact.k.bb.res = ifelse(contact.k.bb.reg$residuals>0.07,as.character(Name),'')
neg.contact.k.bb.res = ifelse(contact.k.bb.reg$residuals<(-0.07),as.character(Name),'')


# K ~ SwStr

swstr.k.reg = lm(K ~ SwStr)
summary(swstr.k.reg)

ggplot(plate.discipline.data, aes(x = SwStr, y = K)) + 
  geom_point() + 
  ggtitle("K%  ~ SwStr %") +
  labs(x="SwStr%",y="K%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_text_repel(aes(label=ifelse(Name %in% pos.contact.k.bb.res,as.character(Name),'')), color = "blue") + 
  geom_text_repel(aes(label=ifelse(Name %in% neg.contact.k.bb.res,as.character(Name),'')), color = "orange") +
  geom_smooth(method=lm)



# BB ~ SwStr

swstr.bb.reg = lm(BB ~ SwStr)
summary(swstr.bb.reg)

ggplot(plate.discipline.data, aes(x = SwStr, y = BB)) + 
  geom_point() + 
  ggtitle("BB%  ~ SwStr %") +
  labs(x="SwStr%",y="BB%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_text_repel(aes(label=ifelse(Name %in% pos.contact.k.bb.res,as.character(Name),'')), color = "blue") + 
  geom_text_repel(aes(label=ifelse(Name %in% neg.contact.k.bb.res,as.character(Name),'')), color = "orange")


# BB vs. F Strike 

fstrike.bb.reg = lm(BB ~ F.Strike)
summary(fstrike.bb.reg)

ggplot(plate.discipline.data, aes(x = F.Strike, y = BB)) + 
  geom_point() + 
  ggtitle("BB%  ~ F.Strike %") +
  labs(x="F.Strike%",y="BB%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_smooth(method=lm)

# BB vs. Swing

swing.bb.reg = lm(BB ~ Swing)
summary(swing.bb.reg)

ggplot(plate.discipline.data, aes(x = Swing, y = BB)) + 
  geom_point() + 
  ggtitle("BB%  ~ Swing %") +
  labs(x="Swing%",y="BB%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_smooth(method=lm)

# BB vs. O.Swing

oswing.bb.reg = lm(BB ~ O.Swing)
summary(oswing.bb.reg)

ggplot(plate.discipline.data, aes(x = O.Swing, y = BB)) + 
  geom_point() + 
  ggtitle("BB%  ~ O.Swing %") +
  labs(x="O.Swing%",y="BB%") + 
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.low,as.character(Name),'')), color = green) +
  geom_text_repel(aes(label=ifelse(Name %in% k.bb.m.high,as.character(Name),'')), color = "red") +
  geom_smooth(method=lm)


