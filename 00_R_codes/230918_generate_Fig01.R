library(ggplot2)
library(dplyr)
library(svglite)

# Figure 1-1
df3 <- read.csv("01_1_phenotype.txt", sep="\t", header=TRUE)
head(df3)
dim(df3)
typeof(df3)

table3 <- df3 %>%
  group_by(sample) %>%
  summarise(mean = mean(value),
            n    = length(sample),
            se   = sd(value)/sqrt(n))

Image3 <- ggplot(data = table3, aes(x=sample, y=mean)) +
  geom_col(fill="gray", color="black") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                position=position_dodge(0.9), width=0.5) +
  labs(x="Accessions", y="Disease index") +
  theme_classic() +
  theme(axis.title=element_text(size=15), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text.x=element_text(size=15), axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_text(size=15), axis.ticks=element_line(size=1)) +
  scale_x_discrete(limits=c("3501","3509","AC2212","Longsweet","ECW30R","CM334","Dempsey","Perennial","TexA","TF68","N.C.","P.C.")) +
  scale_y_continuous(expand=c(0,0), lim=c(0,4.2))

Image3

ggsave(file="Fig1_1.svg", plot=Image3, width=8.96, height=5.41)
ggsave(file="Fig1_1.jpg", plot=Image3, dpi=300)
# Figure 1-2
df <- read.csv("01_2_phenotype.txt", sep="\t", header=TRUE)
head(df)
dim(df)
typeof(df)

table <- df %>%
  group_by(sample) %>%
  summarise(mean = mean(value),
            n    = length(sample),
            se   = sd(value)/sqrt(n))


Image <- ggplot(data = table, aes(x=reorder(sample, -mean), y=mean)) +
  geom_col(fill="gray", color="black") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                position=position_dodge(0.9), width=0.7) +
  labs(x="Individuals", y="Disease index") +
  theme_classic() +
  theme(axis.title=element_text(size=20), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_text(size=20), axis.ticks=element_line(size=1)) +
  scale_y_continuous(expand=c(0,0), lim=c(0,4.2))

ggsave(file="Fig1_2.svg", plot=Image, width=8.96, height=5.41)
ggsave(file="Fig1_2.jpg", plot=Image, dpi=300)

# Figure 1-3
df2 <- read.csv("01_3_phenotype.txt", sep="\t", header=TRUE)
head(df2)
dim(df2)
typeof(df2)

table2 <- df2 %>%
  group_by(sample) %>%
  summarise(mean = mean(value),
            n    = length(sample),
            se   = sd(value)/sqrt(n))

Image2 <- ggplot(data = table2, aes(x=reorder(sample, -mean), y=mean)) +
  geom_col(fill="gray", color="black") +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                position=position_dodge(0.9), width=0.7) +
  labs(x="Individuals", y="Disease index") +
  theme_classic() +
  theme(axis.title=element_text(size=20), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_text(size=20), axis.ticks=element_line(size=1)) +
  scale_y_continuous(expand=c(0,0), lim=c(0,4.2))

ggsave(file="Fig1_3.svg", plot=Image2, width=8.96, height=5.41)
ggsave(file="Fig1_3.jpg", plot=Image2, dpi=300)
