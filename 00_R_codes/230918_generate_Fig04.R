library(ggplot2)
library(dunn.test)

samplename <- "GWAS_marker1"

df <- read.csv(paste0(samplename,".tsv"),sep="\t",head=TRUE)
head(df)
df$genotype <- as.factor(df$genotype)
df <- df[!(df$genotype == "-" ), ]
#df <- df[!(df$genotype == "H" ), ]
head(df)
df$genotype
genotype <-c("A","C") # Modify genotypes according to df$genotype results.

model_fin = kruskal.test(value ~ genotype, data=df)
model_fin
dunn.test(df$value, df$genotype) # Run if p-value in model_kruskal < 0.05;

p <- ggplot(data=df) +
  geom_boxplot(aes(x=genotype, y=value, fill=factor(genotype)), show.legend=FALSE) +
  theme_classic() +
  stat_summary(aes(x=genotype, y=value), geom="point", fun="mean", shape=2, size=5) +
  stat_summary(aes(x=genotype, y=value, label=round(after_stat(y), 1)), geom='text', fun.y='mean', vjust = 1.5, size=5) +
  labs(x="Genotype",y="Disease scale") +
  theme(axis.title=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  scale_y_continuous(limits=c(0,5)) +
  scale_x_discrete(limits=genotype) # Only for QTL SNPs (A, H, B) 
  #geom_signif(comparisons = list(c("A", "G"),c("A", "H"),c("H", "G")), map_signif_level = TRUE, step_increase = 0.1)
#  geom_text(aes(x=c(1), y=3.9, label=c("*")), vjust=-0.5, size=10, position=position_dodge(0.5))

p
dev.off()

# Save files.
png(filename=paste0(samplename,".png"), width=800,height=600,bg="transparent")
p
dev.off()
svg(filename=paste0(samplename,".svg"), width=800,height=600,bg="transparent")
p
dev.off()
