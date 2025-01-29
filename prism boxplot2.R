
library(ggplot2)
library(ggridges)
prism <- readRDS("prism.rds")

prism$bmi_cut <- cut(prism$bmi_median, 4)
cor.test(prism$bmi_median, prism$sgrq_total_score)

pdf(file = "prismBox.pdf", width = 7, height = 4.5)
par(mar = c(4,4,0.2,0.2))
boxplot(prism$sgrq_total_score~prism$bmi_cut,col = "steelblue",xlab = parse(text='BMI~~(kg/m^{2})'), ylab = "SGRQ score")
text(3.5,250,expression(italic(r)["BMI,"~"SGRQ"]==-0.025~","~italic(P)=="0.010"), cex = 1)

dev.off()
##########

prism$bmi_cut2 <- cut(prism$bmi_median, 4)
prism3=prism %>% drop_na(bmi_cut2)

ggplot(prism3, aes(x = bmi_cut2, y = sgrq_total_score)) +
  geom_boxplot(aes(fill = bmi_cut2), width = 0.4, alpha = 0.8) +
  geom_jitter(aes(x = as.numeric(factor(bmi_cut2)) + 0.4, color = bmi_cut), width = 0.15, alpha = 0.2) +
#  geom_beeswarm(aes(x = as.numeric(factor(bmi_cut2)) + 1, color = bmi_cut2), size = 0.15, alpha = 0.1) +
  annotate("text", x = 7, y = 260, label = expression(italic(r)["BMI,"~"SGRQ"]==-0.025~","~italic(P)=="0.010")
, size = 4, color = "black") +
  labs(title = "Box and Dot Plot for BMI vs SGRQ",
       x = "BMI (kg/m^2)",
       y = "SGRQ Score") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(prism3, aes(x = sgrq_total_score, y = bmi_cut2 ,fill=bmi_cut2)) +
  geom_density_ridges(alpha = 0.2, scale = 0.9, color = NA) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5, color = "black") +
  # Add the text annotation
  annotate("text", x = 100, y = 4, label = expression(italic(r)["BMI,"~"SGRQ"]==-0.025~","~italic(P)=="0.010")
           , size = 4, color = "black") +
  labs(title = "Distribution of SGQR in BMI groups",
       y = "BMI (kg/m^2)",
       x = "SGRQ Score") +
  theme_minimal() +
  theme(legend.position = "none")

################################
################################


# Create the plot
ggplot(prism3, aes(x = sgrq_total_score)) +
  geom_histogram(aes(y = ..density.., fill = bmi_cut2), bins = 60, alpha = 0.3, color = "black") +
  geom_boxplot(aes(y = 0, fill = bmi_cut2), width = 0.1, position = position_nudge(y = -0.1), alpha = 0.6, color = "black") +
  #facet_wrap(~ bmi_cut2, scales = "free_y", ncol = 1) +
  facet_wrap(~ bmi_cut2,  ncol = 1) +
  labs(title = "Distribution of SGRQ in BMI groups",
       x = "SGRQ Score",
       y = "BMI (kg/m^2)",
       caption = expression(italic(r)["BMI,"~"SGRQ"]==-0.025~","~italic(P)=="0.010")) +
  theme_minimal() +
  theme(legend.position = "none",axis.text.y=element_blank())

