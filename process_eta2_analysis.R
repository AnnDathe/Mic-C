### estimating eta_square to quantify parameter influence on % necromass
### values are given in PNAS Fig. 4
### Jacob Boes, PhD, Dr. Annette Dathe, Cornell University, June 1st 2026

setwd("C:/Users/Your/Path/Here/")

library(effectsize)
library(car)

x <- readRDS("~/R-scripts/Mic-C_GitHub/fout_redFlux_dN005.RData")

# convert to an array, with dimensions (time, variable, iteration)
z = array(unlist(x),dim=c(
  length(x[[1]][[1]]),
  length(x[[1]]),
  length(x)
))

dim(z)

names_z = c("C", "B", "CO2", "Enz", "Exu", "Waste", "deadB",
            "sumfEx", "N", "BN", "Nof", "NEnz", "NExu", "NWaste", "deadNB")

# reading table with all MC parameter information
y = readRDS('~/R-scripts/Mic-C_GitHub/parhdf2_redFlux_dN005.RData')

names_y = c("muO2", "kO2C", "rg", "rm", "re", "d", "rateEnz", "rateExu", "rateWaste",
            "fNBac", "fNEnz", "fNExu", "fNWaste", "BN_0", "rBN", "Nof",  
            "Enz","Exu", "Waste", "fEx", "necro", "end")

# add 1 to hour, they start at 0, already added below
hour=1729
# dN=0.05   # dN=0.1
#hour=1729  #hour=1729
# hour=330  # hour=176
# hour=12   # hour=11

pNecro = z[hour,7,]/(z[hour,4,]+z[hour,5,]+z[hour,6,]+z[hour,7,])*100
p = array(unlist(pNecro))

# we want an analysis based on parhdf2 where fEx is replaced by pNecro
# make sure to pick the correct hour! 
yy <- cbind(y[,], p)
yy$BN_0 <- NULL
yy$rBN <- NULL
yy$Nof <- NULL
yy$fEx <- NULL
yy$end <- NULL
yy$fNWaste <- NULL
write.table(yy, file = "data_redF/redFlux_dN005.txt", sep = "\t")

df <- read.table("data_redF/redFlux_dN005.txt", header = TRUE)
inVar <- colnames(df)[-(13:ncol(df))]

# all parameters but C:N ratios are log-transformed
dfs <- data.frame(df[,1:12])
for (col in inVar) {
  if (col %in% list('fNBac', 'fNEnz', 'fNExu')) {
    dfs[col] <- scale(dfs[col], scale = FALSE)
  }  else 
    dfs[col] <- scale(log(dfs[col]), scale = FALSE)
}

dfs['p'] <- logit(df['p'])   # R recognizes % and automatically divides by 100

comb <- list()
for (x in seq(1, 3)) {
  mx <- combn(inVar, x)
  comb <- append(comb, as.list(data.frame(mx)))
}

formula <- "p ~ "
for (i in seq_along(comb)) {
  formula <- paste0(formula, paste(comb[[i]], collapse = " * "))
  # Add '+' if not the last combination
  if (i != length(comb)) {
    formula <- paste0(formula, " + ")
  }
}

model <- lm(
  formula = as.formula(formula),
  data = dfs,
  contrasts = c('contr.sum','contr.poly')
)

anova <- car::Anova(model, type='III')
# summary(anova)

eta <- effectsize::eta_squared(anova, partial = TRUE)
head(eta)

out2 <- subset(eta, eta$Parameter %in% 
                 c("muO2", "d", "rateEnz", "rateExu", "rateWaste",
                   "kO2C", "re", "rg", "rm", "fNBac", "fNEnz", "fNExu"))

### saving pretty output, but only two digits reported
#sink("out_eta2_redF_dN01_h1729")
#print(out2)
#sink()

write.table(out2, "out_eta2_redF_dN005_h1729.txt")
