#### Script Information ####
# Title of Script: Final_Script_2025.R
# Publication: Lankester, M., Coles, C., Trotter, A. et al. 
#              The Association Between Academic Achievement and Subsequent 
#              Youth Offending: A Systematic Review and Meta-Analysis. 
#              J Dev Life Course Criminology (2025). 
#              https://doi.org/10.1007/s40865-025-00266-9
# GitHub Author: M. Lankester
# GitHub Link: https://github.com/MadisonLankester/AcaAchieve_YouthOffend_Meta
# Last Updated: 06/05/2025

#### 1.0 Install and load required packages ####
# install.packages("readxl")
# install.packages("pillar")
# install.packages("metafor")
# install.packages("clubSandwich")

library(readxl)
library(pillar)
library(metafor)
library(clubSandwich)

#### 2.0 Import Data from Excel ####
Final_Data <- read_excel("Publ_Final_Data_GitHub.xlsx")

#### 3.0 Check Your Data ####
glimpse(Final_Data)

#### 4.0 Fisher’s r-to-z transformation ####
ri <- Final_Data$Final_r # raw r correlation coefficients
ni <- Final_Data$n       # sample sizes
Final_Data <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = Final_Data) # Override Final_Data
glimpse(Final_Data)

#### 5.0 Meta-analysis using random effects model ####
res <-rma(yi,vi,data = Final_Data, method ="REML")
print(res)

#### 6.0 Pooled ES is that of yi. (If transformed r-to-z you need to transform back) ####
mytransf = predict(res, transf=transf.ztor)
print(mytransf)

#### 7.0 Forest Plot ####
windows(width = 16, height = 10)
forest(res, atransf = mytransf, header = "Author(s), Year                                                                                                                                             Weights (%)", 
       xlab = "Fisher's z", slab = paste(Final_Data$Author, Final_Data$Year, sep=", "),
       showweights=TRUE)

text(-1, -0.96, pos=4, bquote(paste(" (Q = ",
                                       .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-0.95, 1.10, pos=4, "2", cex = 0.7)
text(-0.95, 2.10, pos=4, "1", cex = 0.7)

#### 8.0 Assess for Publication Bias - Egger Test ####
reg <- regtest(res, model="rma")
print(reg)

#### 9.0 Assess for Publication Bias - Contour Enhanced Funnel Plot ####
# Contour-enhanced Funnel Plot
# Produce funnel plot - monochrome
windows(width = 15, height = 10)
funnel(res, level=c(90, 95, 99), 
       shade=c("grey95", "grey80", "grey68"), back = "grey55",
       refline=0, legend = TRUE, cex = 0.6, xlab = "Fisher's z")

#### 10.1 Outcome Definition ####
resD <- rma(yi, vi, data=Final_Data, subset=delinq=="delinquency")
print(resD)

resO <- rma(yi, vi, data=Final_Data, subset=delinq=="offending")
print(resO)
 
dat.comp_DvO <- data.frame(estimate = c(coef(resD), coef(resO)), stderror = c(resD$se, resO$se),
                           meta = c("delinquency", "offending"), tau2 = round(c(resD$tau2, resO$tau2),3))
dat.comp_DvO

rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data=dat.comp_DvO, digits=3)

#### 10.2 Exposure measurements - administrative records vs self/informant report ####
resAd_Ex <- rma(yi, vi, data=Final_Data, subset=exp=="admin")
print(resAd_Ex)

resSe_Ex <- rma(yi, vi, data=Final_Data, subset=exp=="self")
print(resSe_Ex)

dat.comp_AS_Exp <- data.frame(estimate = c(coef(resAd_Ex), coef(resSe_Ex)), stderror = c(resAd_Ex$se, resSe_Ex$se),
                           meta = c("admin", "self"), tau2 = round(c(resAd_Ex$tau2, resSe_Ex$tau2),3))
dat.comp_AS_Exp

rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data=dat.comp_AS_Exp, digits=3)

#### 10.3 Outcome measurements - administrative records vs self/informant report ####
resAd_Out <- rma(yi, vi, data=Final_Data, subset=out=="admin")
print(resAd_Out)

resSe_Out <- rma(yi, vi, data=Final_Data, subset=out=="self")
print(resSe_Out)

dat.comp_AS_Out <- data.frame(estimate = c(coef(resAd_Out), coef(resSe_Out)), stderror = c(resAd_Out$se, resSe_Out$se),
                              meta = c("admin", "self"), tau2 = round(c(resAd_Out$tau2, resSe_Out$tau2),3))
dat.comp_AS_Out

rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data=dat.comp_AS_Out, digits=3)

#### 10.4 Follow-up Period ####
resF_s <- rma(yi, vi, data=Final_Data, subset=followup=="small")
print(resF_s)

resF_l <- rma(yi, vi, data=Final_Data, subset=followup=="large")
print(resF_l)

dat.comp_Folup <- data.frame(estimate = c(coef(resF_l), coef(resF_s)), stderror = c(resF_l$se, resF_s$se),
                              meta = c("large", "small"), tau2 = round(c(resF_l$tau2, resF_s$tau2),3))
dat.comp_Folup

rma(estimate, sei=stderror, mods = ~ meta, method = "FE", data=dat.comp_Folup, digits=3)

#### 10.5 Sensitivity analysis ####
robust(res, cluster=cluster, adjust=TRUE)
robust(res, cluster=cluster, clubSandwich=TRUE, adjust=TRUE)


#### END ####