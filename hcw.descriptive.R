# Descriptive analysis
require(RColorBrewer)
#require(corrplot)
require(ggplot2)

# prepare color pallettes
display.brewer.pal(n = 3, name = 'RdBu')
display.brewer.pal(n = 5, name = 'RdBu')
pal3 <- brewer.pal(n = 3, name = "RdBu")
pal4 <- brewer.pal(n = 4, name = "RdBu")
pal5 <- brewer.pal(n = 5, name = "RdBu")
pal6 <- brewer.pal(n = 6, name = "RdBu")

# +++++++ A| descriptive statitics (for table 1) ++++++
# UNIVARIATE 
# sex
table(hcw.data$sex) # 195F, 110M
# age
table(hcw.data$age_gp) # 15<25, 109 25-34, 102 35-44, 54 45-54, 24 55+
# district
table(hcw.data$district) # 183 FT, 122 Kambia
# ethnic group
table(hcw.data$ethnic_gp) # 89 Temne, 87 Mende, 46 Limba, 12 Fula, 20 Mandingo...
# religion
table(hcw.data$rel) # 167 Christian, 138 Muslim
# health centre type
table(hcw.data$health_ctr_type) # 130 Gov hosp (42,6%), 4 private hosp (1,3%), 11 NGO clinic (3,6%), 119 CHC (39,0%), 24 CHP (7,9%), 17 MCHP (5,6%)

# profession
table(hcw.data$profession)

# Descriptive figures: 
# age distribution
ggplot(hcw.data, aes(x=age_gp)) + geom_histogram()

# BIVARIATE
# radio/tv
counts <- table(hcw.data$hh_radio, hcw.data$hh_tv)
legend1 <- c("No radio", "Radio")
barplot(counts, main="",
        xlab="TV ownership", col=pal2[c(1,3)],
        legend = legend1)

# most people own radio (>80%), most people also have tv (>66%). People with tv more likely to have radio.
inc.edu.table <- table(hcw.data$edu_gp, hcw.data$income_gp)
chisq.test(table(hcw.data$edu_gp, hcw.data$income_gp))
prop.table(inc.edu.table, 1) # better educated earn better on average
prop.table(inc.edu.table, 2) # better educated earn better on average

# proportion of income acc to edu group
inc.edu.table2 <- table(hcw.data$ses_gp, hcw.data$edu_gp)
counts2 <- prop.table(inc.edu.table2, 2)
#counts <- table(hcw.data$income_gp, hcw.data$edu_gp)
legend2 <- c("low income", "medium income", "high income")

pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/edu_income.pdf")
par(mfrow=c(1, 1), mar=c(5, 5, 4, 8), las=1)
barplot(counts2, main="",
        xlab="Education", ylab="Proportion", col=pal3[c(1,2,3)],
        legend = legend2, args.legend = list(x = "topright", bty = "n", inset=c(-0.35, 0)), names.arg = c("Low (n=34)", "Mid (n=136)", "High (n=135)"))
dev.off()

# profession by sex
table(hcw.data$sex, hcw.data$profession)
prof.sex.table <- table(hcw.data$sex, as.factor(hcw.data$prof_name))
counts3 <- prop.table(prof.sex.table,2)
legend3 <- c("female", "male")
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/sex_prof.pdf", height = 7, width = 15)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 6))
barplot(counts3, main="",
        xlab="Profession", ylab="Proportion", col=pal3[c(1,3)],
        legend = legend3, args.legend = list(x = "topright", bty = "n", inset=c(-0.07, 0)),
        names.arg = c("Admin(n=8)", "CHO/A(n=23)", "CHW(n=18)", "Support(n=35)", "Doctor(n=4)", "Lab(n=31)", "MCHA(n=24)", "Midwife(n=18)", "Nurse(n=134)", "Pharma(n=7)"))
dev.off()

# HC by district
dist_ctrtype_tbl <- table(hcw.data$health_ctr_type, hcw.data$district)
counts4 <- prop.table(dist_ctrtype_tbl,2)
legend4 <- c("Govt hospital", "Private clinic", "NGO clinic", "CHC", "CHP", "MCHP")
pdf("C:/Users/Mario/Sync/Sierra Leone survey/paper/figures/hctype_district.pdf", height = 7, width = 9)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 7.2))
barplot(counts4, main="",
        xlab="District", ylab="Proportion", col=pal6[1:6],
        legend = legend4, args.legend = list(x = "topright", bty = "n", inset=c(-0.19, 0)),
        names.arg = c("Freetown (n=183)", "Kambia (n=122)"))
dev.off()

# ++++ check whether population representative (asked Hana for more info) ++++
# sex, age...
# sex 63,8% in HRH strategy 62%

# (0.638-0.62)/(sqrt((0.62*(1-0.62))/305))
prop.test(x=195, n =305, p=0.62)

# professions: oversampled those in hospitals (lab workers), undersampled PHUs
# religion,
# profession (42,5% nurses vs. here 44,4%, 4,7% midwives (5.9%), 5,1% lab (10,2%), 28% MCHA (7,9%), 9,2% CHO(7,5%))
table(hcw.data$profession) # oversampled lab workers undersampled MCHA
# SES?
# volunteer
table(hcw.data$payroll) # 41,0% volunteers (vs. 3600/13500: 26,5%)



# correlations between vars

# calculate correlations between vars
# correlations <- cor(hcw.data[,c(1,3,5,6,7,8,9,10,11,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)])
# corrplot(correlations, method="circle")