rm(list=ls(all=TRUE))

wave1above<-read.csv(file="wave1above.csv",sep=",",header = T)

wave1above <- subset(wave1above, select = c(WKSESL,C1R4MSCL,C1RGSCAL,C1R4RSCL,R1_KAGE,WKPARED,T1INTERN,T1INTERP,P1CONTRO,RACE))
## WKSESL- SES MEASURE
##AGE - CHILD ASSESSMENT 
##T1INTERN - INTERNALIZING PROBLEM BEHAVIOR --5
##C1BMI - BMI
##WKPARED - PARENT HIGHEST EDUCATIONAL LEVEL
## RACE 
## CONTRO - SELF CONTROL -- 5
#5# MINOR - MINORITY STUDENTS -6
#5#INTERP - INTERPERsonal

fit1a_Math <- lm(C1R4MSCL ~ WKSESL+R1_KAGE+WKPARED+T1INTERN+T1INTERP+P1CONTRO+RACE,
                 data=wave1above)

summary(fit1a_Math)
names(fit1a_Math)
coeff1a_Math <- as.data.frame(fit1a_Math$coefficients)


fit1a_Reading <- lm(C1R4RSCL ~ WKSESL+R1_KAGE+WKPARED+T1INTERN+T1INTERP+P1CONTRO+RACE,
                    data=wave1above)
summary(fit1a_Reading)
names(fit1a_Reading)
coeff1a_Reading<-as.data.frame(fit1a_Reading$coefficients)


fit1a_gk <- lm(C1RGSCAL ~ WKSESL+R1_KAGE+WKPARED+T1INTERN+T1INTERP+P1CONTRO+RACE,
               data=wave1above)
summary(fit1a_gk)
names(fit1a_gk)
coeff1a_gk<-as.data.frame(fit1a_gk$coefficients)


################
wave2above<-read.csv(file="wave2above.csv",sep=",",header = T)

wave2above <- subset(wave2above, select = c(WKSESL,C2R4MSCL,C2RGSCAL,C2R4RSCL,S2KMINOR,R2_KAGE,WKPARED,T2INTERN,T2INTERP,P2CONTRO,RACE))


fit2a_Math <- lm(C2R4MSCL ~ WKSESL+R2_KAGE+WKPARED+T2INTERN+T2INTERP+P2CONTRO+RACE+S2KMINOR,
                 data=wave2above)

summary(fit2a_Math)
names(fit2a_Math)
coeff2a_Math <- as.data.frame(fit2a_Math$coefficients)


fit2a_Reading <- lm(C2R4RSCL ~ WKSESL+R2_KAGE+WKPARED+T2INTERN+T2INTERP+P2CONTRO+RACE+S2KMINOR,
                    data=wave2above)
summary(fit2a_Reading)
names(fit2a_Reading)
coeff2a_Reading<-as.data.frame(fit2a_Reading$coefficients)


fit2a_gk <- lm(C2RGSCAL ~ WKSESL+R2_KAGE+WKPARED+T2INTERN+T2INTERP+P2CONTRO+RACE+S2KMINOR,
               data=wave2above)
summary(fit2a_gk)
names(fit2a_gk)
coeff2a_gk<-as.data.frame(fit2a_gk$coefficients)

###################################################


wave3above<-read.csv(file="wave3above.csv",sep=",",header = T)

wave3above <- subset(wave3above, select = c(W1SESL,R3REGION,W1INCCAT,C3R4MSCL,C3RGSCAL,C3R4RSCL,R3AGE,W1PARED,RACE))


fit3a_Math <- lm(C3R4MSCL ~W1SESL+R3REGION+W1INCCAT+R3AGE+W1PARED+RACE,
                 data=wave3above)
summary(fit3a_Math)
names(fit3a_Math)
coeff3a_Math <- as.data.frame(fit3a_Math$coefficients)


fit3a_Reading <- lm(C3R4RSCL ~ W1SESL+R3REGION+W1INCCAT+R3AGE+W1PARED+RACE,
                    data=wave3above)
summary(fit3a_Reading)
names(fit3a_Reading)
coeff3a_Reading<-as.data.frame(fit3a_Reading$coefficients)


fit3a_gk <- lm(C3RGSCAL ~ W1SESL+R3REGION+W1INCCAT+R3AGE+W1PARED+RACE,
               data=wave3above)
summary(fit3a_gk)
names(fit3a_gk)
coeff3a_gk<-as.data.frame(fit3a_gk$coefficients)

#############################################################################



wave4above<-read.csv(file="wave4above.csv",sep=",",header = T)

wave4above <- subset(wave4above, select = c(W1SESL,T4INTERN,T4INTERP,T4CONTRO,S4MINOR,
                                            R4REGION,W1INCCAT,C4R4MSCL,C4RGSCAL,C4R4RSCL,R4AGE,W1PARED,RACE))


fit4a_Math <- lm(C4R4MSCL ~ W1SESL+S4MINOR+R4AGE+W1INCCAT+R4REGION+W1PARED+RACE+T4INTERN+T4INTERP+T4CONTRO,
                 data=wave4above)

summary(fit4a_Math)
names(fit4a_Math)
coeff4a_Math <- as.data.frame(fit4a_Math$coefficients)


fit4a_Reading <- lm(C4R4RSCL ~ W1SESL+S4MINOR+R4AGE+W1INCCAT+R4REGION+W1PARED+RACE+T4INTERN+T4INTERP+T4CONTRO,
                    data=wave4above)
summary(fit4a_Reading)
names(fit4a_Reading)
coeff4a_Reading<-as.data.frame(fit4a_Reading$coefficients)


fit4a_gk <- lm(C4RGSCAL ~ W1SESL+S4MINOR+R4AGE+W1INCCAT+R4REGION+W1PARED+RACE+T4INTERN+T4INTERP+T4CONTRO,
               data=wave4above)
summary(fit4a_gk)
names(fit4a_gk)
coeff4a_gk<-as.data.frame(fit4a_gk$coefficients)

#############################################################################

wave5above<-read.csv(file="wave5above.csv",sep=",",header = T)
colnames(wave5above)
wave5above <- subset(wave5above, select = c(W3SESL,S5MINOR,C5R4MSCL,R5REGION..50.NA.s.,T5CONTRO,
                                            W3INCCAT,C5R2SSCL,C5R4RSCL,R5AGE,W3PARED,
                                            T5INTERN,T5INTERP,RACE))


fit5a_Math <- lm(C5R4MSCL ~ W3SESL+T5CONTRO+S5MINOR+R5AGE+W3PARED+T5INTERN+
                    T5INTERP+RACE+R5REGION..50.NA.s.+W3INCCAT,
                 data=wave5above)

summary(fit5a_Math)
names(fit5a_Math)
coeff5a_Math <- as.data.frame(fit5a_Math$coefficients)


fit5a_Reading <- lm(C5R4RSCL ~ W3SESL+S5MINOR+T5CONTRO+R5AGE
                    +W3PARED+T5INTERN+T5INTERP+RACE+R5REGION..50.NA.s.+W3INCCAT,
                    data=wave5above)
summary(fit5a_Reading)
names(fit5a_Reading)
coeff5a_Reading<-as.data.frame(fit5a_Reading$coefficients)


fit5a_gk <- lm(C5R2SSCL ~ W3SESL+T5CONTRO+S5MINOR+R5AGE+W3PARED+
                  T5INTERN+T5INTERP+RACE+R5REGION..50.NA.s.+W3INCCAT,
               data=wave5above)
summary(fit5a_gk)
names(fit5a_gk)
coeff5a_gk<-as.data.frame(fit5a_gk$coefficients)

############################################################################

wave6above<-read.csv(file="wave6above.csv",sep=",",header = T)

wave6above <- subset(wave6above, select = c(W5SESL,C6R4MSCL,C6R2SSCL,C6R4RSCL,R6AGE,R6REGION,W5INCCAT,S6MINOR,W5PARED,T6INTERN,T6CONTRO,T6INTERP,RACE))


fit6a_Math <- lm(C6R4MSCL ~ W5SESL+R6AGE+S6MINOR+W5PARED+T6INTERN+T6INTERP+RACE+R6REGION+W5INCCAT+T6CONTRO,
                 data=wave6above)

summary(fit6a_Math)
names(fit6a_Math)
coeff6a_Math <- as.data.frame(fit6a_Math$coefficients)


fit6a_Reading <- lm(C6R4RSCL ~W5SESL+R6AGE+S6MINOR+W5PARED+T6INTERN+T6INTERP+RACE+R6REGION+T6CONTRO+W5INCCAT,
                    data=wave6above)
summary(fit6a_Reading)
names(fit6a_Reading)
coeff6a_Reading<-as.data.frame(fit6a_Reading$coefficients)


fit6a_gk <- lm(C6R2SSCL ~W5SESL+R6AGE+S6MINOR+W5PARED+T6INTERN+T6INTERP+RACE+R6REGION+T6CONTRO+W5INCCAT,
               data=wave6above)
summary(fit6a_gk)
names(fit6a_gk)
coeff6a_gk<-as.data.frame(fit6a_gk$coefficients)

############################################################################

wave7above <- read.csv(file="wave7above.csv",sep=",",header = T)

wave7above <- subset(wave7above, select = c(W8SESL,C7R4MSCL,C7R2SSCL,C7R4RSCL,S7MINOR,R7REGION,W8INCCAT,R7AGE,W8PARED,RACE))


fit7a_Math <- lm(C7R2SSCL ~W8SESL+S7MINOR+R7AGE+W8PARED+RACE+R7REGION+W8INCCAT,
                 data=wave7above)

summary(fit7a_Math)
names(fit7a_Math)
coeff7a_Math <- as.data.frame(fit7a_Math$coefficients)


fit7a_Reading <- lm(C7R2SSCL ~W8SESL+S7MINOR+R7AGE+W8PARED+RACE+R7REGION+W8INCCAT,
                    data=wave7above)
summary(fit7a_Reading)
names(fit7a_Reading)
coeff7a_Reading<-as.data.frame(fit7a_Reading$coefficients)



fit7a_gk <- lm(C7R2SSCL ~W8SESL+S7MINOR+R7AGE+W8PARED+RACE+R7REGION+W8INCCAT,
               data=wave7above)
summary(fit7a_gk)
names(fit7a_gk)
coeff7a_gk<-as.data.frame(fit7a_gk$coefficients)



################################################################################

###PLOTTING

z <- c(coeff1a_Math[2,],coeff2a_Math[2,],coeff3a_Math[2,],coeff4a_Math[2,],coeff5a_Math[2,],
       coeff6a_Math[2,],coeff7a_Math[2,])

barplot(z,main="SES impact on MATH",col=c("brown"),names.arg = c("wave1","wave2","wave3","wave4","wave5","wave6","wave7"))

z <- abs(c(coeff1a_Math[12,],coeff2a_Math[12,],coeff4a_Math[36,],coeff5a_Math[20,],
           coeff6a_Math[19,]))

barplot(z,main="INTERNALIZING PROBLEMS impact on MATH",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))


?barplot

z <- abs(c(coeff1a_Math[13,],coeff2a_Math[13,],coeff4a_Math[37,],coeff5a_Math[21,],
           coeff6a_Math[20,]))

barplot(z,main="INTERPERSONAL impact on MATH",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))

######
grep('CONTRO',rownames(coeff5a_Math))
############
z <- abs(c(coeff1a_Math[14,],coeff2a_Math[14,],coeff4a_Math[38,],coeff5a_Math[3,],coeff6a_Math[41,]))

barplot(z,main="SELF-CONTROL impact on MATH",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))

#####################
######################
#Plotting For Reading
z <- c(coeff1a_Reading[2,],coeff2a_Reading[2,],coeff3a_Reading[2,],coeff4a_Reading[2,],coeff5a_Reading[2,],
       coeff6a_Reading[2,],coeff7a_Reading[2,])

barplot(z,main="SES impact on READING",col=c("brown"),names.arg = c("wave1","wave2","wave3","wave4","wave5","wave6","wave7"))

z <- abs(c(coeff1a_Reading[12,],coeff2a_Reading[12,],coeff4a_Reading[36,],coeff5a_Reading[21,],
           coeff6a_Reading[19,]))

barplot(z,main="INTERNALIZING PROBLEMS impact on READING",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))


z <- abs(c(coeff1a_Reading[13,],coeff2a_Reading[13,],coeff4a_Reading[37,],coeff5a_Reading[22,],
           coeff6a_Reading[20,]))

barplot(z,main="INTERPERSONAL impact on READING",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))
######
#grep('CONTRO',rownames(coeff7a_Reading))
############
z <- abs(c(coeff1a_Reading[14,],coeff2a_Reading[14,],coeff4a_Reading[38,],coeff5a_Reading[7,]))

barplot(z,main="SELF-CONTROL impact on READING",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5"))

##############################################################
#Plotting For GK

z <- c(coeff1a_gk[2,],coeff2a_gk[2,],coeff3a_gk[2,],coeff4a_gk[2,],coeff5a_gk[2,],
       coeff6a_gk[2,],coeff7a_gk[2,])

barplot(z,main="SES impact on GK",col=c("brown"),names.arg = c("wave1","wave2","wave3","wave4","wave5","wave6","wave7"))

z <- abs(c(coeff1a_gk[11,],coeff2a_gk[11,],coeff4a_gk[34,],coeff5a_gk[20,],
           coeff6a_gk[19,]))

barplot(z,main="INTERNALIZING PROBLEMS impact on GK",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))


z <- abs(c(coeff1a_gk[12,],coeff2a_gk[12,],coeff4a_gk[36,],coeff5a_gk[21,],
           coeff6a_gk[19,]))

barplot(z,main="INTERPERSONAL impact on GK",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))

######
#grep('INTERP',rownames(coeff7a_gk))
############
z <- abs(c(coeff1a_gk[13,],coeff2a_gk[13,],coeff4a_gk[37,],coeff5a_gk[22,],coeff6a_gk[20,]))

barplot(z,main="SELF-CONTROL impact on GK",col=c("brown"),names.arg = c("wave1","wave2","wave4","wave5","wave6"))

