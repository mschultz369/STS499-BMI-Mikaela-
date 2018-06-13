#R Script for generating cross-sectional and longitudinal BMI Growth Curves 

# Dr. V commented out packages after first installation
#Install necessary R packages for program to run
#install.packages("openxlsx")
#install.packages("gWidgets")
#install.packages("gWidgetstcltk")
#install.packages("RColorBrewer")
#install.packages("readxl")
#install.packages("extrafont")
#install.packages("dplyr")
#install.packages("formattable")

library(openxlsx)
library(gWidgets)
library(gWidgetstcltk)
library(RColorBrewer)
library(readxl)
library(base)
library(stats)
library(extrafont)
library(dplyr)
library(formattable)

# add this to avoid their weird package that doesn't work
library(readxl)

# COMMENTED OUT
#Choose File containing Sex, Age, BMI data for your sample of patients (boys and girls)
#Use the GUI to look up file for BMI_Data.xlsx
#sourceFile = gfile(text="Select your data file", type="open", filter = list("All files" = list(patterns = c("*"))))
#bmi_data = read_excel(sourceFile)


# Dr. V's way to read in the data -- change file path
bmi_data <- read_excel("~/Desktop/Student Related/STS499/Student - Mikaela Schultz/Article and Data Files/Test Data/BMI_Data_Test.xlsx")
head(bmi_data) 

bdata = bmi_data[!is.na(bmi_data$ID ==""),]
head(bdata)
dim(bdata)

ht = as.numeric(bdata$Height_cm)
wt = as.numeric(bdata$Weight_kg)
bdata$BMI_kgm2 = ifelse(!is.na(bdata$Height_cm)& !is.na(bdata$Weight_kg), wt/((ht/ 100)*(ht / 100)) , bdata$BMI)
bdata

#Pools the data for the boys
m_input1 = subset(bdata, Sex=="male", select=c(ID, Age_y, BMI_kgm2))
m_input2 = subset(bdata, Sex=="Male", select=c(ID, Age_y, BMI_kgm2))
m_input3 = subset(bdata, Sex=="m", select=c(ID, Age_y, BMI_kgm2))
m_input4 = subset(bdata, Sex=="M", select=c(ID, Age_y, BMI_kgm2))
m_input = rbind(m_input1, m_input2, m_input3,m_input4)
head(m_input)
dim(m_input)

#Pools the data for the girls 
f_input1 = subset(bdata, Sex=="female", select=c(ID, Age_y, BMI_kgm2))
f_input2 = subset(bdata, Sex=="Female", select=c(ID, Age_y, BMI_kgm2))
f_input3 = subset(bdata, Sex=="f", select=c(ID, Age_y, BMI_kgm2))
f_input4 = subset(bdata, Sex=="F", select=c(ID, Age_y, BMI_kgm2))
f_input = rbind(f_input1, f_input2, f_input3, f_input4)
head(f_input)
dim(f_input)


# COMMENTED OUT
#Import data from the CDC dataset (Ref_percentile_curves.xlsx)
#Choose file with the CDC data;
#cdcFile = gfile(text="Select the CDC reference file", type="open", filter = list("All files" = list(patterns = c("*"))))
#CDC dataset for the boys
#cdc_mdata = read_excel(cdcFile, sheet=1)
#cdc_mdata$age_m = cdc_mdata$AgeInMonths / 12
#head(cdc_mdata)
#CDC dataset for the girls
#cdc_fdata = read.xlsx(cdcFile, sheet=2)
#cdc_fdata$age_m = cdc_fdata$AgeInMonths / 12
#head(cdc_fdata)

# Dr. V's way to read in CDC datasets for boys/girls - change file paths
cdc_mdata <- read_excel("~/Desktop/Student Related/STS499/Student - Mikaela Schultz/Article and Data Files/5_Ref_percentile_curves.xlsx", 
                                       sheet = "Males, 2-20 years")
cdc_mdata$age_m = cdc_mdata$AgeInMonths / 12
cdc_fdata <- read_excel("~/Desktop/Student Related/STS499/Student - Mikaela Schultz/Article and Data Files/5_Ref_percentile_curves.xlsx", 
                                       sheet = "Females, 2-20 years")
cdc_fdata$age_m = cdc_fdata$AgeInMonths / 12

#Plot for Males:
pointsize <- 1
loadfonts()
#Modify name of the graphic output pdf file
pdf("BMI_Graph_males_R.pdf", family="Times")
par(mar=c(4.4,5,2,1))
par(oma=c(1,1,0,0))
par(las=1)
plot(x=m_input$Age_y, y=m_input$BMI_kgm2, xlab="", ylab=expression(bold(paste("Body Mass Index (kg/m"^"2",")"))), type="n", axes=FALSE, xlim=c(2,30), ylim=c(10,60))
title("Males", line=-0.5, adj=0)
mtext("Age (years)", side=1, line=3, adj=0.35, font=2)
points(x=m_input$Age_y, y=m_input$BMI_kgm2, type="p", pch="o", cex=pointsize)
axis(side =1, at=c(2, 20), labels = c("",""), lwd.ticks=0)
axis(side =1, at= seq(2, 20, by=2), lwd=0, lwd.ticks=1)
axis(side =2, at= seq(10, 60, by=2), cex.axis=0.8)
lines(cdc_mdata$age_m, cdc_mdata$m5,type="l", lwd=2, col="black")
lines(cdc_mdata$age_m, cdc_mdata$m50, type="l", lwd=2, col="green4")
lines(cdc_mdata$age_m, cdc_mdata$m85, type="l", lwd=2, col="mediumblue")
lines(cdc_mdata$age_m, cdc_mdata$m95, type="l", lwd=2, col="red1")
lines(cdc_mdata$age_m, cdc_mdata$mSevereC2, type="l", lwd=2, col="tan4")
lines(cdc_mdata$age_m, cdc_mdata$mSevereC3, type="l", lwd=2, col="orange")
len = unique(m_input$ID)
table = aggregate(data.frame(count=m_input$ID),list(value=m_input$ID), length)
if (table$count[1] > 1){
for (i in len) 
{
	lines(m_input$Age_y[m_input$ID==i], m_input$BMI_kgm2[m_input$ID==i],type="l", lty=1, col=i+5)
}}
text(20, 19, expression(bold(paste("5"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 23, expression(bold(paste("50"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 28.5, "Overweight: ", cex=0.8, pos=4, font=2)
text(20, 27, expression(bold(paste("85"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 32.5, "Obese Class 1: ", cex=0.8, pos=4,font=2)
text(20, 31, expression(bold(paste("95"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 38.5, "Severe Obesity Class 2: ", cex=0.8, pos=4, font=2)
text(20, 37, expression(bold(paste("120% of 95"^"th"," percentile"))), cex=0.8, pos=4)
text(20, 44.5, "Severe Obesity Class 3: ", cex=0.8, pos=4, font=2)
text(20, 43, expression(bold(paste("140% of 95"^"th", " percentile"))), cex=0.8, pos=4)

dev.off()


#Plot for Females:
#Modify name of the graphic output pdf file
pdf("BMI_Graph_females_R.pdf", family="Times")
par(mar=c(4.4,5,2,1))
par(oma=c(1,1,0,0))
par(las=1)
plot(x=f_input$Age_y, y=f_input$BMI_kgm2, xlab="", ylab=expression(bold(paste("Body Mass Index (kg/m"^"2",")"))), type="n", axes=FALSE, xlim=c(2,30), ylim=c(10,60))
title("Females", line=-0.5, adj=0)
mtext("Age (years)", side=1, line=3, adj=0.35, font=2)
points(x=f_input$Age_y, y=f_input$BMI_kgm2, type="p", pch="o", cex=0.3)
axis(side =1, at=c(2, 20), labels = c("",""), lwd.ticks=0)
axis(side =1, at= seq(2, 20, by=2), lwd=0, lwd.ticks=1)
axis(side =2, at= seq(10, 60, by=2), cex.axis=0.8)
lines(cdc_fdata$age_m, cdc_fdata$f5,type="l", lwd=2, col="black")
lines(cdc_fdata$age_m, cdc_fdata$f50, type="l", lwd=2, col="green4")
lines(cdc_fdata$age_m, cdc_fdata$f85, type="l", lwd=2, col="mediumblue")
lines(cdc_fdata$age_m, cdc_fdata$f95, type="l", lwd=2, col="red1")
lines(cdc_fdata$age_m, cdc_fdata$fSevereC2, type="l", lwd=2, col="tan4")
lines(cdc_fdata$age_m, cdc_fdata$fSevereC3, type="l", lwd=2, col="orange")
len = unique(f_input$ID)
table = aggregate(data.frame(count=m_input$ID),list(value=m_input$ID), length)
if (table$count[1] > 1){
for (i in len) 
{
	lines(f_input$Age_y[f_input$ID==i], f_input$BMI_kgm2[f_input$ID==i],type="l", lty=1, col=i+5)
}}
text(20, 18, expression(bold(paste("5"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 22, expression(bold(paste("50"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 28.5, "Overweight: ", cex=0.8, pos=4, font=2)
text(20, 27, expression(bold(paste("85"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 33.5, "Obese Class 1: ", cex=0.8, pos=4, font=2)
text(20, 32, expression(bold(paste("95"^"th", " percentile"))), cex=0.8, pos=4)
text(20, 39.5, "Severe Obesity Class 2: ", cex=0.8, pos=4, font=2)
text(20, 38, expression(bold(paste("120% of 95"^"th"," percentile"))), cex=0.8, pos=4)
text(20, 46.5, "Severe Obesity Class 3: ", cex=0.8, pos=4, font=2)
text(20, 45, expression(bold(paste("140% of 95"^"th", " percentile"))), cex=0.8, pos=4)

dev.off()

#End of the Graphing Code



#Results output
bdata$agemos = bdata$Age_y*12
bdata$bmi = bdata$BMI_kgm2
young = which(bdata$agemos < 240)
bdata = bdata[young,]
mydata = bdata
dim(mydata)
head(mydata)

for(i in 1:length(mydata$Sex)){
if( mydata$Sex[i]=="male" || mydata$Sex[i]=="Male" || mydata$Sex[i]=="M" || mydata$Sex[i]=="m"){
	mydata$SEX[i]= 1
}
if( mydata$Sex[i]=="female" || mydata$Sex[i]=="Female" || mydata$Sex[i]=="F" || mydata$Sex[i]=="f"){
	mydata$SEX[i]= 2
}
}

mydata$height = mydata$Height_cm
mydata$weight = mydata$Weight_kg

for(i in 1:length(mydata$agemos)){
if (0 <= mydata$agemos[i] && mydata$agemos[i] < 24){
	mydata$length[i] = mydata$height[i];
	mydata$height[i] = NA;
}
if(24 <= mydata$agemos[i]){
	mydata$stature[i]=mydata$height[i];
	mydata$lenhei[i]=mydata$height[i];
}
}
head(mydata)

#_cinage
for(i in 1:length(mydata$agemos)){
if (mydata$agemos[i]>=0 && mydata$agemos[i] < 0.5){
	mydata$X_AGECAT[i]=0;
}
else{
	mydata$X_AGECAT[i] = as.integer(mydata$agemos[i]+0.5)-0.5;
}
if(is.na(mydata$bmi[i]) && mydata$weight[i] > 0 && mydata$lenhei[i] > 0 && mydata$agemos[i] >=24){
	mydata$bmi[i]=  mydata$weight[i] / (mydata$lenhei[i]/100)^2;
}
}
head(mydata)

#Input the cdcref_d reference dataset here
cdcref = read.csv("cdcref_d.csv")
crefage = filter(cdcref, denom=="age")
finfage = merge(mydata, crefage, by.x=c("SEX","X_AGECAT"), by.y=c("SEX","X_AGECAT"), all.x=TRUE)
head(finfage)
dim(finfage)
finfage$ageint = finfage$X_AGEMOS2 - finfage$X_AGEMOS1
finfage$dage = finfage$agemos - finfage$X_AGEMOS1
head(finfage)

array11 = finfage[, c("X_LLG1", "X_MLG1", "X_SLG1", "X_LHT1", "X_MHT1", "X_SHT1", "X_LWT1", "X_MWT1", "X_SWT1", "X_LHC1", "X_MHC1", "X_SHC1", "X_LBMI1", "X_MBMI1", "X_SBMI1")]
head(array11)
array12 = finfage[, c("X_LLG2", "X_MLG2", "X_SLG2", "X_LHT2", "X_MHT2", "X_SHT2", "X_LWT2", "X_MWT2", "X_SWT2", "X_LHC2", "X_MHC2", "X_SHC2", "X_LBMI2", "X_MBMI2", "X_SBMI2")]
head(array12)
array10 = array11 + (finfage$dage*(array12 - array11))/finfage$ageint
head(array10)

head(finfage)
#Rename variables
names(array10) = c("X_LLG", "X_MLG", "X_SLG", "X_LHT", "X_MHT", "X_SHT", "X_LWT", "X_MWT", "X_SWT", "X_LHC", "X_MHC", "X_SHC", "X_LBMI", "X_MBMI", "X_SBMI")

finfage = cbind(finfage, array10)

 for(i in 1:length(finfage$bmi)){ 
	if (finfage$bmi[i] > 0) {
		if (abs(finfage$X_LBMI[i]) >= 0.01) {
		  finfage$bmiz[i] = ((finfage$bmi[i]/finfage$X_MBMI[i])^(finfage$X_LBMI[i])-1)/(finfage$X_LBMI[i] * finfage$X_SBMI[i])
		} else if (!is.na(abs(finfage$X_LBMI[i]))  && abs(finfage$X_LBMI[i]) < 0.01) {
		  finfage$bmiz[i] = log(finfage$bmi[i]/finfage$X_MBMI[i])/finfage$X_SBMI[i]
		}
	  	finfage$bmipct[i] = pnorm(finfage$bmiz[i])*100;
		sdl = ((finfage$X_MBMI[i] - finfage$X_MBMI[i]*(1 - 2 * finfage$X_LBMI[i] * finfage$X_SBMI[i])^(1/finfage$X_LBMI[i]))/2);
		sdh = ((finfage$X_MBMI[i]*(1+2*finfage$X_LBMI[i]*finfage$X_SBMI[i])^(1/finfage$X_LBMI[i]) - finfage$X_MBMI[i])/2);
		if (finfage$bmi[i] < finfage$X_MBMI[i]) {
		  finfage$Fbmiz[i] = (finfage$bmi[i] - finfage$X_MBMI[i])/sdl
		}else {
		  finfage$Fbmiz[i] = (finfage$bmi[i] - finfage$X_MBMI[i])/sdh
		}
	}
	finfage$bmi95[i] = finfage$X_MBMI[i] * ((1 + finfage$X_LBMI[i]*finfage$X_SBMI[i]*qnorm(0.95))^(1/finfage$X_LBMI[i]));
	finfage$bmipct95[i] = 100*(finfage$bmi[i]/finfage$bmi95[i]);
	
	if(finfage$bmipct[i] >= 0 && finfage$bmipct[i] < 5){
		finfage$Weight_Status[i] = "Underweight";
	}else if (finfage$bmipct[i] >= 5 && finfage$bmipct[i] < 85){
		finfage$Weight_Status[i] = "Healthy Weight";
	}else if (finfage$bmipct[i] >= 85 && finfage$bmipct[i] < 95){ 
		finfage$Weight_Status[i] = "Overweight";
	}else if (finfage$bmipct[i]>= 95 && finfage$bmipct95[i] < 120){ 
		finfage$Weight_Status[i] = "Obese Class 1";
	}else if (finfage$bmipct95[i] >= 120 && finfage$bmipct95[i] < 140){ 
		finfage$Weight_Status[i] = "Severe Obesity Class 2";
	}else  if (finfage$bmipct95[i] >= 140){
		finfage$Weight_Status[i] = "Severe Obesity Class 3";
	}
  }
head(finfage)


output = finfage[ ,c("ID", "Sex", "Age_y", "Height_cm", "Weight_kg", "BMI", "BMI_kgm2", "bmipct", "bmiz", "bmipct95", "Weight_Status")]
names(output) = c("ID", "Sex", "Age_y", "Height_cm", "Weight_kg", "BMI", "BMI_kgm2", "BMI_pct", "BMI_z", "BMI_95", "Weight_Status")
out = output[order(output$ID, output$Sex),]
formattable(c(out$Age_y, out$Height_cm, out$Weight_kg, out$BMI_kgm2), digits=3, format="f")
#Output CSV file
write.csv(out, file = "BMI_Results_long_R.csv")

