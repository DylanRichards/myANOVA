myANOVA = function(value, type) {
	loadPackages()

	boxplot(value ~ type)
	type = as.factor(type)
	outAOV = aov(value ~ type)
	outSummaryAOV = summary(outAOV)
	pValAOV = outSummaryAOV[[1]][["Pr(>F)"]][[1]]
	
	if(pValAOV < 0.05){
		#print("Statistical significant finding established (p < 0.05)")
		print("There is a difference (statistically) in values between different groups")
	} else {
		#print("Not significant (p > 0.05)")
		print("There is NO difference between the group's values")
		return("Done")
	}
	
	#print("Check Assumptions")
	norm = isNormal(value, type)
	if(norm){
		#print("Data is normal")
	} else {
		print("Data is NOT normal")
	}
	
	homo = isHomoscedasticity(value, type)
	if(homo){
		#print("Variances are similar. Good.")
	} else {
		#print("Variances are NOT similar. NOT good.")
		print("The data has a differing amount of how much it is spread out")
	}
	
	if(norm && homo){
		#print("Assumptions passed!")
		print("The data has been validated (normal and homogeneity). You can report this finding confidently")
	} else {
		#print("Assumptions did NOT pass. Kruskal-Wallis test?")
		print("The data could not be validated with this method. We could try the Kruskal-Wallis test?")
	}
	
	#print("Post hoc: Check which groups are different")
	postHoc(outAOV, type)
	
	outlier = checkOutliers(value, type)
	if(!outlier){
		#print("No outliers detected")
		print("The data does not contain any unusual points (no outliers)")
	}
}

loadPackages = function(){
	if (!require("lawstat",character.only = TRUE))
    {
      install.packages("lawstat", dep=TRUE, repos="https://ftp.heanet.ie/mirrors/cran.r-project.org/")
    }
	library("lawstat")
}

isNormal = function(value, type){
	for(i in levels(type)){
		outShapiro = shapiro.test(value[type==i])		
		shapPVal = outShapiro$p.value
		
		if(shapPVal < 0.05){
			return (FALSE)
		}
	}
	return (TRUE)
}

isHomoscedasticity = function(value, type){
	outLevene = levene.test(value, type)
	levenePVal = outLevene$p.value
	return (levenePVal > 0.05)
}

postHoc = function(outAOV, type) {
	outTukey = TukeyHSD(outAOV)
	tukeyFrame = data.frame(outTukey$type)
	print(tukeyFrame["p.adj"])
	for(i in tukeyFrame[["p.adj"]]){
		if(i < 0.05){
			cat(sprintf("There is a difference between the two varables where the p.adj is: %s \n", i))
		}
	}

}

checkOutliers = function(value, type){
	isOutlier = FALSE
	for(i in levels(type)){
		outliers = boxplot(value[type==i], plot=FALSE)$out
		if(length(outliers)!=0){
			#cat(sprintf("Outliers found in group: %s \n", i))
			cat(sprintf("Unusual data found in group: %s \n", i))
			print(outliers)
			isOutlier = TRUE
		}
	}
	return (isOutlier)
}