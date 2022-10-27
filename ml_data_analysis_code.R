#############################
##
## ML DATA ANALYSIS CODE
##
#############################

#
#
# ANOVAs with post-hocs
#
#

dv_gv_AOV <- aov(lm(dv ~ as.factor(gv), data = d)) # dv is the dependent variable
TukeyHSD(dv_gv_AOV)                                # and gv is the group variable

pander(summary(aov(dv ~ gv, data = d)))
pander(summarySE(d, groupvar="gv", measurevar="dv",na.rm=TRUE),round=2)


#
#
# Correlation matrix
#
#

# Creating a function to create nice correlation matrices

cortable <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  ## build a new matrix that includes the correlations with their appropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
} 

# Example code for a correlation matrix

ex_corr <- cortable(d[c("v1", "v2", "v3", "v4", "v5", "v6")])

pander(ex_corr,round=2,title = "Item Correlations")


#
#
# Creating great tables and graphs
#
#



#
#
# EFA
#
#




#
#
# CFA
#
#




#
#
# Path analysis and SEM
#
#




#
#
# Reliability / Cronbach's alpha coefficients
#
#




#
#
# Linear regression
#
#




#
#
# Logistic regression
#
#




#
#
# Tests of baseline equivalence
#
#




#
#
# Multilevel models / HLMs
#
#




#
#
# Response surface analysis
#
#




#
#
# Cluster analysis
#
#




#
#
# Multigroup analysis (testing indirect effects in a path model for different demographic groups)
#
#



#
#
# Profile analysis
#
#




#
#
# Power analysis
#
#



#
#
# Bayesian predictive models
#
#




#
#
# Mediation/moderation analysis
#
#




#
#
# Propensity score matching
#
#




#
#
# Creating a color scheme to match our ML branding and use in our graphs
#
#





