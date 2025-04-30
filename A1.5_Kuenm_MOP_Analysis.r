################################Instalando o pacote#############################
# Installing and loading packages
#Installing the package
# The kuenm R package is in a GitHub repository and can be installed and/or loaded using
# the following code (make sure to have Internet connection). To warranty the package 
# functionality, a crucial requirement is to have the maxent.jar application in any 
# user-defined directory (we encourage you to maintain it in a fixed directory). 
# This software is available in the Maxent repository. Another important requirement 
# for using Maxent and therefore the kuenm package is to have the Java Development Kit 
# installed. The Java Development Kit is available in this repository. Finally, for 
# Windows users, Rtools needs to be installed in the computer; it is important that this
# software is added to the PATH. For instructions on how to download and install it see 
# https://github.com/marlonecobos/kuenm. 


if(!require(devtools)){
    install.packages("devtools")
}

if(!require(kuenm)){
    devtools::install_github("marlonecobos/kuenm")
}

library(kuenm)

########################Downloading the example data############################

# Define your actual directory.

setwd("C:/Herpetofauna_MOP") # set the working directory

dir() # check what is in your working directory

# If you have your own data and they are organized as in the first part of Figure 1, change 
# your directory and follow the instructions below.

############################## Extrapolation risk analysis ###############################
help(kuenm_mmop)
M_var_dir <- "Layers"
G_var_dir <- "T_Layers"
sets_var <- "Set1" # a vector of various sets can be used
out_mop <- "MOP_results"
percent <- 10
paral <- FALSE # make this true to perform MOP calculations in parallel, recommended
               # only if a powerfull computer is used (see function's help)
is_swd <- FALSE
# Two of the variables used here as arguments were already created for previous functions
kuenm_mmop(G.var.dir = G_var_dir, M.var.dir = M_var_dir, is.swd = is_swd, sets.var = sets_var, out.mop = out_mop,
           percent = percent, parallel = paral)	
######################################### End of Code ####################################									   