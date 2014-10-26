Getting And Cleaning Data Project
=================================

Getting and Cleaning Data project

This R script called run_analysis.R does the following:
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 

The run_analysis function do the following:
* do a loop on test files and merge them in a data set
* update the colonnes names of the data set
* write the data set to a txt file called "bind_data.txt"
* do a loop on train files and merge them to a new data set
* write the new data set to the txt file with append=TRUE option

The run_calc function do the following:
* read the file "bind_data.txt" that have been generated by run_analysis function
* calculate the mean and the standard deviation for each measurement using sapply function
* generate the output
