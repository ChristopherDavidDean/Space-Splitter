# Space-Splitter
Script to generate geographic grid space based on longitude and latitude for fossil occurrences.
Written by Christopher Dean and Dominic Bennett, 2016.

This script assigns fossil occurrence data to grid squares based on their longitude and latitude. This is to allow for statistical manipulation and interrogation of the data at a later date. 

Currently the project contains the following steps.

genLonsandLats - function that generates maximum and minimum values of each grid square, in lists for Latitude and Longitude.
Input of data - set up to allow users to add thier own data from PBDB downloads.
Generation of gridspace - creates a grid using the values from genLonsandLats, with each grid square receiving a unique ID.
Matching of occurences to grid squares - reads fossil occurrences latitude and Longitude and assigns the occurrence to an individual grid square. 
[RESULT] - calls a vector of the total number of occurrences in each grid square.

Future steps will involve reading of occurrence attributes and generating false data from original grid square to allow for statistical testing of hypotheses relating to geographic distribution.
