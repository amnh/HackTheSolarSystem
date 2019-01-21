Documentation for the Best Fit Circle Process for Comet Particle Track Analysis

This distribution should contain the following: an ImageJ/Fiji plugin called 
“FindMaximaPerSlice.txt”, a C++ source file called “BestFitCircleProgram.cpp”, 
an executable called “BestFit.exe”, and an initially empty directory titled 
“data_files”.

The purpose of this distribution is to determine the volume of a comet particle 
track from a series of cross-sectional images. Each image of this sequence should 
be a tif file, meaning that the entire track is essentially a 3D image itself. 
Through this process, each of these cross-sectional images are fed through the 
image processor ImageJ/Fiji, which determines a list of data points based on 
the maximum valued pixels contained in each image. These data points are saved 
in separate data files in the directory “data_files”. For more specific details 
and instructions on how to run this step, see the below section titled “Step 1: 
ImageJ/Fiji Plugin”. Next, each data set is individually analyzed for its best 
fit circle. The area of these best fit circles are added together to yield the 
cumulative volume of the entire track. The file “Areas.txt” created in this 
step contains details for each image slice, including the area, radius, and 
approximate center. The resulting volume should display automatically, but 
will also output as the last line in “Areas.txt”.  For more specific details, 
see the below section titled “Step 2: Finding the Best Fit”.


/************************* How to Use This Software ***************************/

Step 1: ImageJ/Fiji Plugin:

First and foremost, be sure that the directory “data_files” exists in the 
parent directory of this distribution. If it does not exist, make a directory 
and title it “data_files”. 

Next, open up the text file titled “FindMaximaPerSlice.txt” and locate the 
path variables. Please specify the path to the track’s cross-sectional images 
using the same syntax listed in Windows, replacing the “\\” with “/” in Linux 
or MacOSX. The images should have the same name except for the last few digits, 
which represent their position in the track. This software is capable of analyzing 
tracks with a total number of cross-sections between 1,000 and 9,999 because 
of the way the plugin determines the current image’s name. By changing the number 
of leading zeros at each threshold, this limit can be altered. See the in-file 
comments for details. 

Once you have the path to the tracks specified, specify the location of this parent
directory. If there is no “data_files” directory in this parent directory, and/or
the path to this directory is not specified correctly, then the plugin will not 
be able to save any of the data points for later steps. 
 
Next, look for the track length variables. You have the option of analyzing 
however many images slices you would like. Simply set the first track length 
variable to whichever slice you wish to start at, and the second variable to 
the last slice you would like to analyze. DO NOT INCLUDE LEADING ZEROS which 
may be contained in the name of the track. For example, if the first track in 
the file is “track_path/track_name0000.tif”, then set the first variable to 0, 
not 0000. The leading zeros will be accounted for later on (see the first 
paragraph in this section for details).

Once you have the track length and path variables properly specified, 
open ImageJ/Fiji (it doesn’t matter which in this case) and click the menu 
option “Plugins”. On the drop down menu, hover over the option “Macros”, and 
select the option “Run…” In the browsing window, navigate to this folder, or 
type in the system path. Select the plugin “FindMaximaPerSlice.txt” and hit 
open. The plugin will then start running. It will take quite a while to complete 
the full procedure, but there’s no need to worry about memory overflow as no 
more than one image is open at a time. 

Once this step is completed, there should be a data file for each image slice 
contained within the directory “data_files”.



Step 2: Finding the Best Fit

To find the approximate volume for this length of track, run the executable 
file “BestFit.exe”. You can do this by navigating to the appropriate directory 
via the command line and typing “BestFit” or by opening the directory in the 
graphical user interface and double clicking the executable file. The following 
text should appear in the console:
	
	“Best Fit Circle Program
	Make sure that you have run the ImageJ/Fiji plugin "FindMaximaPerSlice.txt" 
	prior to running this software.
	1. Process Solo Image
	2. Process Image Stack
	How would you like to use this software?”

Option 1 allows the user to analyze an individual image slice. This option will 
display the results of the best fit circle calculation directly on the console, 
as well as create a data file containing the data points of this best fit circle 
in a text file titled “BestFitCircle.txt”. To use this option, the user must have 
a data file titled “data{i}.txt”, where {i} is the index the program will ask the 
user to specify, or they must know the location and name of a specific file they 
wish to use. The format of the files inputted into this software is very particular
due to the way in which ImageJ/Fiji outputs the data points. In order to run this 
software successfully, any file inputted must have two elements on the first line, 
and three on each subsequent line, with the same separator between each.

Option 2 allows the user to analyze an entire image stack. This could be the 
entire meteorite particle’s track, or a portion of it. The volume of the track 
will be displayed on the terminal, as well as in the bottom of the file 
“Areas.txt”. The individual area, radius, and approximate center for each 
image in this sequence are also stored in the file “Areas.txt”. If the user 
selects this option, then the data points for each best fit circle are not 
outputted. The range should be specified with the first track to analyze first 
and the last track to analyze second with only a space in between (ex. 0 3736). 
If one of the files does not exist, the program should report which file it 
couldn’t locate, and then skip it. In this case or in the case that there are not 
enough data points in the file, the resulting output in “Areas.txt” will be ‘nan’, 
meaning ‘not a number’. This does not impact the overall result so long as it 
occurs in only a small fraction of the total analyzed files.


/****************** Possible Errors and Troubleshooting Tips ******************/


The accuracy of the best fit program is analyzed in more detail in the paper, 
“Determining the Volume of a Comet Particle Track via an Automatic Best Fit 
Circle Method”, which details the whole process used to determine the best 
method of automatically analyzing the comet particle tracks. It is important to 
note that the amount of noise on the image slices is directly proportional to the 
accuracy of the results, and so reducing the error caused by the instruments plays 
a key role in the calculation of a track’s volume.

The said, one semantic error that may impact the results heavily is the number 
of viable images used. A viable image is one which produces more than one data 
point when run through the ImageJ/Fiji plugin. If the image is so noisy that all 
of the points are eliminated, perhaps it would be prudent to lower the noise 
tolerance used when calculating the maxima. This will not make a difference if the 
image is made binary, as the noise tolerance is based on how close a pixel comes 
to the maximum amount, and in a binary image, there is only two pixel colors used 
(white and black). Playing around with the noise tolerance (removing the “make 
binary” step), and whether or not the image is inverted, is a great way to 
customize this plugin to the individual needs of different tracks. See the in-file 
comments for details about where these edits can be made. 
