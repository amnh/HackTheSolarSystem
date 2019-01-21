#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdlib>
#include <string>
#include <sstream>
#include <fstream>
#include <cmath>

using namespace std;

/********** Functions used in this program include: *************/

/* Print functions for a 1D or a 2D vector */
void print_1d(vector<double> V);
void print_2d(vector<vector<double> > V);

/* A function to find the radius of the best fit circle for a given set of data points */
vector<double> findRadius(vector<vector<double> > & V, double & radius);

/* And a function to calculate and output to file the points lying along the edge of a circle with radius r. */
string outputCirclePoints(vector<double> c, double r);

/* There are two built-in ways to run this code: for a solo image or an image stack */
int ProcessSoloImage(int slice);
int ProcessImageStack(int firstSlice, int lastSlice);

/****************************************************************/

/* Global Variables */
double pxtomic = 0.458; //conversion factor from pixels to microns
string multiAreaOut = "Areas.txt"; //output file for the individual areas per slice (if ProcessImageStack(int, int) is called)

/***************************** Main *****************************/
int main(int argc, char *argv[])
{	
	cout << "Best Fit Circle Program" << endl;
	cout << "Make sure that you have run the ImageJ/Fiji plugin \"FindMaximaPerSlice.txt\" prior to running this software." << endl;
	cout << "1. Process Solo Image" << endl;
	cout << "2. Process Image Stack" << endl;
	cout << "How would you like to use this software? ";
	int selection, ft, lt;
	cin >> selection;
	
	if(selection == 1)
	{
		char yorn;
		cout << "Analyze a data file outputted from \"FindMaximaPerSlice.txt\" plugin? (y/n) " ;
		cin >> yorn;
		
		if(yorn == 'y')
		{
			cout << "Please input the index of slice/data file. " ;
			cin >> ft;
			ProcessSoloImage(ft);
		}
		else
			ProcessSoloImage(-1);
	}
	else if(selection == 2)
	{
		cout << "Please input the range of the track you wish to analyze separated by a space. Ex: 0 3736" << endl;
		cin >> ft >> lt;
		if(lt > ft)
		{
			cout << "Calculating. This may take a few moments depending on track length." << endl;
			ProcessImageStack(ft, lt);
		}
		else
			cerr << "Inputted range [" << ft << ", " << lt << "] is invalid." << endl;
	}
	else
		cerr << "Invalid Selection." << endl;
	
	return 0;
}


/* Subroutine to determine the volume of all slices in a given length of track */
int ProcessImageStack(int firstSlice, int lastSlice)
{
	string inputfilename;
	double volume = 0.0;
	
	ofstream fout( multiAreaOut.c_str() );
	fout << "Slice #" << setw(9) << "Area" << setw(13) << "Radius" << setw(16) << "Center" << endl;
	
	for(int z = firstSlice; z <= lastSlice; z++)
	{
		// First recalculate the name of the input file //
		stringstream ss;
		ss << z;
		inputfilename = "data_files/data" + ss.str() + ".txt"; 
		/* Code assumes that the location of the data files is ./data_files and the name of the */
		/* files is data{z}.txt as that is what the output of the ImageJ/FIJI plugin should be. */
		ifstream fin( inputfilename.c_str() );
	
		// Define initial variables //
		vector<vector<double> > V;
		vector<double> S;
		double temp, col1, col2;
		char fl1, fl2;
	
		if( fin.is_open() )
		{	
			int i = 0;
		
			fin >> fl1 >> fl2; //first line is outputted from ImageJ/FIJI plugin with useless data, for which this line placeholds
			
			while( fin >> temp >> col1 >> col2 )
			{   //the three columns from ImageJ/FIJI plugin output: an arbitrary number, the x coordinate in pixels, the y coordinate in pixels.
				V.push_back(S);
				V[i].push_back(col1*pxtomic); //multiply each of these by the conversion factor to get microns
				V[i].push_back(col2*pxtomic);
			
				i++;
			}
		}
		else
		{
			cerr << "Cannot open file \"" << inputfilename << "\"." << endl;
			return 0;
		}
		
		if(V.size() > 1)
		{
			double radius = 0;
			vector<double> c = findRadius(V, radius);
			double area = M_PI * radius * radius;
			volume += area;
	
			/* Uncomment for details about specific slices. */
			/* ADVISED: ONLY UNCOMMENT IF RUNNING PROGRAM FOR SMALL NUMBER OF SLICES */
			/*cout << "Circle " << z << ":" << endl;
			cout << "The average radius length is " << radius << " and the approxiamte center is (" << c[0] << ", " << c[1] << ")." << endl;
			cout << "The approxiamate area of the best fit circle is " << area << "." << endl;
			cout << "The data points lying along this circle can be found in " <<  outputCirclePoints(c, radius) << "." << endl;
			cout << endl;*/
		
			fout << setw(4) << z << setw(14) << area << setw(12) << radius << setw(7) << "(" << c[0] << ", " << c[1] << ")" << endl;
		}
		else
			fout << setw(4) << z << setw(12) << "nan" << setw(12) << "nan" << setw(18) << "nan" << endl;
	}
	
	//outputs results to console
	cout << "The volume for these " << (lastSlice - firstSlice) + 1 << " slices is approxiamately " << volume << "." << endl;
	cout << "The areas for each individual slice can be found in the file \"" << multiAreaOut << "\"." << endl;
	
	//outputs results to file
	fout << "The volume for these " << (lastSlice - firstSlice) + 1 << " slices is approxiamately " << volume << "." << endl;
	fout << "The areas for each individual slice can be found in the file \"" << multiAreaOut << "\"." << endl;

	return 0;
}


/* Print Functions for 1D and 2D arrays */
void print_1d(vector<double> V)
{
	for(int i = 0; i < V.size(); i++)
	{
		cout << V[i] << " " ;
	}
	cout << endl;
}

void print_2d(vector<vector<double> > V)
{
	for(int i = 0; i < V.size(); i++)
	{
		for(int j = 0; j < V[i].size(); j++)
		{
			cout << V[i][j] << " ";
		}
		cout << endl;
	}
}


/* This function determines the average center of the circle, and then calculates the average radius from this center to each point */
vector<double> findRadius(vector<vector<double> > & V, double & radius)
{
	vector<double> center; center.push_back(0.0); center.push_back(0.0); //create void center counter (calculated values will update this below)
	int totalLinesDrawn = 0;
	
	for(int i = 0; i < V.size(); i++)
	{		
		for(int j = i+1; j < V.size(); j++)
		{
			double x1 = V[i][0], x2 = V[j][0];
			double y1 = V[i][1], y2 = V[j][1];
			
			double cx, cy;
			
			cx = (x1 + x2) / 2; cy = (y1 + y2) / 2; //center of the line between these two points

			center[0] += cx; center[1] += cy; //update the counter for the average center calculation
			totalLinesDrawn++;
		}
	}
	
	// finds the average center
	center[0] /= totalLinesDrawn; center[1] /= totalLinesDrawn;

	int counter = 0;
	
	for(int i = 0; i < V.size(); i++)
	{   // takes the center from above and determines the average radius to each point
		double x1 = V[i][0], y1 = V[i][1];
		radius += sqrt( pow( (x1 - center[0]), 2 ) + pow( (y1 - center[1]), 2) );
		counter++;
	}
	
	radius /= counter;
	
	return center;
	
}


/* This function can output the data points for the best fit circle of a particular image to a file for plotting purposes, etc. */
string outputCirclePoints(vector<double> c, double r)
{
	string outputfile = "BestFitCircle.txt";
	ofstream fout( outputfile.c_str() );
	
	double ypos, yneg;
	
	c[0] /= pxtomic; c[1] /= pxtomic; r /= pxtomic; // converts back to pixels for plotting
	
	for(double x = (c[0] - r); x <= (c[0] + r); x += 0.25)
	{	// (x-cx)**2 + (y-cy)**2 = r**2
		ypos = c[1] + sqrt( pow( r, 2 ) - pow( (x - c[0]), 2 ) );
		yneg = c[1] - sqrt( pow( r, 2 ) - pow( (x - c[0]), 2 ) );
		
		fout << x << " " << ypos << endl;
		fout << x << " " << yneg << endl;	
	}
	
	return outputfile;
}


/* In this routine, only one data file (user specified via file or command line) is analyzed, and the results are outputted to the console immediately. */
int ProcessSoloImage(int slice)
{
	// First recalculate the name of the input file //
	string inputfilename;
	if(slice < 0)
	{
		cout << "Please specify file where data is stored (\"path\\filename.txt\"): ";
		cin >> inputfilename;
	}
	else
	{  
		stringstream ss;
		ss << slice;
		inputfilename = "data_files/data" + ss.str() + ".txt"; 
		/* Code assumes that the location of the data files is ./data_files and the name of the */
		/* files is data{slice}.txt as that is what the output of the ImageJ/FIJI plugin should be. */
	}	
	
	ifstream fin( inputfilename.c_str() );
	
	vector<vector<double> > V;
	vector<double> S;
	double temp, col1, col2;
	char fl1, fl2, fl3;
	
	if( fin.is_open() )
	{	
		int i = 0;
		
		fin >> fl1 >> fl2; //first line is outputted from ImageJ/FIJI plugin with useless data, for which this line placeholds
		
		while( fin >> temp >> col1 >> col2 )
		{   //the three columns from ImageJ/FIJI plugin output: an arbitrary number, the x coordinate in pixels, the y coordinate in pixels.
			V.push_back(S);
			V[i].push_back(col1*pxtomic); //multiply each of these by the conversion factor to get microns
			V[i].push_back(col2*pxtomic);
			
			i++;
		}
	}
	else
	{
		cerr << "Cannot open file \"" << inputfilename << "\"." << endl;
		return 0;
	}

	if(V.size() > 1)
	{
		double radius = 0;
		vector<double> c = findRadius(V, radius);
	
		cout << "The average radius length is " << radius << " and the approxiamte center is (" << c[0] << ", " << c[1] << ")." << endl;
		cout << "The approxiamate area of the best fit circle is " << M_PI * radius * radius << "." << endl;
		cout << "The data points lying along this circle can be found in " <<  outputCirclePoints(c, radius) << "." << endl;
	}
	else
		cout << "There is only one point in this file, therefore the radius is nan." << endl;
		
	return 0;
}