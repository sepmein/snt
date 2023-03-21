# snt
Global Malaria Programme Data Working Pipelines

Maintainer: Bea & Spencer

- 1. project development
	- Update packages
- 2. reproducible analysis setup
	- Now using repana as project setup
	- I need to take a further look into the official CRAN website, a lot of nice projects are there.
- 3. data management
	- Importation
		- read files
		- raster
		- shapefile
			- extract from WHO gishub
		- rename header
		- standard db structure
	- cleaning
		- check outliers
		- fuzzy matching
	- Integration
		- Schema integration
		- Entity identification
		- resolving value concepts
	- Transformation
	- Normalization
		- **Smoothing:** With the help of algorithms, we can remove noise from the dataset, which helps in knowing the important features of the dataset. By smoothing, we can find even a simple change that helps in prediction.
		- **Aggregation:** In this method, the data is stored and presented in the form of a summary. The data set, which is from multiple sources, is integrated into with data analysis description. This is an important step since the accuracy of the data depends on the quantity and quality of the data. When the quality and the quantity of the data are good, the results are more relevant.
		- **Discretization:** The continuous data here is split into intervals. Discretization reduces the data size. For example, rather than specifying the class time, we can set an interval like (3 pm-5 pm, or 6 pm-8 pm).
		- **Normalization:** It is the method of scaling the data so that it can be represented in a smaller range. Example ranging from -1.0 to 1.0.
- 4. db
- 5. snt analysis
- 6. reporting system
	- quatro template
