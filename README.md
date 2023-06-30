# PhD-thesis-algorithms
Learning TAN, CL, STAN and BN from incomplete and imbalanced data

1-  Ensure that you install the following libraries:
	- graph
	- e1071
	- mix
	- bnlearn
	- pcalg
	- Rgraphviz
	- filesstrings
	- dplyr
 	- infotheo
  	- ggm
   	- mclust

2- EM folder: folder for learning BN structure from incomplete data:
		- install gobnilp from the following link:
			https://www.cs.york.ac.uk/aig/sw/gobnilp/

		- put "scoring2a" file in the folder with the data

		- Create a Gobnilp set file named "gobnilp.set" and write to it:
			gobnilp/outputfile/adjacencymatrix = "bi.mat"
			
		- run the file main_1.R first to get all ".mat" files.

		- To obtain the HSD and comparison graphs in PDF files, execute the "main_2.R" file..

3- TAN_CL folder: folder for other algorithms -- learing TAN, Chaw-Liu and Selactv TAN from incomplete and imbalanced data.
