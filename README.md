# IncomeDataVisualization
## CENG 574 Data Visualization Project

This project aims to analyze the U.S. Census Bureau Income Dataset. The main goal was to project the data onto 2 dimensions to better visualize the multivariable dataset and to identify clusters using unsupervised machine learning algorithms. It was submitted as part of the Final Project for Fall 2020 METU CENG 574: Statistical Data Analysis course.

The codes used to generate the results for the [Final Paper](CENG_574_Final_Paper.pdf) can be found in the [Final_Paper_Script](Final_Paper_Script.R). The original [R Markdown file's](RMarkdown/FinalReport.Rmd) generated pdf, [Final_Paper_Extra_Plots](Final_Paper_Extra_Plots.pdf), contains all of the extra plots that were mentioned, but not shown, in the Final Paper. Additionally, these plots can be seen individually directly inside the [Plots directory](Plots/).

### Projection Methods Used:
* Principal Component Analysis (PCA)
* Multiple  Multidimensional Scaling (MDS) (Classic Torgerson’s, Sammon's, Kruskal's nonlinear mapping, Symmetric Smacof)
* Uniform Manifold Approximation and Projection (UMAP)
* t-distributed Stochastic Neighbor Embedding (t-SNE)

### Clustering Algorithms Used:
* Agglomerative Nesting (AGNES) hierarchical clustering (with 6 different linkages)
* Divisive Analysis (DIANA) clustering
* k-means Clustering
* k-medoids Clustering
* k-means clustering applied on a Self-organizing Map (SOM)

### Cluster Validation Tests Used:
* Stability: Nonparametric Bootstrap, Avg. Proportion of Non-overlap, Avg. Distance (AD), AD between Means, and Figure of Merit
* Internal Validation: Connectivity, Silhouette Width, and Dunn Index
* External Validation: Rand Index
