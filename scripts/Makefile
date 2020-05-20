data := ../data
out := ../output
R := ../R

all: cluster parse net regression clanalysis

## Clean the cached regression model
clean:
	rm $(data)/04_model.Rds

## Additional analysis of clusters
clanalysis: $(wildcard $(out)/05_*.*)
$(out)/05_*.*: 05_cluster_analysis.R \
			   $(data)/01_dist_matrix.Rds \
			   $(data)/02_parsed.Rdata \
			   $(data)/03_univ_net_stats.rds
	Rscript 05_cluster_analysis.R
	rm Rplots.pdf


## Regression model
regression: $(wildcard $(out)/04_*.*)
$(out)/04_*.*: 04_regressions.R \
			   $(data)/04_model.Rds \
			   $(R)/predictions.R \
			   $(R)/posterior_estimates.R \
			   $(data)/02_parsed.Rdata \
			   $(data)/03_univ_net_stats.Rds
	Rscript -e "rmarkdown::render('04_regressions.R')"


## Construct and analyze the hiring network
net: $(data)/03_univ_net_stats.rds
$(data)/03_univ_net_stats.rds: 03_network.R \
							   03_network_cache \
							   $(data)/02_parsed.Rdata
	# Clear the knitr cache
	-rm -r 03_network_cache
	Rscript -e "rmarkdown::render('03_network.R')"


## Parse the data used in the network and regression analyses
parse: $(data)/02_parsed.Rdata
$(data)/02_parsed.Rdata: 02_parse.R \

						 $(data)/00_query_result_2018-11-09.csv \
						 $(data)/00_university_table_2018-11-09.csv \
						 $(data)/01_university_and_cluster.Rds
	Rscript -e "rmarkdown::render('02_parse.R')"


## Construct clusters
cluster: $(wildcard $(data)/01_*.*)
$(data)/01_*.* $(out)/01_*.png: 01_clustering.R \
				 $(data)/00_AosCorrespondenceTable.csv \
				 $(data)/00_newGradData_2018-12-11.csv \
				 $(data)/00_newSurveyData_2018-12-11.csv
	Rscript -e "rmarkdown::render('01_clustering.R')"