data := ../data
out := ../output
R := ../R

all: cluster parse net regression clanalysis

## Clean the cached regression model
clean:
	rm $(data)/04_model.Rds

## Additional analysis of clusters
clanalysis: $(wildcard $(out)/05_*.*)
$(out)/05_*.* &: 05_cluster_analysis.R \
			   $(data)/01_dist_matrix.Rds \
			   $(data)/02_parsed.Rdata \
			   $(data)/03_univ_net_stats.rds
	Rscript -e "rmarkdown::render('05_cluster_analysis.R')"


## Regression model
regression: $(wildcard $(out)/04_*.*)
$(out)/04_*.* &: 04_regressions.R \
			   $(data)/04_model.Rds \
			   $(R)/predictions.R \
			   $(R)/posterior_estimates.R \
			   $(data)/02_parsed.Rdata \
			   $(data)/03_univ_net_stats.Rds
	Rscript -e "rmarkdown::render('04_regressions.R')"
	
## Force resampling the model (don't use cache)
force: 
	Rscript 04_regressions.R --force


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
						 $(data)/00_PaperDataAPDA_2021-08-18.csv \
						 $(data)/00_university_table_2018-11-09.csv \
						 $(data)/01_university_and_cluster.Rds
	Rscript -e "rmarkdown::render('02_parse.R')"


## Construct clusters
cluster: $(wildcard $(data)/01_*.*)
$(data)/01_*.* $(out)/01_*.png &: 01_clustering.R \
				 $(data)/00_PaperDataAPDA_2021-08-18.csv \
				 $(data)/00_AOSDatax_2021-08-19.xlsx
	Rscript -e "rmarkdown::render('01_clustering.R')"