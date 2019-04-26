figure-evaluate.png: figure-evaluate.R
	R --vanilla < $<
figure-spectra.png: figure-spectra.R Mayan_folds.csv
	R --vanilla < $<
Mayan_folds.csv: Mayan_folds.R Mayan_data.csv
	R --vanilla < $<
