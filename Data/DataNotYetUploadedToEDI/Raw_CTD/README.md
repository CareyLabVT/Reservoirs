# CTD overview

Welcome to the CTD megalopolis! This sub-directory includes ALL of the CTD data (raw and processed) and code for the Carey Lab reservoir monitoring program. This ReadMe will provide pointers for common tasks and describe data processing workflows.

## Common tasks

To start off, here are a few common things you might want to do:

Data use:

-   If you want to immediately get started using this year's data, `ctd_L1.csv` is the file for you. Historical data is published on EDI.

Data visualization:

-   To take a look at current/recent casts, you can look in the `./PDF_outputs/` folder. There are *lots* of files here, so you can't see the most recent files on the web (sad). More recent files are still stored, and can be visualized locally.

Data/metadata upload

-   If you are in charge of uploading data, new data files (.xml, .cnv, .hex) get uploaded into the `./RawDownloads/` folder. Maintenance records go into `CTD_Maintenance_Log.csv`. Thanks!

## Comprehensive file structure overview

Confused about something in this folder? Here's the comprehensive reference with everything I know:

-   `csv_outputs`: .csv files for all processed CTD casts (individually). Automatic data processing will check whether a given cast has been processed and exported here. If not, it will convert the .cnv file for a given cast to a .csv and store it here

-   `CTD_catwalk_figures`: comparisons between CTD data and *in-situ* streaming sensors. Legacy folder, not currently used

-   `CTD_code`: code used for automated data processing, visualization, and comparisons of the two CTDs. More description below

-   `CTD_EDI`: all files for publishing CTD data to EDI (e.g., metadata, archives of data and code files, .xml files, etc). If you are in charge of publishing data to EDI, this is where you will do that work! More information on data publication below.

-   `ctd_L1.csv`: provisional data for the most recent year (format matches EDI published data)

-   `CTD_Maintenance_Log.csv`: maintenance log with all maintenance/QAQC dates

-   `CTD_Notes.txt`: Additional notes about CTD maintenance/QAQC. **NOTE: To be removed?**

-   `CTD_season_csvs`: .csv files that combine all data for this year. **NOTE: To be removed?**

-   `ctd_seasonal_flags.csv`: not sure. **NOTE: To be removed?**

-   `metadata_files`: Automatically generated metadata for each cast. Note that I don't really think these metadata files are useful... sorry.

-   `Old_XMLCON_files`: .xml configuration affects the initial data processing when getting data off of the CTD. We archive these configuration files here for reproducibility

-   `PAR_files`: Visualizations (.pdf) and data (.csv) for PAR data from each cast. This was originally done for Mary many years ago, but we could probably stop generating these. **NOTE: To be removed?**

-   `PDF_outputs`: Visualizations of individual profiles as a quality control gut-check.

-   `RawDownloads`: ALL data files (.xml, .cnv, .hex) from all years are stored here, and re-processed for data publication

-   `README.md`: that's this, silly!

## Automatic data processing workflow

Automatic data processing is done by `ctd_QAQC.R`, using helper functions in `./CTD_code/R/`. As an outline, here's the broad workflow:

1.  Compare the file names in the `RawDownloads` folder (unprocessed files) with the file names in `csv_outputs` (processed files). If there are files in `RawDownloads` that are not also in `csv_outputs`, these need to be processed! Otherwise, there's nothing we need to do here, and the function ends.
2.  Process new files (converting from .cnv to usable .csv). In addition to producing a .csv file, this makes a pdf visualization in `PDF_outputs`, saves the scan number of the start of the profile in `saved_scan_numbers.csv`, and generates PAR-specific outputs in `PAR_files`.
3.  Generate updated seasonal .csv file (`CTD_Meta_2023.csv`)
4.  Add data flags to seasonal csv, producing this year's L1 file

All that said, if there is something you need to *change* about the data processing, you probably want to start with `ctd_QAQC.R`, look at what step needs to be changed, then refer to the relevant file in `./CTD_code/R/`.

## Data publication

The good news for data publication is that we have done LOTS of work on CTD data processing automation in 2023. At this point, you should have an up-to-date L1 file for the current year's data, which you can combine with historical data to create the EDI data publication. Here's the approximate workflow:

1.  Re-process ALL raw files (2019-present) with the current QAQC script. This will take a long time. We made the decision to do this in January 2024 to make sure we are using standardized QAQC for all profiles over time. We don't necessarily have raw files from before 2018, so we are using published data from 2013-2018 and combining those with re-processed data from 2019-2023.

2.  Visualize the combined dataset (updating maintenance logs if necessary). This is done using `CTD_visualization.R`.

3.  Update metadata in the EDI folder (`CTD_EDI`)

4.  Stage package (using `MakeEMLCTD.Rmd`), peer review, publication following other instructions
