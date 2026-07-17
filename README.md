# ice_detention_reports
Repo for finalized reporting of ICE detention parameterized reports

### Part 1 Create Interval ADP charts
1. Visually check new facility data.
	- Are population values rounded?
	- Do any facilities include blanks?
	- Check equality between the three categories (classification, criminality, threat level).
	- Note the date of data extraction for this release (in header).
	- Note the total number of facilities in ICE list for this release (max excel row number minus number of header rows).

2. Add the new xlsx file to your local directory.
   
3. Run `interval_adp.r`.
	- `detloc_processing.r` is sourced here and it provides a cleaning mechanism for the Detention Facility Codes (DETLOC).
	- Validate any irregularities by looking at the interval ADP numbers (`back_interval_adp`) calculated from the most recent release.

4. Run `interval_adp_chartmaker.r`
	- Change date touchpoints in chart (also check in one_facility_plot.r and one_facility_table.r).	
	- Certain facilities need manual entry due to specifics such as legend position. Touchpoints here.
	- Add/subtract manual entries where necessary.

5. Validate charts and ensure the number of charts equals the number of facilities in the ICE facility list.

### Part 2 Calculate Individualized Interval ADP subpopulations
1. Run `Individual_Internal_ADP.r`.
   
2. Check for equality between categories in most recent release.
   
### Part 3 Render Reports
1. Move png charts to interval file in directory.
   
2. Move individual spreadsheet file to directory if necessary.
   
3. If a facility is new, you may need to google their lat/longs and add them to the location file (currently `fy25_detention_ouptu_2025-05-11.csv`)
	- Test one file by "rendering" the qmd file using the parameters of the facility name (ALL CAPS) and latitude and longitude. 
	- Check to see if the html file renders correctly.

4. Check parameters, and run `facility_reporting.r` quarto reporting loop.
   
5. Manually render the facilities (Clinton County NY/IN for example) which have bugs by inputting their info into the sheet, applying the touchpoint filters and rendering.
   
6. Validate
	- Does the number of new html files equal the number of total facilities in the ICE list?
	- Does the file size of each html exceed 2kb? Anything less suggests that the script could not find the png interval file.

### Part 4 Update Metadata
1. At this point, I prefer to make the changes in an excel file, and then write to json.
   
2. Filename should be `index.json`.

### Part 5 Push to GitHub
1. Push the interval png files (Step 1) to the interval subdirectory.
   
2. Push the HTML Reports (Part 3) to the facility subdirectory.
	- If doing this using the GitHub in desktop mode, the html files can be pushed only in groups of 13.
	- Push the index.json file (Part 4) to the main directory.
