tabHome_welcome <- HTML("<h2>Welcome to Eva!</h2>
              <p><b>Eva</b> is an <a href = 'https://github.com/abtassociates/eva'
              target= '_blank' rel='noopener noreferrer'>open-source</a>
              project intended for local use by HMIS Administrators in Continuums
              of Care (CoCs) around the U.S. and its territories. Eva is designed 
              to help you (1) assess the accuracy and completeness of the data within 
              your HMIS, and (2) understand your homeless response system’s flow 
              and performance. Using Eva does not result in reporting or sharing 
              data with HUD and use of Eva is not required by HUD.</p>
              
              <p>Eva is a web-based tool built with R Shiny. This means:</p>
              <ul>
                <li>Eva will only access your CoC&rsquo;s data during your session, 
                <strong>no CoC data is being retained or viewed by anyone besides 
                you.</strong> Eva does retain metadata about the upload file itself, 
                such as the name of your software vendor, your export dates, hash 
                status, and data source information. This is collected for 
                troubleshooting and tool planning purposes.
                </li>
                <li>You can upload a zipped CSV Export of up to 200 MB. The 
                file must be hashed.</li>
                <li>You can stay up to date with the new features by visiting 
                the Changelog tab.</li>
              </ul> 
            ")

tabHome_home_live_instructions <-  HTML(
  "<p>Eva works by uploading a hashed 
              <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Export</a>.
              </p>
              <p>Generate a hashed HMIS CSV Export from your local HMIS and store
              it in a secure location that you can easily find again. It must be
              a .zip file with 23 csv files in it.
              <ul>
              <li>A hashed export means that the personal identifiers are obscured
              when the export is generated.</li>
              <li>The HMIS CSV Export has client-level data in it, so it must be
              stored in a secure location per HUD, state, and local rules and
              regulations.</li>
              <li>If you are unsure how to generate your hashed HMIS CSV Export,
              please contact your vendor.</li>
              </ul>
              
              <p>Once you have exported the correct file from your HMIS, you are
              ready to engage with Eva. Navigate to the \'Upload HMIS CSV Export\' tab
              and follow the instructions there.</p>

              <p>Want to explore Eva without uploading? Use Eva's Demo Mode by clicking the 
              toggle at the top.</p>
              ")

tabHome_home_demo_instructions <- HTML(
  "<div class = 'in_demo_mode' style='display:none'>
              <p>Welcome to Eva’s Demo Mode. In Demo Mode, you can explore the
              functionality of Eva with a pre-uploaded HMIS CSV Export file that
              uses sample HMIS data. When Demo Mode is on, Eva has the same
              functionality but uses the sample HMIS data to provide examples of
              possible File Structure Errors, Data Quality Errors, and Warnings.
              Select any of Eva's pages from the navigation menu to the left to
              explore the application. </p>
              <h4>Turning Demo Mode On & Off</h4>
              <p>To turn Demo Mode on and off, use the yellow Demo Mode toggle
              on the top right of the screen. This toggle will be available from
              every page in Eva. Please note that you can turn Demo Mode off or
              on at any time, the application will just ask you to confirm your
              choice.</p>
              <p>If you uploaded your own dataset to Eva and then decide to turn
              on Demo Mode, Eva will (1) clear the application of your HMIS data,
              ending the session, and (2) replace it with that of the sample
              dataset. <strong>If you wish to see your results again you will need to
              re-upload your hashed HMIS CSV Export file.</strong> To do so, you
              need to turn off Demo Mode. This will clear the sample HMIS data
              from the application so you can operate Eva as normal and upload
              your own HMIS data again.</p></div>
              ")

tabHome_need_help <-   HTML(
  "<p>Trouble-shooting tips:
              <ul>
              <li>Be sure you have generated the correct export. You cannot use
              an APR or LSA export file for use with this tool.</li>
              <li>Eva is looking for a zip file and will extract the files for you,
              so unzipping your export is not necessary.</li>
              <li>If your export is a .7z file, you must convert it to a .zip file.
              If you are not sure about how to do this, please contact your vendor.
              <li>If something is not working, please go to <a
              href='https://github.com/abtassociates/eva/issues' target= '_blank'
              rel='noopener noreferrer'>GitHub</a> to check for known issues
              and/or enter any new issues or feature enhancement requests. To
              enter an Issue on GitHub, you must have an account. If you do not
              already have a GitHub account, you can sign up for one <a
              href='https://github.com/join' target= '_blank'
              rel='noopener noreferrer'>here</a>.
              </ul>"
)

tabHome_citations <-  HTML("
                 <p>This project would not exist were it not for the existence of
                 other quality, free and open source products. The following are
                 citations for the products this app relies on.
                              
                 <p> The foundational code for the app was shared via AGPL license by 
                 the Coalition on Homelessness and Housing in Ohio (<a href =
                 'https://www.cohhio.org' 
                 target= '_blank' rel='noopener noreferrer'>COHHIO</a>). 
                 
                 <p>R Core Team (2022). R: A language and environment for statistical
                 computing. R Foundation for Statistical Computing. Vienna, Austria. 
                 <a href = 'https://www.r-project.org' 
                 target= '_blank' rel='noopener noreferrer'>R programming language</a>.
                 
                 <p>Wickham et al., (2019). Welcome to the tidyverse. <a href = 
                 'https://doi.org/10.21105/joss.01686'
                 target = '_blank' rel='noopener noreferrer'>Journal of Open Source
                 Software</a>, 4(43), 1686, 
                 <a href = 'https://cran.r-project.org/web/packages/tidyverse/index.html'
                 target= '_blank' rel='noopener noreferrer'>Tidyverse package</a>.
                 
                 <p>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen
                 J, McPherson J, Dipert A, Borges B (2021). _shiny: Web Application
                 Framework for R_. R package version 1.7.1, 
                 <a href = 'https://cran.r-project.org/web/packages/shiny/index.html'
                 target= '_blank' rel='noopener noreferrer'>R Shiny package</a>.
                 
                 <p>Chang W, Borges Ribeiro B (2021). _shinydashboard: Create
                 Dashboards with 'Shiny'_. R package version 0.7.2, 
                 <a href = 'https://CRAN.R-project.org/package=shinydashboard'
                 target= '_blank' rel='noopener noreferrer'>shinydashboard package</a>.
                 
                 <p> Special thanks to 
                 <a href=\"http://www.squarepegdata.com/\" 
                 target= '_blank' rel='noopener noreferrer'>
                 Square Peg Data</a>, the CoCs who provided us with
                 sample datasets to support programming.")

tabUpload_instructions <-  HTML("
                 <h4>Upload hashed HMIS CSV Export</h4>
                 
              <p>To upload your hashed HMIS CSV Export, click the \'Browse\'
              button. Once you find the zip file on your computer, click on it to select it, 
              and click \'Open\' to begin the upload. Eva might take a few moments 
              to process your selected file. Eva will first check to determine if 
              the export is hashed. If it is not, Eva will reject the file with 
              an error message, and clear Eva's memory. Eva will continue to do 
              this until you upload a hashed HMIS CSV Export.</p>
              
              <h4>HMIS CSV Export File Structure Analysis</h4>
              <p>After confirming your export is hashed, Eva will review and process 
              the file structure of your upload. The File Structure Analysis assesses 
              the structural components of the uploaded .zip file and determines 
              if it meets Eva’s file structure requirements, such as if the files 
              have all the right names, columns, data types, and allowable values etc.</p>
              
              <p>Once your upload is processed and Eva has finished assessing the 
              file structure integrity of your upload, Eva will provide a pop-up 
              message alerting you of your upload status. You can have either a 
              successful upload or an unsuccessful upload based on the structural 
              integrity of your HMIS CSV export. The key difference between a successful 
              upload and an unsuccessful upload is if the upload has any High Priority 
              File Structure Errors.</p>
              
              <p>While any error identified during the File Structure Analysis represent 
              components in the uploaded HMIS CSV export file that do not meet the 
              most recent <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Format Specifications</a>, 
              there are some file structural errors that are more relevant to the 
              functionality of Eva.</p>
              
              <ul>
                <li><b>High Priority File Structure Errors</b> are file structure issues 
                that will cause Eva to not work.</li>
                <li><b>General File Structure Errors</b> are file structure issues that 
                will not impact Eva’s ability to work, but do not meet HMIS CSV 
                format specifications.</li>
              </ul>
              
              <p>If Eva identifies any High Priority File Structure Errors during 
              the File Structure Analysis that prevent Eva from functioning, Eva 
              will reject your upload and stop processing the export. You will thus 
              not be able to assess the data quality of your upload or analyze the 
              system performance of your homeless response system. For both successful 
              and unsuccessful uploads, all identified file structure errors will 
              display in the HMIS CSV File Structure Analysis panel, where you can 
              download the details.</p>
              
              <p>It is essential that you contact your HMIS vendor to resolve all 
              High Priority File Structure Errors identified in the HMIS CSV Export 
              File Structure Analysis, as well as any other structural issues which 
              you feel need to be corrected. Not all structural issues found in 
              this analysis will prevent the data from being accepted for analysis, 
              so they may not require immediate attention. Once your vendor has 
              addressed any High Priority File Structure Errors, you can attempt 
              another upload.</p>
              
              <p>Once you have uploaded a hashed and structurally sound .zip file, 
              you will see a confirmation that your upload was successful, the date 
              range of the files you uploaded, plus the date your Export was downloaded 
              from your HMIS. You will then be able to assess the data quality of 
              your upload and analyze the system performance of your homeless response system.</p>
              
              
              ")

tabUpload_in_demo_mode <- HTML(paste0("<div class='in_demo_mode' style='display:none'>
                 <p>You’re currently in Demo Mode and viewing sample HMIS data
                 from a curated HMIS CSV Export file. View the File Structure
                 Analysis below to see examples of the File Structure Errors you
                 could get in your own uploads. For a full list of possible
                 errors, see <a href =
                 'https://github.com/abtassociates/eva/blob/main/public-resources/EvaChecks.csv' 
                 target= '_blank' rel='noopener noreferrer'>Eva Checks</a>.</p>
                 <p>To explore your own File Structure Errors, turn off Demo
                 Mode and upload your own hashed HMIS CSV Export file.</p></div>
            "))