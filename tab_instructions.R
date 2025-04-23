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
