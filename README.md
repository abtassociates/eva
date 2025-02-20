# Eva

An open-source HMIS reporting system. This repository would be helpful to any HUD-designated Continuum of Care or HMIS Lead looking for a way to get more out of their HMIS data. 

### About

This is an open source project released under the GNU AGPLv3 license. See LICENSE for more details or visit the official GNU page at http://www.gnu.org/licenses/agpl-3.0.html.

All the code in this repository is written using R and R Studio. No knowledge of R or programming is necessary to use this tool.

### Data Source

This app takes an upload of your HMIS system's hashed HMIS CSV Export, runs logic on the uploaded data, and produces summary and detail data for the HMIS Lead's benefit.

### Demo Mode

In Demo Mode, you can explore the functionality of Eva with a pre-uploaded HMIS CSV Export file that uses sample HMIS data. When Demo Mode is on, Eva has the same functionality but uses the sample HMIS data to provide examples of possible File Structure Errors, Data Quality Errors, and Warnings. Select any of Eva's pages from the navigation menu to the left to explore the application.

### Security

No HMIS data is ever included in the code in this repository. To make this code work, you will need to supply your own HMIS data. You are responsible for securing your HUD CSV export on your computer and ensuring that it is not compromised using the security measures you use for that locally.

### Compliance

As per the [HMIS CSV Specs](https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf), right single quotation marks (’) and right double quotation marks (”) are not among the permitted characters in string fields. These invalid characters can make their way into CSV files by copy-pasting from Microsoft Word. Please be aware that Eva will not work if these characters are included in string fields.

