# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

guidance_conflicting_hi <- 
  "If the user answered \"Yes\" to \"Covered by Health Insurance?\", then
  there should be a Health Insurance subassessment where it indicates which
  type of health insurance the client is receiving. Similarly if the user
  answered \"No\", there should not be any Health Insurance records that say
  the client is receiving that type of Health Insurance."

guidance_conflicting_income <- 
  "If the user answered \"Yes\" to \"Income from any source\", then
  there should be an income subassessment where it indicates which
  type of income the client is receiving. Similarly if the user answered
  \"No\", there should not be any income records that say the client is
  receiving that type of income."

guidance_conflicting_ncbs <-
  "If the user answered \"Yes\" to \"Non-cash benefits from any source\",
  then there should be a Non-cash benefits subassessment where it indicates
  which type of income the client is receiving. Similarly if the user answered
  \"No\", then there should not be any non-cash records that say the client is
  receiving that type of benefit"

guidance_missing_at_exit <- 
  "Please enter the data for this item by clicking into the Exit pencil on the 
  given Client ID on the appropriate program stay."

guidance_missing_at_entry <- 
  "This data element is required to be collected at project Entry. Please
  click into the client's Entry pencil to save this data to HMIS."

guidance_missing_pii <- 
  "Please correct by navigating to the client's record, then clicking the
  Client Profile tab, then click into the Client Record pencil to save the
  missing data."

guidance_referral_on_non_hoh <- 
  "Users should not checkbox all the household members when creating a
  Referral. Only the Head of Household needs the Referral. It is recommended
  that users delete any referrals on Non Heads of Household related to this
  project stay so that the receiving agency does not have to deal with them
  and they stop showing in reporting."

guidance_service_on_non_hoh <- 
  "Users should not checkbox all the household members when creating a
  Service Transaction. Only the Head of Household needs a Service
  Transaction. Delete any extraneous Service Transactions related to this
  project stay."

guidance_dkr_data <- 
  "It is widely understood that not every client will be able to or consent
  to answer every question in every assessment. If you do have any of this
  data, but it is just not entered into HMIS yet, please enter it. If you
  can reasonably attempt again to collect this data from the client (like
  if they are still in your project), then please do so. Otherwise, there is
  no action needed."

