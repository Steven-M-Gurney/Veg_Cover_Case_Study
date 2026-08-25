###############################################################################
###############################################################################

# 🔗 Openland Assessment Project
# 📋 Phase 1, Step 4 — Parent WLD Lands and MiFI Representation Crosswalk
#
# Supporting Analytical Workflow for:
# Michigan Department of Natural Resources
# Wildlife Division, Planning and Adaptation Section
# Technical Report
#
# Author: Steven M. Gurney
# Last updated: 25 AUGUST 2026

###############################################################################
###############################################################################

# PURPOSE
# -------
# This script compares the parent/reference list of DNR-WLD lands to the full
# Wildlife Division MiFI management-area list.
#
# This script creates three products:
#
#   1) A list of parent-list DNR-WLD management areas not represented in MiFI.
#
#   2) A manual review table showing parent-list records that did not exactly
#      match a MiFI management-area name after name cleaning.
#
#   3) A final appendix-ready table listing all parent-list DNR-WLD management
#      areas, with Region, Unit, MiFI name, and whether each area is represented
#      in MiFI.
#
#
# IMPORTANT INTERPRETATION NOTES
# ------------------------------
# This is a name-matching workflow, not a spatial validation workflow.
#
# Exact cleaned-name matches are accepted automatically.
#
# No fuzzy matching is used. Any parent-list area that does not exactly match a
# MiFI management-area name after cleaning is sent to manual review.
#
# Final "In MiFI" values are based on:
#
#   • accepted exact cleaned-name matches, and
#   • manual review decisions entered in the reviewed CSV file.
#
# Areas listed as "No" in the final appendix table should be interpreted as not
# confidently represented in MiFI by this workflow. They may be truly absent from
# MiFI, stored under another MiFI management-area name, nested within another
# area, or not represented as a separate MiFI unit_key.


###############################################################################
# 📦 1. Load Required Packages
###############################################################################
# ⭐ Why this matters:
# This loads the packages needed to read the MiFI data, read the parent lands
# list, clean names, summarize results, and export tables.

library(sf)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)
library(janitor)


###############################################################################
# 📁 2. Set Paths and Output Folder
###############################################################################

gdb <- "C:/Users/GurneyS5/OneDrive - State of Michigan DTMB/ArcGIS/Projects/Grasslands/Grasslands.gdb"

output_dir <- "phase1_step4_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


###############################################################################
# 📁 3. Read Full WLD MiFI Working Dataset
###############################################################################
# ⭐ Why this matters:
# This reads the full prepared WLD MiFI dataset from Step 1 before Agriculture,
# Herbaceous Openland, latest-snapshot, or terrestrial filters are applied.

stands_working <- readRDS(
  "phase1_step1_outputs/stands_working_prepped.rds"
)

# Inspect object structure.
glimpse(stands_working)
names(stands_working)


###############################################################################
# 📋 4. Create Full MiFI WLD Management-Area List
###############################################################################
# ⭐ Why this matters:
# This creates the broadest WLD MiFI management-area list available from the
# prepared Step 1 working dataset.

mifi_wld_areas <- stands_working %>%
  st_drop_geometry() %>%
  distinct(
    mifi_management_area = unit_key
  ) %>%
  filter(
    !is.na(mifi_management_area)
  ) %>%
  arrange(
    mifi_management_area
  )

# Print MiFI management-area list.
mifi_wld_areas

# Count MiFI management areas.
mifi_wld_area_summary <- mifi_wld_areas %>%
  summarise(
    mifi_management_areas = n()
  )

# Print summary.
mifi_wld_area_summary


###############################################################################
# 📁 5. Read Parent WLD Lands List
###############################################################################
# ⭐ Why this matters:
# This reads the parent/reference list used to define the full set of expected
# DNR-WLD management areas.

parent_wld_lands_raw <- read_excel(
  "WLD_lands_MDNR2020_modified.xlsx"
) %>%
  clean_names()

# Check column names before selecting or renaming fields.
names(parent_wld_lands_raw)

# Inspect raw parent list.
parent_wld_lands_raw


###############################################################################
# 🧹 6. Prepare Parent DNR-WLD Lands List
###############################################################################
# ⭐ Why this matters:
# This keeps only parent-list records where the listed authority is DNR-WLD.
# This prevents non-Wildlife Division lands from being treated as missing from
# MiFI.

parent_wld_lands <- parent_wld_lands_raw %>%
  mutate(
    
    # Standardize authority before filtering.
    authority = authority %>%
      str_to_upper() %>%
      str_squish(),
    
    # Preserve the original region value for appendix notes and QA.
    region_original = region,
    
    # Standardize region text.
    region = region %>%
      str_to_upper() %>%
      str_squish(),
    
    # Collapse older regional labels into current planning regions.
    region = case_when(
      region %in% c("SELP", "SWLP") ~ "SMR",
      region %in% c("NLP", "UP") ~ "NMR",
      TRUE ~ region
    )
  ) %>%
  
  # Keep only Wildlife Division-administered lands.
  filter(
    authority == "DNR-WLD"
  ) %>%
  
  # Use the parent/reference name as the final reporting name.
  rename(
    parent_management_area = name
  ) %>%
  
  # Remove blank names.
  filter(
    !is.na(parent_management_area)
  ) %>%
  
  # Remove exact duplicate records.
  distinct()

# Fix record error
parent_wld_lands <- parent_wld_lands %>%
  mutate(
    region = case_when(
      parent_management_area == "Potterville State Game Area" ~ "SMR",
      TRUE ~ region
    )
  )

# Print filtered parent list.
parent_wld_lands

# Count parent DNR-WLD records.
parent_wld_lands_summary <- parent_wld_lands %>%
  summarise(
    parent_dnr_wld_records = n(),
    parent_dnr_wld_management_areas = n_distinct(parent_management_area)
  )

# Print summary.
parent_wld_lands_summary


###############################################################################
# 🧼 7. Define Name-Cleaning Helper Function
###############################################################################
# ⭐ Why this matters:
# Parent-list names and MiFI names often use different naming conventions. This
# helper standardizes common differences before exact matching.
#
# The goal is not to rewrite names for reporting. The cleaned names are used
# only for matching.

clean_area_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("'", "") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("\\(", " ") %>%
    str_replace_all("\\)", " ") %>%
    str_replace_all("/", " ") %>%
    
    # Replace specific land-type names before broader names.
    str_replace_all("\\bstate wildlife research area\\b", "swra") %>%
    str_replace_all("\\bwildlife research area\\b", "wra") %>%
    str_replace_all("\\bstate game area\\b", "sga") %>%
    str_replace_all("\\bstate wildlife area\\b", "swa") %>%
    str_replace_all("\\bwildlife area\\b", "wa") %>%
    str_replace_all("\\bwaterfowl production area\\b", "wpa") %>%
    
    # Standardize common wording differences.
    str_replace_all("\\btownship\\b", "twp") %>%
    str_replace_all("\\btwp\\b", "twp") %>%
    str_replace_all("\\bnumber\\b", "no") %>%
    str_replace_all("\\bno\\b", "no") %>%
    
    str_squish()
}


###############################################################################
# 🧼 8. Clean MiFI and Parent Names for Matching
###############################################################################
# ⭐ Why this matters:
# This creates standardized versions of the parent and MiFI names for exact
# matching.

mifi_names <- mifi_wld_areas %>%
  mutate(
    mifi_management_area_clean =
      clean_area_name(mifi_management_area)
  )

parent_names <- parent_wld_lands %>%
  mutate(
    parent_management_area_clean =
      clean_area_name(parent_management_area)
  )

# Print cleaned names for QA.
mifi_names
parent_names


###############################################################################
# 🧪 8A. QA Cleaned Name Duplicates
###############################################################################
# ⭐ Why this matters:
# This checks whether multiple parent-list names or multiple MiFI names collapse
# to the same cleaned name. Duplicates are not always wrong, but they should be
# reviewed because they can create ambiguous matches.

parent_clean_name_duplicate_check <- parent_names %>%
  count(
    parent_management_area_clean,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

mifi_clean_name_duplicate_check <- mifi_names %>%
  count(
    mifi_management_area_clean,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

# Print duplicate checks.
parent_clean_name_duplicate_check
mifi_clean_name_duplicate_check


###############################################################################
# ✅ 9. Identify Exact Cleaned-Name Matches
###############################################################################
# ⭐ Why this matters:
# Exact cleaned-name matches are accepted automatically. No fuzzy matching is
# used in this workflow.

exact_parent_to_mifi_matches <- parent_names %>%
  left_join(
    mifi_names,
    by = c(
      "parent_management_area_clean" = "mifi_management_area_clean"
    )
  ) %>%
  filter(
    !is.na(mifi_management_area)
  ) %>%
  mutate(
    match_status = "Accepted exact match",
    match_method = "Exact",
    match_quality = "Exact cleaned-name match",
    
    # Recreate the MiFI clean-name field after the join for transparency.
    mifi_management_area_clean = parent_management_area_clean
  )

# Print exact matches.
exact_parent_to_mifi_matches


###############################################################################
# 🔎 10. Identify Parent Records Without an Exact Match
###############################################################################
# ⭐ Why this matters:
# This creates the set of parent-list records that did not exactly match a MiFI
# management-area name after standardizing names.
#
# These records are not automatically assigned to MiFI. They are sent to manual
# review so questionable matches do not get accepted by automated matching.

parent_without_exact_match <- parent_names %>%
  anti_join(
    exact_parent_to_mifi_matches %>%
      select(
        parent_management_area
      ),
    by = "parent_management_area"
  )

# Print parent records without exact match.
parent_without_exact_match


###############################################################################
# 🔗 11. Create Draft Parent-to-MiFI Crosswalk
###############################################################################
# ⭐ Why this matters:
# This creates the draft crosswalk using exact cleaned-name matches only.
#
# Exact cleaned-name matches are accepted automatically.
# All non-exact records are sent to manual review.

exact_match_crosswalk <- exact_parent_to_mifi_matches %>%
  mutate(
    mifi_management_area_final = mifi_management_area
  )

non_exact_review_crosswalk <- parent_without_exact_match %>%
  mutate(
    mifi_management_area = NA_character_,
    match_status = "Needs review",
    match_method = "Manual review required",
    match_quality = "No exact cleaned-name match",
    suggested_mifi_management_area = NA_character_,
    mifi_management_area_final = NA_character_
  )

parent_to_mifi_crosswalk_draft <- bind_rows(
  exact_match_crosswalk,
  non_exact_review_crosswalk
) %>%
  select(
    -any_of(c(
      "mifi_management_area"
    ))
  ) %>%
  select(
    parent_management_area,
    mifi_management_area = mifi_management_area_final,
    match_status,
    match_method,
    match_quality,
    suggested_mifi_management_area,
    region,
    region_original,
    wld_unit,
    authority,
    type,
    county,
    everything(),
    -any_of(c(
      "parent_management_area_clean",
      "mifi_management_area_clean"
    ))
  ) %>%
  arrange(
    match_status,
    parent_management_area
  )

# Print draft crosswalk.
parent_to_mifi_crosswalk_draft


###############################################################################
# 📝 12. Create Manual Review Table
###############################################################################
# ⭐ Why this matters:
# This is the only table that needs manual review. Each record listed here did
# not exactly match a MiFI management-area name after name cleaning.
#
# Fill in the manual decision columns in Excel, then save the reviewed file as:
#
#   product2_parent_to_mifi_review_table_reviewed.csv
#
# Use only these manual_decision values:
#
#   • Accepted manual match
#   • Not represented in MiFI
#   • Needs further review
#
# For accepted manual matches, enter the original MiFI unit_key name in:
#
#   manual_mifi_management_area
#
# Example:
#
#   parent_management_area: Grand River Gale Road State Game Area
#   manual_decision: Accepted manual match
#   manual_mifi_management_area: Gale Road Grand River SGA
#   manual_review_note: Known naming-order difference

parent_to_mifi_review_table <- parent_to_mifi_crosswalk_draft %>%
  filter(
    match_status == "Needs review"
  ) %>%
  select(
    parent_management_area,
    region,
    unit = wld_unit,
    type,
    county
  ) %>%
  mutate(
    manual_decision = NA_character_,
    manual_mifi_management_area = NA_character_,
    manual_review_note = NA_character_
  ) %>%
  arrange(
    region,
    unit,
    parent_management_area
  )

# Print manual review table.
parent_to_mifi_review_table


###############################################################################
# 📊 13. Export Draft Products for Manual Review
###############################################################################
# ⭐ Why this matters:
# This exports the draft crosswalk and manual review table. After reviewing the
# manual review table in Excel, rerun this script to create the final products.

write_csv(
  parent_to_mifi_crosswalk_draft,
  file.path(output_dir, "parent_to_mifi_crosswalk_draft.csv")
)

write_csv(
  parent_to_mifi_review_table,
  file.path(output_dir, "product2_parent_to_mifi_review_table.csv")
)

write_csv(
  mifi_wld_areas,
  file.path(output_dir, "mifi_wld_management_area_full_list.csv")
)

# Check draft exports.
list.files(output_dir)


###############################################################################
# ✍️ 14. Apply Completed Manual Review
###############################################################################
# ⭐ Why this matters:
# This step reads the completed manual review table back into R and uses those
# decisions to create the final MiFI representation products.
#
# Workflow:
#
#   1) Run this script through Section 13.
#   2) Open product2_parent_to_mifi_review_table.csv in Excel.
#   3) Fill in the manual decision columns.
#   4) Save the reviewed file as:
#
#        product2_parent_to_mifi_review_table_reviewed.csv
#
#   5) Rerun the script.
#
# The final products are created only if the reviewed file exists.

manual_review_file <- file.path(
  output_dir,
  "product2_parent_to_mifi_review_table_reviewed.csv"
)

if (file.exists(manual_review_file)) {
  
  manual_review_decisions <- read_csv(
    manual_review_file,
    show_col_types = FALSE
  ) %>%
    select(
      parent_management_area,
      manual_decision,
      manual_mifi_management_area,
      manual_review_note
    ) %>%
    mutate(
      manual_decision = str_squish(manual_decision),
      manual_mifi_management_area = str_squish(manual_mifi_management_area),
      manual_review_note = str_squish(manual_review_note)
    ) %>%
    filter(
      !is.na(manual_decision),
      manual_decision != ""
    )
  
  # Check manual decision values.
  manual_review_decision_summary <- manual_review_decisions %>%
    count(
      manual_decision,
      sort = TRUE
    )
  
  # Print manual decision summary.
  manual_review_decision_summary
  
  # Stop if unexpected decision values were entered.
  valid_manual_decisions <- c(
    "Accepted manual match",
    "Not represented in MiFI",
    "Needs staff/spatial review"
  )
  
  invalid_manual_decisions <- manual_review_decisions %>%
    filter(
      !manual_decision %in% valid_manual_decisions
    )
  
  if (nrow(invalid_manual_decisions) > 0) {
    print(invalid_manual_decisions)
    stop(
      "Unexpected manual_decision value found. Use only the approved decision values."
    )
  }
  
  
  ###############################################################################
  # ✅ 15. Create Reviewed Parent-to-MiFI Crosswalk
  ###############################################################################
  # ⭐ Why this matters:
  # This applies manual review decisions to the draft crosswalk. Exact matches
  # and accepted manual matches are treated as represented in MiFI.
  
  parent_to_mifi_crosswalk_reviewed <- parent_to_mifi_crosswalk_draft %>%
    left_join(
      manual_review_decisions,
      by = "parent_management_area"
    ) %>%
    mutate(
      
      # Use manual MiFI name when an accepted manual match is provided.
      mifi_management_area = case_when(
        manual_decision == "Accepted manual match" ~
          manual_mifi_management_area,
        
        TRUE ~
          mifi_management_area
      ),
      
      # Update match status based on manual review.
      match_status = case_when(
        !is.na(manual_decision) ~
          manual_decision,
        
        TRUE ~
          match_status
      ),
      
      # Assign final In MiFI field.
      in_mifi = case_when(
        match_status %in% c(
          "Accepted exact match",
          "Accepted manual match"
        ) ~ "Yes",
        
        TRUE ~ "No"
      )
    ) %>%
    select(
      parent_management_area,
      mifi_management_area,
      in_mifi,
      match_status,
      match_method,
      match_quality,
      suggested_mifi_management_area,
      manual_mifi_management_area,
      manual_review_note,
      region,
      region_original,
      wld_unit,
      authority,
      type,
      county,
      -any_of(c(
        "manual_decision"
      ))
    ) %>%
    arrange(
      region,
      wld_unit,
      parent_management_area
    )
  
  # Print reviewed crosswalk.
  parent_to_mifi_crosswalk_reviewed
  
  
  ###############################################################################
  # ❓ 16. Product 1 — Parent Areas Not Listed in MiFI
  ###############################################################################
  # ⭐ Why this matters:
  # This is the main list of parent-list DNR-WLD areas that are not represented
  # in MiFI after exact matching and manual review.
  
  parent_areas_not_listed_in_mifi <- parent_to_mifi_crosswalk_reviewed %>%
    filter(
      in_mifi == "No"
    ) %>%
    select(
      parent_management_area,
      region,
      unit = wld_unit,
      match_status,
      manual_review_note
    ) %>%
    arrange(
      region,
      unit,
      parent_management_area
    )
  
  # Print Product 1.
  parent_areas_not_listed_in_mifi
  
  
  ###############################################################################
  # 📋 17. Product 3 — Final Appendix Table
  ###############################################################################
  # ⭐ Why this matters:
  # This creates the simple appendix-ready table listing all parent-list DNR-WLD
  # management areas, the original MiFI name when matched, Region, Unit, and
  # whether each area was represented in MiFI.
  
  appendix_wld_lands_mifi_table <- parent_to_mifi_crosswalk_reviewed %>%
    select(
      area_name = parent_management_area,
      mifi_name = mifi_management_area,
      region,
      unit = wld_unit,
      in_mifi
    ) %>%
    mutate(
      mifi_name = case_when(
        in_mifi == "Yes" ~ mifi_name,
        TRUE ~ NA_character_
      )
    ) %>%
    arrange(
      region,
      unit,
      area_name
    )
  
  # Print Product 3.
  appendix_wld_lands_mifi_table
  
  
  ###############################################################################
  # 📊 18. Summarize Final Match Results
  ###############################################################################
  # ⭐ Why this matters:
  # This creates a simple summary table for describing the final crosswalk
  # results in text.
  
  match_summary <- parent_to_mifi_crosswalk_reviewed %>%
    count(
      in_mifi,
      match_status,
      name = "records"
    ) %>%
    arrange(
      in_mifi,
      match_status
    )
  
  # Print summary.
  match_summary
  
  
  ###############################################################################
  # ✅ 19. Final Appendix Tables and QA Checks
  ###############################################################################
  # ⭐ Why this matters:
  # This section creates the final appendix tables and checks whether anything
  # slipped through the cracks before final outputs are saved.
  #
  # The appendix will include:
  #
  #   1) A parent administrative reference table showing parent area, Region,
  #      and Unit.
  #
  #   2) A complete administrative crosswalk showing every parent area, whether
  #      it was represented in MiFI, the accepted MiFI name, and any manual
  #      review note.
  #
  # The complete crosswalk also includes reviewed MiFI-only records identified
  # during the reverse QA check. These records have a blank parent_area_name
  # because they were present in MiFI but not found in the parent/reference list.
  
  
  ###############################################################################
  # 19A. Create Parent Administrative Reference Table
  ###############################################################################
  # This is the simple appendix table that documents the parent/reference lands
  # list and the administrative scale used for summaries.
  
  appendix_parent_administrative_reference <- parent_wld_lands %>%
    select(
      parent_area_name = parent_management_area,
      region,
      unit = wld_unit
    ) %>%
    arrange(
      region,
      unit,
      parent_area_name
    )
  
  # Print parent administrative reference table.
  appendix_parent_administrative_reference
  
  
  ###############################################################################
  # 19B. Reverse Check Before Manual MiFI-Only Additions
  ###############################################################################
  # This asks which MiFI management-area names exist in the full MiFI WLD list
  # but were not used in accepted parent-list matches.
  #
  # This is how MiFI-only records such as Fox Islands are detected.
  
  mifi_areas_not_used_in_parent_crosswalk <- mifi_wld_areas %>%
    anti_join(
      parent_to_mifi_crosswalk_reviewed %>%
        filter(
          in_mifi == "Yes"
        ) %>%
        distinct(
          mifi_management_area
        ),
      by = c(
        "mifi_management_area" = "mifi_management_area"
      )
    ) %>%
    arrange(
      mifi_management_area
    )
  
  # Print MiFI areas not used in accepted parent matches.
  mifi_areas_not_used_in_parent_crosswalk
  
  
  ###############################################################################
  # 19C. Manual Addition of MiFI-Only Records
  ###############################################################################
  # This step manually adds reviewed MiFI-only records to the complete appendix
  # crosswalk.
  #
  # These records are not added to the parent administrative reference table
  # because they did not come from the parent/reference list.
  #
  # Leave parent_area_name blank for MiFI-only records.
  
  manual_mifi_only_additions <- tibble::tribble(
    ~parent_area_name, ~region, ~unit, ~in_mifi, ~mifi_name, ~manual_review_note,
    
    NA_character_, "NMR", NA_character_, "Yes", "Fox Islands",
    "Present in MiFI but not found in parent DNR-WLD lands list; retained as MiFI-only reference record"
  )
  
  # Print manual MiFI-only additions.
  manual_mifi_only_additions
  
  
  ###############################################################################
  # 19D. Create Complete Administrative Crosswalk Table
  ###############################################################################
  # This is the complete appendix-ready crosswalk table.
  #
  # It includes every parent-list DNR-WLD area, whether it was represented in
  # MiFI, the accepted original MiFI management-area name, and any manual review
  # note.
  #
  # MiFI-only records identified during reverse QA are added with a blank
  # parent_area_name.
  
  appendix_administrative_crosswalk_complete <- parent_to_mifi_crosswalk_reviewed %>%
    transmute(
      parent_area_name = parent_management_area,
      region,
      unit = wld_unit,
      in_mifi,
      mifi_name = case_when(
        in_mifi == "Yes" ~ mifi_management_area,
        TRUE ~ NA_character_
      ),
      manual_review_note
    ) %>%
    bind_rows(
      manual_mifi_only_additions
    ) %>%
    arrange(
      region,
      unit,
      parent_area_name,
      mifi_name
    )
  
  # Print complete administrative crosswalk table.
  appendix_administrative_crosswalk_complete
  
  
  ###############################################################################
  # 19E. Check Parent-Based Record Counts
  ###############################################################################
  # The parent administrative reference and reviewed crosswalk should match the
  # number of records in the filtered parent DNR-WLD lands list.
  
  parent_record_count_qa <- tibble(
    table = c(
      "Parent DNR-WLD lands",
      "Reviewed parent-to-MiFI crosswalk",
      "Parent administrative reference table"
    ),
    
    records = c(
      nrow(parent_wld_lands),
      nrow(parent_to_mifi_crosswalk_reviewed),
      nrow(appendix_parent_administrative_reference)
    )
  )
  
  # Print parent record count QA.
  parent_record_count_qa
  
  if (length(unique(parent_record_count_qa$records)) != 1) {
    print(parent_record_count_qa)
    stop(
      "Parent-based record counts do not match."
    )
  }
  
  
  ###############################################################################
  # 19F. Check Complete Crosswalk Count
  ###############################################################################
  # The complete administrative crosswalk should equal the parent-list count plus
  # any manually added MiFI-only records.
  
  complete_crosswalk_count_qa <- tibble(
    table = c(
      "Parent DNR-WLD lands",
      "Manual MiFI-only additions",
      "Expected complete crosswalk records",
      "Actual complete crosswalk records"
    ),
    
    records = c(
      nrow(parent_wld_lands),
      nrow(manual_mifi_only_additions),
      nrow(parent_wld_lands) + nrow(manual_mifi_only_additions),
      nrow(appendix_administrative_crosswalk_complete)
    )
  )
  
  # Print complete crosswalk count QA.
  complete_crosswalk_count_qa
  
  if (
    nrow(appendix_administrative_crosswalk_complete) !=
    nrow(parent_wld_lands) + nrow(manual_mifi_only_additions)
  ) {
    print(complete_crosswalk_count_qa)
    stop(
      "Complete administrative crosswalk record count does not match expected count."
    )
  }
  
  
  ###############################################################################
  # 19G. Check for Duplicate Parent Names in Parent Reference Table
  ###############################################################################
  # Ideally this should return zero rows.
  
  parent_reference_duplicate_name_check <- appendix_parent_administrative_reference %>%
    count(
      parent_area_name,
      sort = TRUE
    ) %>%
    filter(
      n > 1
    )
  
  # Print duplicate check.
  parent_reference_duplicate_name_check
  
  if (nrow(parent_reference_duplicate_name_check) > 0) {
    print(parent_reference_duplicate_name_check)
    stop(
      "Duplicate parent-list area names were found in the parent administrative reference table."
    )
  }
  
  
  ###############################################################################
  # 19H. Check for Duplicate Parent Names in Complete Crosswalk
  ###############################################################################
  # This ignores blank parent names because MiFI-only records intentionally have
  # no parent_area_name.
  
  complete_crosswalk_duplicate_parent_check <- appendix_administrative_crosswalk_complete %>%
    filter(
      !is.na(parent_area_name),
      parent_area_name != ""
    ) %>%
    count(
      parent_area_name,
      sort = TRUE
    ) %>%
    filter(
      n > 1
    )
  
  # Print duplicate check.
  complete_crosswalk_duplicate_parent_check
  
  if (nrow(complete_crosswalk_duplicate_parent_check) > 0) {
    print(complete_crosswalk_duplicate_parent_check)
    stop(
      "Duplicate parent-list area names were found in the complete administrative crosswalk."
    )
  }
  
  
  ###############################################################################
  # 19I. Check Final Yes/No Totals
  ###############################################################################
  # This summarizes how many records in the complete crosswalk are represented or
  # not represented in MiFI.
  
  appendix_in_mifi_summary <- appendix_administrative_crosswalk_complete %>%
    count(
      in_mifi,
      name = "records"
    )
  
  # Print Yes/No summary.
  appendix_in_mifi_summary
  
  
  ###############################################################################
  # 19J. Check Match Status Summary for Parent Records
  ###############################################################################
  # This shows how the parent-list In MiFI assignments were made.
  
  match_status_summary_qa <- parent_to_mifi_crosswalk_reviewed %>%
    count(
      in_mifi,
      match_status,
      name = "records"
    ) %>%
    arrange(
      in_mifi,
      match_status
    )
  
  # Print match status summary.
  match_status_summary_qa
  
  
  ###############################################################################
  # 19K. Check for Blank or Unexpected In MiFI Values
  ###############################################################################
  # This makes sure every complete crosswalk record has a clean Yes/No value.
  
  appendix_in_mifi_value_check <- appendix_administrative_crosswalk_complete %>%
    filter(
      is.na(in_mifi) |
        !in_mifi %in% c(
          "Yes",
          "No"
        )
    )
  
  # Print value check.
  appendix_in_mifi_value_check
  
  if (nrow(appendix_in_mifi_value_check) > 0) {
    print(appendix_in_mifi_value_check)
    stop(
      "Unexpected or missing In MiFI values found in the complete administrative crosswalk."
    )
  }
  
  
  ###############################################################################
  # 19L. Check that Yes Records Have a MiFI Name
  ###############################################################################
  # Any record marked Yes should have an accepted MiFI management-area name.
  
  yes_without_mifi_name_check <- appendix_administrative_crosswalk_complete %>%
    filter(
      in_mifi == "Yes",
      is.na(mifi_name) |
        mifi_name == ""
    )
  
  # Print check.
  yes_without_mifi_name_check
  
  if (nrow(yes_without_mifi_name_check) > 0) {
    print(yes_without_mifi_name_check)
    stop(
      "One or more Yes records are missing a MiFI name."
    )
  }
  
  
  ###############################################################################
  # 19M. Check that No Records Do Not Have a MiFI Name
  ###############################################################################
  # Any record marked No should have a blank MiFI name in the appendix crosswalk.
  
  no_with_mifi_name_check <- appendix_administrative_crosswalk_complete %>%
    filter(
      in_mifi == "No",
      !is.na(mifi_name),
      mifi_name != ""
    )
  
  # Print check.
  no_with_mifi_name_check
  
  if (nrow(no_with_mifi_name_check) > 0) {
    print(no_with_mifi_name_check)
    stop(
      "One or more No records still have a MiFI name in the complete administrative crosswalk."
    )
  }
  
  
  ###############################################################################
  # 19N. Check Accepted Manual Matches Against Full MiFI List
  ###############################################################################
  # This confirms that every manually entered MiFI name is actually present in
  # the full MiFI management-area list.
  
  accepted_manual_match_name_check <- parent_to_mifi_crosswalk_reviewed %>%
    filter(
      match_status == "Accepted manual match"
    ) %>%
    filter(
      is.na(mifi_management_area) |
        mifi_management_area == "" |
        !mifi_management_area %in% mifi_wld_areas$mifi_management_area
    ) %>%
    select(
      parent_management_area,
      mifi_management_area,
      manual_mifi_management_area,
      manual_review_note
    )
  
  # Print manual match name check.
  accepted_manual_match_name_check
  
  if (nrow(accepted_manual_match_name_check) > 0) {
    print(accepted_manual_match_name_check)
    stop(
      "One or more accepted manual matches are missing a valid original MiFI management-area name."
    )
  }
  
  
  ###############################################################################
  # 19O. Check Manual MiFI-Only Additions Against Full MiFI List
  ###############################################################################
  # This confirms that manually added MiFI-only records are actually present in
  # the full MiFI management-area list.
  
  manual_mifi_only_name_check <- manual_mifi_only_additions %>%
    filter(
      is.na(mifi_name) |
        mifi_name == "" |
        !mifi_name %in% mifi_wld_areas$mifi_management_area
    )
  
  # Print MiFI-only addition name check.
  manual_mifi_only_name_check
  
  if (nrow(manual_mifi_only_name_check) > 0) {
    print(manual_mifi_only_name_check)
    stop(
      "One or more manual MiFI-only additions are not present in the full MiFI management-area list."
    )
  }
  
  
  ###############################################################################
  # 19P. Reverse Check After Manual MiFI-Only Additions
  ###############################################################################
  # This asks which MiFI management-area names exist in the full MiFI WLD list
  # but were not used in the complete administrative crosswalk after manual
  # MiFI-only additions were included.
  #
  # Ideally, this should return zero rows unless you intentionally decide not to
  # include a MiFI-only record.
  
  mifi_areas_not_used_in_complete_crosswalk <- mifi_wld_areas %>%
    anti_join(
      appendix_administrative_crosswalk_complete %>%
        filter(
          in_mifi == "Yes"
        ) %>%
        distinct(
          mifi_name
        ),
      by = c(
        "mifi_management_area" = "mifi_name"
      )
    ) %>%
    arrange(
      mifi_management_area
    )
  
  # Print MiFI areas not used in complete crosswalk.
  mifi_areas_not_used_in_complete_crosswalk
  
  
  ###############################################################################
  # 19Q. Reverse Check Count
  ###############################################################################
  # This provides a quick count of MiFI management-area names that were not used
  # in the final complete crosswalk.
  
  mifi_areas_not_used_summary <- mifi_areas_not_used_in_complete_crosswalk %>%
    summarise(
      mifi_areas_not_used_in_complete_crosswalk = n()
    )
  
  # Print reverse-check summary.
  mifi_areas_not_used_summary
  
  
  ###############################################################################
  # 19R. Final Parent Records Marked Not Represented in MiFI
  ###############################################################################
  # This prints the final parent-list records marked No so they can be reviewed
  # one last time before the appendix table is used.
  
  final_no_review_check <- parent_areas_not_listed_in_mifi %>%
    arrange(
      region,
      unit,
      parent_management_area
    )
  
  # Print final No review list.
  final_no_review_check
  
  
  ###############################################################################
  # 19S. Appendix Table Caption Text
  ###############################################################################
  # ⭐ Why this matters:
  # These captions can be copied into the report appendix.
  
  appendix_parent_reference_caption <- paste0(
    "Parent administrative reference for DNR-WLD lands included in the ",
    "administrative crosswalk. Region and Unit reflect the parent/reference ",
    "lands list, with region labels standardized to SMR and NMR."
  )
  
  appendix_complete_crosswalk_caption <- paste0(
    "Complete administrative crosswalk between the parent/reference DNR-WLD ",
    "lands list and Wildlife Division MiFI management-area names. Parent area ",
    "name is blank for MiFI-only records identified during reverse QA review. ",
    "The MiFI name column shows the original MiFI management-area name for ",
    "accepted matches and is blank where no accepted MiFI match was identified."
  )
  
  # Print appendix captions.
  appendix_parent_reference_caption
  appendix_complete_crosswalk_caption
  
  
  ###############################################################################
  # 💾 20. Export Final Products
  ###############################################################################
  # ⭐ Why this matters:
  # This exports the final reviewed products after the manual review file has
  # been completed.
  
  write_csv(
    parent_areas_not_listed_in_mifi,
    file.path(output_dir, "product1_parent_areas_not_listed_in_mifi.csv")
  )
  
  write_csv(
    appendix_parent_administrative_reference,
    file.path(output_dir, "appendix_parent_administrative_reference.csv")
  )
  
  write_csv(
    appendix_administrative_crosswalk_complete,
    file.path(output_dir, "appendix_administrative_crosswalk_complete.csv")
  )
  
  write_csv(
    parent_to_mifi_crosswalk_reviewed,
    file.path(output_dir, "parent_to_mifi_crosswalk_reviewed.csv")
  )
  
  write_csv(
    match_summary,
    file.path(output_dir, "match_summary.csv")
  )
  
  write_csv(
    mifi_areas_not_used_in_parent_crosswalk,
    file.path(output_dir, "qa_mifi_areas_not_used_in_parent_crosswalk.csv")
  )
  
  write_csv(
    mifi_areas_not_used_in_complete_crosswalk,
    file.path(output_dir, "qa_mifi_areas_not_used_in_complete_crosswalk.csv")
  )
  
  write_csv(
    final_no_review_check,
    file.path(output_dir, "qa_final_parent_records_not_represented_in_mifi.csv")
  )
  
  # Check final exports.
  list.files(output_dir)
  
} else {
  
  message(
    paste0(
      "\nManual review file not found: ",
      manual_review_file,
      "\n\nDraft products were exported. Complete product2_parent_to_mifi_review_table.csv ",
      "in Excel, save it as product2_parent_to_mifi_review_table_reviewed.csv, ",
      "and rerun the script to create final products.\n"
    )
  )
}


###############################################################################
# 🧭 21. Administrative Summaries by Region and Unit
###############################################################################
# ⭐ Why this matters:
# This uses the final administrative crosswalk from Step 4 to summarize the
# Step 3 management-area results by Region and Unit.
#
# This avoids editing the large Step 3 script. Step 3 remains the place where
# management-area acres were calculated. Step 4 becomes the place where those
# results are assigned to administrative Region and Unit.

management_area_summary <- read_csv(
  "phase1_step3_outputs/wld_management_area_full_summary.csv",
  show_col_types = FALSE
)

# Create the Region/Unit lookup from the final complete crosswalk.
admin_lookup <- appendix_administrative_crosswalk_complete %>%
  filter(
    in_mifi == "Yes",
    !is.na(mifi_name),
    mifi_name != ""
  ) %>%
  select(
    unit_key = mifi_name,
    region,
    unit
  ) %>%
  distinct()

# Join Region and Unit onto the Step 3 management-area summary.
management_area_summary_admin <- management_area_summary %>%
  left_join(
    admin_lookup,
    by = "unit_key"
  )

# QA: identify Step 3 management areas that did not receive Region/Unit.
management_area_missing_admin_check <- management_area_summary_admin %>%
  filter(
    is.na(region) |
      region == "" |
      is.na(unit) |
      unit == ""
  ) %>%
  select(
    unit_key,
    region,
    unit,
    total_terrestrial_acres,
    agriculture_acres,
    herbaceous_openland_acres,
    agriculture_and_herbaceous_openland_acres
  ) %>%
  arrange(
    unit_key
  )

# Print missing admin check.
management_area_missing_admin_check


###############################################################################
# 21A. Summarize by Region
###############################################################################
# ⭐ Why this matters:
# This creates regional totals. These totals are then used as denominators for
# Unit-within-Region percentages.

region_summary <- management_area_summary_admin %>%
  group_by(
    region
  ) %>%
  summarise(
    management_areas = n_distinct(unit_key),
    
    total_terrestrial_acres =
      sum(total_terrestrial_acres, na.rm = TRUE),
    
    agriculture_acres =
      sum(agriculture_acres, na.rm = TRUE),
    
    herbaceous_openland_acres =
      sum(herbaceous_openland_acres, na.rm = TRUE),
    
    agriculture_and_herbaceous_openland_acres =
      sum(agriculture_and_herbaceous_openland_acres, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    agriculture_percent_terrestrial_acres =
      100 * agriculture_acres / total_terrestrial_acres,
    
    herbaceous_openland_percent_terrestrial_acres =
      100 * herbaceous_openland_acres / total_terrestrial_acres,
    
    agriculture_and_herbaceous_openland_percent_terrestrial_acres =
      100 * agriculture_and_herbaceous_openland_acres / total_terrestrial_acres
  ) %>%
  arrange(
    region
  )

# Print Region summary.
region_summary


###############################################################################
# 21B. Summarize Units Within Region
###############################################################################
# ⭐ Why this matters:
# This summarizes each Unit within its Region. Percentages are calculated using
# the Region total as the denominator, not the statewide WLD total.
#
# This is useful because NMR and SMR are very different landscapes and because
# the NMR administrative data are less reliable for this assessment.

unit_summary_by_region <- management_area_summary_admin %>%
  mutate(
    unit = case_when(
      is.na(unit) | unit == "" ~ "Unassigned / MiFI-only",
      TRUE ~ unit
    )
  ) %>%
  group_by(
    region,
    unit
  ) %>%
  summarise(
    management_areas = n_distinct(unit_key),
    
    total_terrestrial_acres =
      sum(total_terrestrial_acres, na.rm = TRUE),
    
    agriculture_acres =
      sum(agriculture_acres, na.rm = TRUE),
    
    herbaceous_openland_acres =
      sum(herbaceous_openland_acres, na.rm = TRUE),
    
    agriculture_and_herbaceous_openland_acres =
      sum(agriculture_and_herbaceous_openland_acres, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  left_join(
    region_summary %>%
      select(
        region,
        region_total_terrestrial_acres = total_terrestrial_acres,
        region_agriculture_acres = agriculture_acres,
        region_herbaceous_openland_acres = herbaceous_openland_acres,
        region_agriculture_and_herbaceous_openland_acres =
          agriculture_and_herbaceous_openland_acres
      ),
    by = "region"
  ) %>%
  mutate(
    # Percent of each Region's full terrestrial MiFI acreage.
    percent_of_region_terrestrial_acres =
      100 * total_terrestrial_acres / region_total_terrestrial_acres,
    
    # Percent of each Region's Agriculture acreage.
    percent_of_region_agriculture_acres =
      100 * agriculture_acres / region_agriculture_acres,
    
    # Percent of each Region's Herbaceous Openland acreage.
    percent_of_region_herbaceous_openland_acres =
      100 * herbaceous_openland_acres / region_herbaceous_openland_acres,
    
    # Percent of each Region's combined Agriculture + Herbaceous Openland acres.
    percent_of_region_agriculture_and_herbaceous_openland_acres =
      100 * agriculture_and_herbaceous_openland_acres /
      region_agriculture_and_herbaceous_openland_acres,
    
    # Within-Unit composition.
    agriculture_percent_unit_terrestrial_acres =
      100 * agriculture_acres / total_terrestrial_acres,
    
    herbaceous_openland_percent_unit_terrestrial_acres =
      100 * herbaceous_openland_acres / total_terrestrial_acres,
    
    agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres =
      100 * agriculture_and_herbaceous_openland_acres /
      total_terrestrial_acres
  ) %>%
  arrange(
    region,
    desc(percent_of_region_agriculture_and_herbaceous_openland_acres)
  )

# Print Unit-within-Region summary.
unit_summary_by_region


###############################################################################
# 21C. Export Administrative Summary Tables
###############################################################################

write_csv(
  management_area_summary_admin,
  file.path(output_dir, "wld_management_area_summary_with_region_unit.csv")
)

write_csv(
  region_summary,
  file.path(output_dir, "wld_region_summary.csv")
)

write_csv(
  unit_summary_by_region,
  file.path(output_dir, "wld_unit_summary_by_region.csv")
)

write_csv(
  management_area_missing_admin_check,
  file.path(output_dir, "qa_management_area_missing_region_unit.csv")
)


###############################################################################
# 🗺️ 22. Unit-Level Maps for Agriculture and Herbaceous Openland
###############################################################################
# ⭐ Why this matters:
# These maps show where project-relevant cover classes are concentrated across
# Wildlife Division administrative Units.
#
# Both percent-based and acre-based maps are created:
#   • Percent maps standardize by each Unit's terrestrial land base.
#   • Acre maps show the total inventoried acres associated with each Unit.
#
# NOTE:
# These maps use Unit polygons only for display. Summary values come from the
# administrative crosswalk and are joined to the Unit layer by Unit and Region.
# They are not derived by clipping stands to Unit boundaries.

library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)


###############################################################################
# Read Wildlife Unit polygons
###############################################################################
# This is the administrative Unit layer used only for mapping/display.

unit_map_sf <- st_read(
  dsn = gdb,
  layer = "DNRWildlifeManagementUnitsFieldOperationsCoverageAreas_ExportFeatures",
  quiet = TRUE
) %>%
  clean_names() %>%
  mutate(
    
    # Extract abbreviation from names like "Big River Unit (BRU)".
    unit = str_extract(
      field_operation_unit,
      "(?<=\\()[A-Za-z0-9]+(?=\\))"
    ),
    
    # Standardize Region names to match the Step 4 summary table.
    region = case_when(
      wildlife_region == "Southern Michigan Region" ~ "SMR",
      wildlife_region == "Northern Michigan Region" ~ "NMR",
      TRUE ~ wildlife_region
    )
  ) %>%
  select(
    unit,
    region,
    field_operation_unit,
    wildlife_region,
    everything()
  )

# Check prepared Unit layer.
unit_map_sf %>%
  st_drop_geometry() %>%
  select(
    unit,
    region,
    field_operation_unit,
    wildlife_region
  )


###############################################################################
# Prepare Unit summary values for mapping
###############################################################################
# This assumes wld_unit_summary_by_region already exists.
#
# Expected percent columns:
#   agriculture_percent_terrestrial_acres
#   herbaceous_openland_percent_terrestrial_acres
#   agriculture_and_herbaceous_openland_percent_terrestrial_acres
#
# Expected acre columns:
#   agriculture_acres
#   herbaceous_openland_acres
#
# If the combined-acre column does not already exist, create it here.

unit_summary_map_tbl <- unit_summary_by_region %>%
  mutate(
    agriculture_and_herbaceous_openland_acres =
      if_else(
        !is.na(agriculture_acres) & !is.na(herbaceous_openland_acres),
        agriculture_acres + herbaceous_openland_acres,
        NA_real_
      )
  )

# Join Unit summary values to Unit polygons.
unit_map_data <- unit_map_sf %>%
  left_join(
    unit_summary_map_tbl,
    by = c("region", "unit")
  )

# Print joined table.
unit_map_data


###############################################################################
# Build helper function for Unit maps
###############################################################################
# This keeps map formatting consistent across cover classes and Regions.

###############################################################################
# Build helper function for Unit maps
###############################################################################
# This keeps map formatting consistent across cover classes and Regions without
# assigning custom colors.

make_unit_map <- function(
    data,
    region_filter,
    value_col,
    legend_title,
    label_type = c("percent", "acres")
) {
  
  label_type <- match.arg(label_type)
  
  plot_data <- data %>%
    filter(region == region_filter)
  
  fill_scale <- if (label_type == "percent") {
    scale_fill_continuous(
      labels = function(x) paste0(round(x, 1), "%"),
      name = legend_title
    )
  } else {
    scale_fill_continuous(
      labels = comma_format(accuracy = 1),
      name = legend_title
    )
  }
  
  ggplot(plot_data) +
    geom_sf(
      aes(fill = .data[[value_col]]),
      linewidth = 0.3
    ) +
    geom_sf_text(
      aes(label = unit),
      size = 3
    ) +
    fill_scale +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_void() +
    theme(
      legend.position = "right"
    )
}


###############################################################################
# SMR Percent Maps
###############################################################################
# Percent values represent the share of each Unit's own terrestrial land base in
# the indicated cover class.

smr_ag_herb_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres",
  legend_title = "% Agriculture +\nHerbaceous Openland",
  label_type = "percent"
)

smr_ag_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "agriculture_percent_unit_terrestrial_acres",
  legend_title = "% Agriculture",
  label_type = "percent"
)

smr_herb_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "herbaceous_openland_percent_unit_terrestrial_acres",
  legend_title = "% Herbaceous\nOpenland",
  label_type = "percent"
)

# Print SMR percent maps.
smr_ag_herb_percent_map
smr_ag_percent_map
smr_herb_percent_map


###############################################################################
# NMR Percent Maps
###############################################################################
# Percent values represent the share of each Unit's own terrestrial land base in
# the indicated cover class.

nmr_ag_herb_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres",
  legend_title = "% Agriculture +\nHerbaceous Openland",
  label_type = "percent"
)

nmr_ag_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "agriculture_percent_unit_terrestrial_acres",
  legend_title = "% Agriculture",
  label_type = "percent"
)

nmr_herb_percent_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "herbaceous_openland_percent_unit_terrestrial_acres",
  legend_title = "% Herbaceous\nOpenland",
  label_type = "percent"
)

# Print NMR percent maps.
nmr_ag_herb_percent_map
nmr_ag_percent_map
nmr_herb_percent_map

###############################################################################
# SMR Acre Maps
###############################################################################
# Acre values represent total inventoried acres associated with each Unit.

smr_ag_herb_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "agriculture_and_herbaceous_openland_acres",
  legend_title = "Agriculture +\nHerbaceous Openland\n(acres)",
  label_type = "acres"
)

smr_ag_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "agriculture_acres",
  legend_title = "Agriculture\n(acres)",
  label_type = "acres"
)

smr_herb_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "herbaceous_openland_acres",
  legend_title = "Herbaceous\nOpenland\n(acres)",
  label_type = "acres"
)

# Print SMR acre maps.
smr_ag_herb_acre_map
smr_ag_acre_map
smr_herb_acre_map


###############################################################################
# NMR Acre Maps
###############################################################################
# These use the same acre summaries and display style as the SMR maps.

nmr_ag_herb_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "agriculture_and_herbaceous_openland_acres",
  legend_title = "Agriculture +\nHerbaceous Openland\n(acres)",
  label_type = "acres"
)

nmr_ag_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "agriculture_acres",
  legend_title = "Agriculture\n(acres)",
  label_type = "acres"
)

nmr_herb_acre_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "NMR",
  value_col = "herbaceous_openland_acres",
  legend_title = "Herbaceous\nOpenland\n(acres)",
  label_type = "acres"
)

# Print NMR acre maps.
nmr_ag_herb_acre_map
nmr_ag_acre_map
nmr_herb_acre_map


###############################################################################
# Optional: Save Figures
###############################################################################
# Percent maps.
ggsave(
  file.path(output_dir, "fig_smr_unit_ag_herb_percent_map.png"),
  smr_ag_herb_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_smr_unit_ag_percent_map.png"),
  smr_ag_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_smr_unit_herb_percent_map.png"),
  smr_herb_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_ag_herb_percent_map.png"),
  nmr_ag_herb_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_ag_percent_map.png"),
  nmr_ag_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_herb_percent_map.png"),
  nmr_herb_percent_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

# Acre maps.
ggsave(
  file.path(output_dir, "fig_smr_unit_ag_herb_acre_map.png"),
  smr_ag_herb_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_smr_unit_ag_acre_map.png"),
  smr_ag_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_smr_unit_herb_acre_map.png"),
  smr_herb_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_ag_herb_acre_map.png"),
  nmr_ag_herb_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_ag_acre_map.png"),
  nmr_ag_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_nmr_unit_herb_acre_map.png"),
  nmr_herb_acre_map,
  width = 8.5,
  height = 6.5,
  dpi = figure_dpi
)


###############################################################################
# End of script
###############################################################################