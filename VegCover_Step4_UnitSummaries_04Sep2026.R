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
library(shadowtext)
library(ggspatial)

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
# 🗺️ 22. Main Unit-Level Results Figure
###############################################################################
# ⭐ Why this matters:
# This section creates the main administrative Unit-level figure pieces for the
# Results section.
#
# The figure focuses on the Southern Michigan Region because most inventoried
# Agriculture and Herbaceous Openland acres occur there, and because Northern
# Michigan Region summaries have greater data limitations.
#
# These maps use Unit polygons only for display. Summary values come from the
# administrative crosswalk and are joined to the Unit layer by Unit and Region.
# They are not derived by clipping stands to Unit boundaries.

library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(shadowtext)
library(ggspatial)
library(grid)


###############################################################################
# Read Wildlife Unit Polygons
###############################################################################
# This is the administrative Unit layer used only for mapping/display.

unit_map_sf <- st_read(
  dsn = gdb,
  layer = "DNRWildlifeManagementUnitsFieldOperationsCoverageAreas_ExportFeatures",
  quiet = TRUE
) %>%
  clean_names() %>%
  st_make_valid() %>%
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
# Prepare Unit Summary Values for Mapping
###############################################################################
# This uses the Unit-within-Region summary created in Step 21.

unit_summary_map_tbl <- unit_summary_by_region %>%
  mutate(
    agriculture_and_herbaceous_openland_acres =
      agriculture_acres + herbaceous_openland_acres
  )

# Check available fields.
unit_summary_map_tbl %>%
  select(
    region,
    unit,
    total_terrestrial_acres,
    agriculture_acres,
    herbaceous_openland_acres,
    agriculture_and_herbaceous_openland_acres,
    agriculture_percent_unit_terrestrial_acres,
    herbaceous_openland_percent_unit_terrestrial_acres,
    agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres
  )


###############################################################################
# Join Unit Summaries to Unit Polygons and Dissolve by Unit
###############################################################################
# ⭐ Why this matters:
# The Unit boundary layer may contain multiple polygons for the same Unit.
# Dissolving by Region and Unit creates one mapped feature per Unit so each Unit
# is labeled once.
#
# The small buffer step helps remove internal seam artifacts from adjacent
# polygon pieces that belong to the same Unit. This is for map display only and
# does not change the acreage summaries.

unit_map_data <- unit_map_sf %>%
  st_buffer(1) %>%
  left_join(
    unit_summary_map_tbl,
    by = c(
      "region",
      "unit"
    )
  ) %>%
  group_by(
    region,
    unit
  ) %>%
  summarise(
    field_operation_unit =
      first(field_operation_unit),
    
    wildlife_region =
      first(wildlife_region),
    
    management_areas =
      first(management_areas),
    
    total_terrestrial_acres =
      first(total_terrestrial_acres),
    
    agriculture_acres =
      first(agriculture_acres),
    
    herbaceous_openland_acres =
      first(herbaceous_openland_acres),
    
    agriculture_and_herbaceous_openland_acres =
      first(agriculture_and_herbaceous_openland_acres),
    
    region_total_terrestrial_acres =
      first(region_total_terrestrial_acres),
    
    region_agriculture_acres =
      first(region_agriculture_acres),
    
    region_herbaceous_openland_acres =
      first(region_herbaceous_openland_acres),
    
    region_agriculture_and_herbaceous_openland_acres =
      first(region_agriculture_and_herbaceous_openland_acres),
    
    percent_of_region_terrestrial_acres =
      first(percent_of_region_terrestrial_acres),
    
    percent_of_region_agriculture_acres =
      first(percent_of_region_agriculture_acres),
    
    percent_of_region_herbaceous_openland_acres =
      first(percent_of_region_herbaceous_openland_acres),
    
    percent_of_region_agriculture_and_herbaceous_openland_acres =
      first(percent_of_region_agriculture_and_herbaceous_openland_acres),
    
    agriculture_percent_unit_terrestrial_acres =
      first(agriculture_percent_unit_terrestrial_acres),
    
    herbaceous_openland_percent_unit_terrestrial_acres =
      first(herbaceous_openland_percent_unit_terrestrial_acres),
    
    agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres =
      first(agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres),
    
    .groups = "drop",
    do_union = TRUE
  ) %>%
  st_buffer(-1) %>%
  st_make_valid()

# Print dissolved map layer.
unit_map_data


###############################################################################
# QA: Confirm One Feature per Region and Unit
###############################################################################
# Each Region + Unit should have one row after dissolving.

unit_map_dissolve_check <- unit_map_data %>%
  st_drop_geometry() %>%
  count(
    region,
    unit,
    sort = TRUE
  )

unit_map_dissolve_check


###############################################################################
# QA: Check for Units Missing Summary Values
###############################################################################
# This identifies Unit polygons that did not receive acre or percent summaries.

unit_map_join_check <- unit_map_data %>%
  st_drop_geometry() %>%
  filter(
    is.na(agriculture_and_herbaceous_openland_acres)
  ) %>%
  select(
    region,
    unit,
    field_operation_unit,
    wildlife_region
  ) %>%
  arrange(
    region,
    unit
  )

unit_map_join_check


###############################################################################
# Build Unit Map Helper Function
###############################################################################
# This keeps map formatting consistent across acre and percent maps.

# NOTE:
# Some internal seams may remain from the source Unit geometry even after
# dissolving. These are cartographic artifacts only; Unit summaries are based on
# one dissolved Region + Unit record.

###############################################################################
# Build Unit Map Helper Function
###############################################################################
# This keeps map formatting consistent across acre and percent maps.

###############################################################################
# Build Unit Map Helper Function
###############################################################################
# This keeps map formatting consistent across acre and percent maps.

###############################################################################
###############################################################################
# Build Unit Map Helper Function
###############################################################################
# This keeps map formatting consistent across acre and percent maps.

###############################################################################
# Build Unit Map Helper Function
###############################################################################
# This keeps map formatting consistent across acre and percent maps and allows
# either Brewer palettes or custom two-color gradients.

make_unit_map <- function(
    data,
    region_filter,
    value_col,
    legend_title,
    label_type = c("percent", "acres"),
    palette_name = NULL,
    fill_low = NULL,
    fill_high = NULL
) {
  
  label_type <- match.arg(label_type)
  
  # Filter to the requested Region.
  plot_data <- data %>%
    filter(
      region == region_filter
    )
  
  # Create one label point inside each Unit polygon.
  label_data <- plot_data %>%
    st_point_on_surface() %>%
    mutate(
      label_x = st_coordinates(.)[, 1],
      label_y = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
  
  # Format legend labels.
  if (label_type == "percent") {
    
    if (!is.null(fill_low) & !is.null(fill_high)) {
      
      fill_scale <- scale_fill_gradient(
        low = fill_low,
        high = fill_high,
        breaks = scales::pretty_breaks(n = 4),
        labels = function(x) paste0(round(x, 1), "%"),
        name = legend_title
      )
      
    } else {
      
      fill_scale <- scale_fill_distiller(
        palette = palette_name,
        direction = 1,
        breaks = scales::pretty_breaks(n = 4),
        labels = function(x) paste0(round(x, 1), "%"),
        name = legend_title
      )
    }
    
  } else {
    
    if (!is.null(fill_low) & !is.null(fill_high)) {
      
      fill_scale <- scale_fill_gradient(
        low = fill_low,
        high = fill_high,
        breaks = scales::pretty_breaks(n = 4),
        labels = scales::comma_format(accuracy = 1),
        name = legend_title
      )
      
    } else {
      
      fill_scale <- scale_fill_distiller(
        palette = palette_name,
        direction = 1,
        breaks = scales::pretty_breaks(n = 4),
        labels = scales::comma_format(accuracy = 1),
        name = legend_title
      )
    }
  }
  
  # Build map.
  ggplot() +
    geom_sf(
      data = plot_data,
      aes(
        fill = .data[[value_col]]
      ),
      color = "grey35",
      linewidth = 0.3
    ) +
    shadowtext::geom_shadowtext(
      data = label_data,
      aes(
        x = label_x,
        y = label_y,
        label = unit
      ),
      color = "black",
      bg.color = "white",
      bg.r = 0.15,
      size = 3.5
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.25,
      unit_category = "imperial",
      text_cex = 0.75,
      pad_x = grid::unit(0.25, "in"),
      pad_y = grid::unit(0.25, "in")
    ) +
    fill_scale +
    coord_sf(
      ylim = c(
        st_bbox(plot_data)$ymin - 30000,
        st_bbox(plot_data)$ymax
      ),
      expand = FALSE
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(
        face = "bold",
        size = 10
      ),
      legend.text = element_text(
        size = 9
      ),
      legend.box.margin = margin(0, 0, 0, 25),
      plot.margin = margin(10, 20, 20, 10)
    )
}

###############################################################################
# Create SMR Main Figure Table
###############################################################################
# This table is used for the bar chart panels.
#
# The figures use three related but distinct measures:
#
#   Panel A: Each Unit's share of the SMR total.
#
#   Panel B: Total inventoried acres by Unit.
#
#   Panel C: Cover type as a percent of each Unit's own inventoried
#            terrestrial acres.

smr_unit_main_figure_tbl <- unit_summary_by_region %>%
  filter(
    region == "SMR"
  ) %>%
  mutate(
    
    # Combined Agriculture + Herbaceous Openland ordering.
    unit_combined_acres_order = reorder(
      unit,
      agriculture_and_herbaceous_openland_acres
    ),
    
    unit_combined_percent_order = reorder(
      unit,
      agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres
    ),
    
    # Agriculture ordering.
    unit_agriculture_acres_order = reorder(
      unit,
      agriculture_acres
    ),
    
    unit_agriculture_percent_order = reorder(
      unit,
      agriculture_percent_unit_terrestrial_acres
    ),
    
    # Herbaceous Openland ordering.
    unit_herbaceous_openland_acres_order = reorder(
      unit,
      herbaceous_openland_acres
    ),
    
    unit_herbaceous_openland_percent_order = reorder(
      unit,
      herbaceous_openland_percent_unit_terrestrial_acres
    )
  )


###############################################################################
# Combined Agriculture + Herbaceous Openland Figure
###############################################################################

###############################################################################
# Panel A. SMR Map — Percent of Regional Combined Acres by Unit
###############################################################################

smr_main_combined_region_share_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "percent_of_region_agriculture_and_herbaceous_openland_acres",
  legend_title = "Agriculture +\nHerbaceous Openland\n(% of WLD regional total)",
  label_type = "percent",
  palette_name = "Reds"
)

# Print map.
smr_main_combined_region_share_map


###############################################################################
# Panel B. SMR Bar Chart — Combined Acres by Unit
###############################################################################
# This bar chart shows total inventoried acres. Acres are important because they
# represent management footprint, workload, and resource needs.

smr_main_combined_acres_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_combined_acres_order,
    y = agriculture_and_herbaceous_openland_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = comma(
        round(agriculture_and_herbaceous_openland_acres, 0)
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Inventoried Agriculture and Herbaceous\nOpenland (acres)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_main_combined_acres_bar


###############################################################################
# Panel C. SMR Bar Chart — Combined Percent of Unit Terrestrial Acres
###############################################################################
# This bar chart standardizes by each Unit's own inventoried terrestrial land
# base. This helps compare Units with different total Wildlife Division acreage.

smr_main_combined_percent_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_combined_percent_order,
    y = agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = paste0(
        round(
          agriculture_and_herbaceous_openland_percent_unit_terrestrial_acres,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of each Unit's terrestrial acres"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_main_combined_percent_bar


###############################################################################
# Agriculture Figure
###############################################################################

###############################################################################
# Panel A. SMR Map — Percent of Regional Agriculture Acres by Unit
###############################################################################

smr_agriculture_region_share_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "percent_of_region_agriculture_acres",
  legend_title = "Agriculture\n(% of WLD regional total)",
  label_type = "percent",
  fill_low = "#FFF7CC",
  fill_high = "#C49A2C"
)

# Print map.
smr_agriculture_region_share_map


###############################################################################
# Panel B. SMR Bar Chart — Agriculture Acres by Unit
###############################################################################
# This bar chart shows total inventoried Agriculture acres by Unit.

smr_agriculture_acres_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_agriculture_acres_order,
    y = agriculture_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = comma(
        round(agriculture_acres, 0)
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Inventoried Agriculture (acres)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_agriculture_acres_bar


###############################################################################
# Panel C. SMR Bar Chart — Agriculture Percent of Unit Terrestrial Acres
###############################################################################
# This bar chart standardizes Agriculture by each Unit's own inventoried
# terrestrial land base.

smr_agriculture_percent_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_agriculture_percent_order,
    y = agriculture_percent_unit_terrestrial_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = paste0(
        round(
          agriculture_percent_unit_terrestrial_acres,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of each Unit's terrestrial acres"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_agriculture_percent_bar


###############################################################################
# Herbaceous Openland Figure
###############################################################################

###############################################################################
# Panel A. SMR Map — Percent of Regional Herbaceous Openland Acres by Unit
###############################################################################

smr_herbaceous_openland_region_share_map <- make_unit_map(
  data = unit_map_data,
  region_filter = "SMR",
  value_col = "percent_of_region_herbaceous_openland_acres",
  legend_title = "Herbaceous Openland\n(% of WLD regional total)",
  label_type = "percent",
  fill_low = "honeydew",
  fill_high = "springgreen4"
)

# Print map.
smr_herbaceous_openland_region_share_map

###############################################################################
# Panel B. SMR Bar Chart — Herbaceous Openland Acres by Unit
###############################################################################
# This bar chart shows total inventoried Herbaceous Openland acres by Unit.

smr_herbaceous_openland_acres_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_herbaceous_openland_acres_order,
    y = herbaceous_openland_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = comma(
        round(herbaceous_openland_acres, 0)
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Inventoried Herbaceous Openland (acres)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_herbaceous_openland_acres_bar


###############################################################################
# Panel C. SMR Bar Chart — Herbaceous Openland Percent of Unit Terrestrial Acres
###############################################################################
# This bar chart standardizes Herbaceous Openland by each Unit's own inventoried
# terrestrial land base.

smr_herbaceous_openland_percent_bar <- ggplot(
  smr_unit_main_figure_tbl,
  aes(
    x = unit_herbaceous_openland_percent_order,
    y = herbaceous_openland_percent_unit_terrestrial_acres
  )
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(
      label = paste0(
        round(
          herbaceous_openland_percent_unit_terrestrial_acres,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of each Unit's terrestrial acres"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold")
  )

# Print bar chart.
smr_herbaceous_openland_percent_bar


###############################################################################
# Export Main Results Figure Pieces
###############################################################################

# Combined Agriculture + Herbaceous Openland.

ggsave(
  file.path(output_dir, "fig_main_A_smr_combined_region_share_map.png"),
  smr_main_combined_region_share_map,
  width = 8.5,
  height = 6.5,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_main_B_smr_combined_acres_bar.png"),
  smr_main_combined_acres_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_main_C_smr_combined_percent_bar.png"),
  smr_main_combined_percent_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)


# Agriculture.

ggsave(
  file.path(output_dir, "fig_agriculture_A_smr_region_share_map.png"),
  smr_agriculture_region_share_map,
  width = 8.5,
  height = 6.5,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_agriculture_B_smr_acres_bar.png"),
  smr_agriculture_acres_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_agriculture_C_smr_percent_bar.png"),
  smr_agriculture_percent_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)


# Herbaceous Openland.

ggsave(
  file.path(output_dir, "fig_herbaceous_openland_A_smr_region_share_map.png"),
  smr_herbaceous_openland_region_share_map,
  width = 8.5,
  height = 6.5,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_herbaceous_openland_B_smr_acres_bar.png"),
  smr_herbaceous_openland_acres_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_herbaceous_openland_C_smr_percent_bar.png"),
  smr_herbaceous_openland_percent_bar,
  width = 3.5,
  height = 3,
  dpi = 300
)


###############################################################################
# Create Unit Scale Context Tables
###############################################################################
# ⭐ Why this matters:
# These tables give readers context for the administrative scale of each Unit.
#
# The SMR table is used as context for the main Unit figures and only reports
# the number of represented MiFI management areas and total inventoried Wildlife
# Division terrestrial acres.
#
# The NMR table includes additional Agriculture and Herbaceous Openland acreage
# summaries so NMR results can be reported transparently without overemphasizing
# maps or figures.

###############################################################################
# SMR Unit Scale Context Table
###############################################################################
# This table supports the SMR Unit figures in the main Results section.
#
# These totals describe inventoried Wildlife Division-administered lands assigned
# to each Unit. They do not represent the full geographic area of Unit boundaries.

smr_unit_scale_context_table <- unit_summary_by_region %>%
  filter(
    region == "SMR"
  ) %>%
  arrange(
    desc(total_terrestrial_acres)
  ) %>%
  transmute(
    Unit = unit,
    `Management areas represented in MiFI` = management_areas,
    `Inventoried WLD terrestrial acres` = round(total_terrestrial_acres, 0)
  )

# Print SMR table.
smr_unit_scale_context_table


###############################################################################
# NMR Unit Summary Table
###############################################################################
# This table reports Northern Michigan Region Unit summaries separately.
#
# Interpretation should note that NMR MiFI representation is incomplete. These
# values summarize inventoried Wildlife Division-administered lands assigned to
# each Unit and do not represent the full geographic area of Unit boundaries.

nmr_unit_summary_table <- unit_summary_by_region %>%
  filter(
    region == "NMR"
  ) %>%
  arrange(
    desc(total_terrestrial_acres)
  ) %>%
  transmute(
    Unit = unit,
    `Management areas represented in MiFI` = management_areas,
    `Inventoried WLD terrestrial acres` = round(total_terrestrial_acres, 0),
    `Agriculture acres` = round(agriculture_acres, 0),
    `Herbaceous Openland acres` = round(herbaceous_openland_acres, 0),
    `Agriculture + Herbaceous Openland acres` =
      round(agriculture_and_herbaceous_openland_acres, 0)
  )

# Print NMR table.
nmr_unit_summary_table


###############################################################################
# Export Unit Scale and NMR Summary Tables
###############################################################################

write_csv(
  smr_unit_scale_context_table,
  file.path(output_dir, "table_unit_scale_context_smr.csv")
)

write_csv(
  nmr_unit_summary_table,
  file.path(output_dir, "table_nmr_unit_summary.csv")
)


###############################################################################
# End of script
###############################################################################