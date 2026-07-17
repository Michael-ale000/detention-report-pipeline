# add_operator_info_to_index.r
# Adds Owner, Operator, and Institutional Type to a copy of index.json without
# touching index.json itself.
#
# Source: ICE_Facility_Owner&Operator.xlsx joined on DETLOC — the same join
# used in Individual_Internal_ADP.r.
#
# Usage: source("add_operator_info_to_index.r")
# Output: index_with_operator_info.json in the project root

library(dplyr)
library(jsonlite)
library(openxlsx)

# ---- Load the existing index (read-only; index.json is never modified) ----
index_df <- jsonlite::read_json("index.json", simplifyVector = TRUE)

# ---- Load Owner/Operator/Institutional Type, keyed by DETLOC ----
operator_info <- openxlsx::read.xlsx("./ICE_Facility_Owner&Operator.xlsx") |>
  select(DETLOC, Owner, Operator, Institutional.Type)

# ---- Join — every existing index.json field is carried through unchanged ----
index_with_operator <- index_df |>
  left_join(operator_info, by = "DETLOC")

missing_operator_info <- index_with_operator |>
  filter(DETLOC != "NOTES", is.na(Owner) & is.na(Operator) & is.na(Institutional.Type))

if (nrow(missing_operator_info) > 0) {
  message("\n========================================")
  message("NO OWNER/OPERATOR MATCH — ", nrow(missing_operator_info), " facility(ies) not found in ICE_Facility_Owner&Operator.xlsx by DETLOC:")
  for (nm in missing_operator_info$Name) message("  - ", nm)
  message("========================================\n")
}

# ---- Write to a separate file, leaving index.json untouched ----
output_path <- "index_with_operator_info.json"

write_json(
  index_with_operator,
  output_path,
  pretty      = 4,
  auto_unbox  = TRUE,
  na          = "null"
)

message(
  "index_with_operator_info.json written: ", nrow(index_with_operator), " rows\n",
  "Output: ", normalizePath(output_path)
)
