library (dplyr)
library(stringr)
library(stringi)
#Read in the raw GBIF download. It is best to do this separately for each state to avoid key-words matching in the wrong state (i.e., 'Chiricahua' should not match with specimens in New Mexico)
az_specimens_nongeoreferenced <- read.csv("arizona_specimens_nongeoreferenced.csv")

#remove any records that lack a locality
specimens_with_locality <- az_specimens_nongeoreferenced %>%
  filter(!is.na(locality) & locality !="")

#Restore records that have a species ID in the verbatimScientificName column but not the species column (generally when the verbatimScientificName is in ALL CAPS)
specimens_with_locality_automated_id <- specimens_with_locality %>%
  mutate(
    vscn_trim = str_trim(verbatimScientificName),
    # Match ALL-CAPS names: GENUS SPECIES (SUBSPECIES, if present)
    m = str_match(vscn_trim, "^([A-Z][A-Z\\-]+)\\s+([A-Z][A-Z\\-]+)(?:\\s+([A-Z][A-Z\\-]+))?$"),
    cond = taxonRank == "GENUS" & !is.na(m[,1]),
    # Build formatted names 
    genus_from_verbatim  = str_to_title(str_to_lower(m[,2])),
    species_from_verbatim = if_else(cond,
                                    str_to_sentence(paste(str_to_lower(m[,2]), str_to_lower(m[,3]))),
                                    NA_character_),
    infra_from_verbatim   = if_else(cond, str_to_lower(m[,4]), NA_character_),
    # Update species/subspecies columns
    species = if_else(cond & (is.na(species) | str_trim(species) == ""),
                      species_from_verbatim,
                      species),
    infraspecificEpithet = if_else(cond & (is.na(infraspecificEpithet) | str_trim(infraspecificEpithet) == ""),
                                   infra_from_verbatim,
                                   infraspecificEpithet)
  ) %>%
  select(-vscn_trim, -m, -genus_from_verbatim, -species_from_verbatim, -infra_from_verbatim, -cond)

# Count how many records were updated
changed_rows <- with(
  list(old = specimens_with_locality, new = specimens_with_locality_automated_id),
  which(
    (is.na(old$species) & !is.na(new$species)) |
      (str_trim(old$species) == "" & str_trim(new$species) != "")
  )
)
cat("Rows updated:", length(changed_rows), "\n")

#Extract any specimens that have both a taxonRank of "GENUS" and nothing in the species column and put in a new data frame
genus_only_id <- specimens_with_locality_automated_id %>%
  filter(taxonRank == "GENUS" & (is.na(species) | species == ""))

#Export the extracted specimens to manually restore species IDs. The automated version only finds verbatimScientificNames in all caps, but misses fringe cases where only part of the field is in caps and other oddities.
write.csv(genus_only_id, "genus_only_id.csv", row.names = FALSE)

#Remove the specimens that have both a taxonRank of "GENUS" and nothing in the species column from the original data frame
specimens_with_locality_automated_id_no_genus_only <- specimens_with_locality_automated_id %>%
  filter(!(taxonRank == "GENUS" & (is.na(species) | species == "")))

#Import the manually restored version of the specimens.
manual_id_restore <- read.csv("manual_id_restore.csv")

#Combine the manually restored specimens with the rest of the specimens.
specimens_with_locality_restored_id <- bind_rows(manual_id_restore, specimens_with_locality_automated_id_no_genus_only)

#Filter out any specimens that remain without a full specific ID. The resulting data set includes only specimens that have a locality and specific ID.
specimens_with_locality_complete_id <- specimens_with_locality_restored_id %>%
  filter(str_count(str_trim(species), "\\S+") > 1)

#The next section uses key-words to search the locality field for specimens that are likely found in the Madrean Sky Islands.

# Normalize the text in the locality field (all lowercase, removes symbols, etc)
normalize_text <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%  
    str_to_lower() %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}
specimen_norm <- specimens_with_locality_complete_id %>%
  mutate(locality_norm = normalize_text(locality))
#Mountain names as the main key-words
key_words <- c(
  "superstition", "pinal", "teresa", "catalina", "galiuro", "pinaleno",
  "peloncillo", "rincon", "winchester", "dos cabezas", "baboquivari",
  "sierrita", "rita", "whetstone", "dragoon", "swisshelm", "chiricahua",
  "atascosa", "patagonia", "canelo", "huachuca", "mule", "pajarito"
)

#Potential variants includes the plural form of the mountain name and two special cases
variants <- c(
  # plurals
  paste0(key_words[!str_detect(key_words, "\\s")], "s"),
  #Special cases
  "portal", "graham"
)
all_terms <- unique(c(key_words, variants))

#Build regex pattern
final_pat <- paste0("\\b(", paste(all_terms, collapse = "|"), ")\\b")

#Filter specimens based on locality
specimen_matched <- specimen_norm %>%
  mutate(matched_term = str_extract(locality_norm, regex(final_pat, ignore_case = TRUE)))

sky_island_specimens_nongeoreferenced <- specimen_matched %>%
  filter(!is.na(matched_term))

#Count how many records matched each key-word
mountain_counts <- sky_island_specimens_nongeoreferenced %>%
  count(matched_term, sort = TRUE)

#Export the data set to manually inspect the localities. In some cases, localities match the key words but are obviously not in the right area (e.g., "Chiricahua Ranch, San Carlos Indian Reservation" is not in the Chiricahua Mountains)
write.csv(sky_island_specimens_nongeoreferenced, "sky_island_specimens_nongeoreferenced.csv", row.names = FALSE)
