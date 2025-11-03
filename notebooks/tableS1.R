pedigree <-
  read.csv(
    file.path(
      data_path,
      "wytham_ringing_breeding",
      "GTIT_pedigree_full.csv"),
    header=T) %>% 
  rename(bto_ring=id) %>% 
  rename(father=sire) %>% 
  rename(mother=dam)

id <- read.table(
  file.path(
    data_path,
    "plink",
    "10K_processed",
    "10K.fam"
  ),
  header = F
) %>%
  as_tibble() %>% 
  rename(bto_ring=V2) %>% 
  select(bto_ring)

id_600K <- read.table(
  file.path(
    data_path,
    "plink",
    "600K_processed",
    "600K.fam"
  ),
  header = F
) %>%
  as_tibble() %>% 
  rename(bto_ring=V2) %>% 
  select(bto_ring)

# Create combined ID list with all birds from both datasets
id_combined <- bind_rows(id, id_600K) %>% 
  distinct(bto_ring)

imm_res <-
  read.csv(
    file.path(
      data_path,
      "wytham_ringing_breeding",
      "ebmp_database_ringing_record_export_GT&BT_all.csv"
    ),
    header = T
  ) %>% 
  filter(bto_species_code=="GRETI") %>% 
  arrange(age) %>% 
  filter(age>0) %>% 
  distinct(bto_ring,.keep_all=TRUE) %>% 
  mutate(imm_res=ifelse(age==1, "resident", "immigrant")) %>% 
  select(bto_ring, imm_res)

meta <- read.csv(
  file.path(
    data_path,
    "wytham_ringing_breeding",
    "ebmp_database_ringing_record_export_GT&BT_all.csv"
  ),
  header = TRUE
) %>% 
  filter(bto_species_code == "GRETI") %>% 
  arrange(age) %>% 
  filter(age > 0) %>% 
  distinct(bto_ring, .keep_all = TRUE) %>% 
  select(bto_ring, yr, nb) %>% 
  mutate(
    dataset = case_when(
      bto_ring %in% id_600K$bto_ring & bto_ring %in% id$bto_ring ~ "10K + 600K",
      bto_ring %in% id_600K$bto_ring ~ "600K",
      TRUE ~ "10K"
    )
  )

# DIAGNOSTIC CHECKS
cat("=== DIAGNOSTIC CHECKS ===\n")
cat("Length of id (10K):", nrow(id), "\n")
cat("Length of id_600K:", nrow(id_600K), "\n")
cat("Length of id_combined:", nrow(id_combined), "\n")
cat("Length of meta:", nrow(meta), "\n")

# Check overlap between datasets
cat("\n=== DATASET OVERLAPS ===\n")
both_datasets <- intersect(id$bto_ring, id_600K$bto_ring)
cat("Birds in both 10K and 600K:", length(both_datasets), "\n")
cat("Birds only in 10K:", sum(id$bto_ring %in% setdiff(id$bto_ring, id_600K$bto_ring)), "\n")
cat("Birds only in 600K:", sum(id_600K$bto_ring %in% setdiff(id_600K$bto_ring, id$bto_ring)), "\n")

# Check which 600K birds are missing from meta
missing_from_meta <- setdiff(id_600K$bto_ring, meta$bto_ring)
cat("\n=== MISSING DATA CHECKS ===\n")
cat("600K birds missing from meta:", length(missing_from_meta), "\n")
cat("10K birds missing from meta:", length(setdiff(id$bto_ring, meta$bto_ring)), "\n")

if(length(missing_from_meta) > 0) {
  cat("Examples of 600K birds missing from meta:\n")
  print(head(missing_from_meta, 10))
}

# Check dataset assignments in meta
cat("\n=== DATASET ASSIGNMENTS IN META ===\n")
dataset_counts <- table(meta$dataset)
print(dataset_counts)
cat("Total 600K birds in meta (600K + 10K + 600K):", sum(dataset_counts[c("600K", "10K + 600K")]), "\n")

# Create final dataset with all birds, including those missing from meta
final_table <- id_combined %>% 
  left_join(meta, by = "bto_ring") %>% 
  left_join(pedigree, by = "bto_ring") %>% 
  left_join(imm_res, by = "bto_ring") %>% 
  mutate(
    # Fix dataset assignment for birds not in meta
    dataset = case_when(
      is.na(dataset) & bto_ring %in% id_600K$bto_ring & bto_ring %in% id$bto_ring ~ "10K + 600K",
      is.na(dataset) & bto_ring %in% id_600K$bto_ring ~ "600K",
      is.na(dataset) & bto_ring %in% id$bto_ring ~ "10K",
      TRUE ~ dataset
    )
  )

cat("\n=== FINAL TABLE DATASET COUNTS ===\n")
final_dataset_counts <- table(final_table$dataset, useNA = "ifany")
print(final_dataset_counts)
cat("Total 600K birds in final table:", sum(final_dataset_counts[c("600K", "10K + 600K")]), "\n")

write.csv(final_table, "tableS1.csv", row.names = F)