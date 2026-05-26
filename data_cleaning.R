# ============================================================
#  DATA CLEANING — DatasetRMS_studenten
#  Steps: 
# 1) Exclude participants (consent + missing data)
# 2) Check if everything is right  
# 3) Create scores 
# 4) Save the cleaned data frame 
# ============================================================

# ── Required packages ────────────────────────────────────────
# If not yet installed, run first:
# install.packages(c("haven", "dplyr", "tidyr"))

library(haven)   # reads .sav files from SPSS
library(dplyr)   # data manipulation
library(tidyr)   # missing data handling


# ============================================================
# 0. LOAD THE FILE
# ============================================================
# let's read the file from SPSS
df <- read_sav("DatasetRMS_studenten7.sav")

# Let's take a look at it
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")
glimpse(df)
#View(df)

# ============================================================
# 1. EXCLUDE PARTICIPANTS
# ============================================================

# --- 1a. Remove participants who did NOT give consent (Consent_1 != 1) ---
# Consent_1 = 1 means "Ja" (yes); 2 = "Nee" (no); NA = no response
# We first create a clean data frame: df_clean 
df_clean <- df %>%
  filter(Consent_1 == 1) # code equivalent to df_clean <- filter(df, Consent_1 == 1)


cat("\nParticipants removed due to missing consent:",
    nrow(df) - nrow(df_clean), "\n")

# --- 1b. Retain only participants with complete data (Group 1) ---
# Inspection of the dataset revealed three distinct groups based on missingness:

sum(complete.cases(df_clean)) # only 18 complete cases 
sum(complete.cases(df_clean$posttest1))
sum(complete.cases(df_clean$posttest2)) # the pattern continues, only 18 complete cases

# This missing pattern is consistent with the study design (not all participants
# could attend all measurement moments). However, all RQ require posttest data
#   - RQ1: MR: posttest ~ retrieval + spaced + interleaved
#   - RQ2: correlation between pretest <> posttest
#   - RQ3: Interaction: posttest ~ pretest x study strategies 

# We need to run another step to remove NA (not available): 
df_clean <- df_clean %>%
  drop_na() 
# this is equivalent to df_clean <- drop_na(df_clean)

cat("Participants retained after exclusions (Group 1 only):", nrow(df_clean), "\n")


# ============================================================
# 2. VERIFY REMAINING DATA QUALITY
# ============================================================

# 2a. Now we confirm that there are no missing: 
cat("\nMissing values remaining:", sum(is.na(df_clean)), "\n")

# 2b. Confirm no duplicate rows:
cat("Duplicate rows:", sum(duplicated(df_clean)), "\n")

# 2c. Confirm values are within expected ranges:

# Pretest/posttest items: should only contain 0 or 1
cat("Pretest/posttest out of range (expected 0 or 1):\n")
cat("pretest1:",  sum(!df_clean$pretest1  %in% c(0, 1)), "\n") 
# TRUE (=1) means that there are values different than 0 or 1 
# False (=0) means that there are only values 0 or 1
cat("pretest2:",  sum(!df_clean$pretest2  %in% c(0, 1)), "\n")
cat("pretest3:",  sum(!df_clean$pretest3  %in% c(0, 1)), "\n")
cat("pretest4:",  sum(!df_clean$pretest4  %in% c(0, 1)), "\n")
cat("pretest5:",  sum(!df_clean$pretest5  %in% c(0, 1)), "\n")
cat("pretest6:",  sum(!df_clean$pretest6  %in% c(0, 1)), "\n")
cat("pretest7:",  sum(!df_clean$pretest7  %in% c(0, 1)), "\n")
cat("pretest8:",  sum(!df_clean$pretest8  %in% c(0, 1)), "\n")
cat("posttest1:", sum(!df_clean$posttest1 %in% c(0, 1)), "\n")
cat("posttest2:", sum(!df_clean$posttest2 %in% c(0, 1)), "\n")
cat("posttest3:", sum(!df_clean$posttest3 %in% c(0, 1)), "\n")
cat("posttest4:", sum(!df_clean$posttest4 %in% c(0, 1)), "\n")
cat("posttest5:", sum(!df_clean$posttest5 %in% c(0, 1)), "\n")
cat("posttest6:", sum(!df_clean$posttest6 %in% c(0, 1)), "\n")
cat("posttest7:", sum(!df_clean$posttest7 %in% c(0, 1)), "\n")
cat("posttest8:", sum(!df_clean$posttest8 %in% c(0, 1)), "\n")

# Similarly, study strategy should only be 1, 2, or 3
cat("\nStrategy scales out of range (expected 1-3):\n")
cat("retrieval1:",   sum(!df_clean$retrieval1   %in% c(1, 2, 3)), "\n")
cat("retrieval2:",   sum(!df_clean$retrieval2   %in% c(1, 2, 3)), "\n")
cat("retrieval3:",   sum(!df_clean$retrieval3   %in% c(1, 2, 3)), "\n")
cat("retrieval4:",   sum(!df_clean$retrieval4   %in% c(1, 2, 3)), "\n")
cat("retrieval5:",   sum(!df_clean$retrieval5   %in% c(1, 2, 3)), "\n")
cat("retrieval6:",   sum(!df_clean$retrieval6   %in% c(1, 2, 3)), "\n")
cat("retrieval7:",   sum(!df_clean$retrieval7   %in% c(1, 2, 3)), "\n")
cat("retrieval8:",   sum(!df_clean$retrieval8   %in% c(1, 2, 3)), "\n")
cat("retrieval9:",   sum(!df_clean$retrieval9   %in% c(1, 2, 3)), "\n")
cat("retrieval10:",  sum(!df_clean$retrieval10  %in% c(1, 2, 3)), "\n")
cat("retrieval11:",  sum(!df_clean$retrieval11  %in% c(1, 2, 3)), "\n")
cat("retrieval12:",  sum(!df_clean$retrieval12  %in% c(1, 2, 3)), "\n")
cat("Spaced1:",      sum(!df_clean$Spaced1      %in% c(1, 2, 3)), "\n")
cat("Spaced2:",      sum(!df_clean$Spaced2      %in% c(1, 2, 3)), "\n")
cat("Interleaved1:", sum(!df_clean$Interleaved1 %in% c(1, 2, 3)), "\n")
cat("Interleaved2:", sum(!df_clean$Interleaved2 %in% c(1, 2, 3)), "\n")

# ============================================================
# 3. CREATE SCORES 
# ============================================================

# 3a. Remove SPSS labels
df_clean <- df_clean %>%
  mutate(across(everything(), as.numeric)) 
# equivalent to df_clean <- as.data.frame(lapply(df_clean, as.numeric))

# 3b. Computing total scores 
# Now, an important part, we can calculate the scores 
# Pretest: correct answer = 1 across all items (equal weight, possible range 0 to 8)
# Posttest: weighted score (see Posttest Scoring table):
#   - No belief-bias items (posttest1, 5, 6, 7) = 6 points each  -> max 24
#   - Belief-bias items    (posttest2, 3, 4, 8) = 8 points each  -> max 32
#   - Total maximum posttest score = 56 (range 0 to 56)
df_clean <- df_clean %>%
  mutate(
    total_pretest  = rowSums(select(., starts_with("pretest")), na.rm = TRUE),
    total_posttest = (posttest1 + posttest5 + posttest6 + posttest7) * 6 +
                     (posttest2 + posttest3 + posttest4 + posttest8) * 8
  )

# --- 3c. Compute composite study strategy scores ---
# Spaced practice: mean of Spaced1 and Spaced2 (scale 1-3, max = 3)
# Interleaved practice: mean of Interleaved1 and Interleaved2 (scale 1-3, max = 3)
# Retrieval practice: mean of retrieval1-12 (scale 1-3, max = 3)
df_clean <- df_clean %>%
  mutate(
    score_spaced      = (Spaced1 + Spaced2) / 2,
    score_interleaved = (Interleaved1 + Interleaved2) / 2,
    score_retrieval = (retrieval1 + retrieval2 + retrieval3 + retrieval4 +
                         retrieval5 + retrieval6 + retrieval7 + retrieval8 +
                         retrieval9 + retrieval10 + retrieval11 + retrieval12) / 12
  )

# ============================================================
# 4. FINAL CHECK
# ============================================================
cat("\n========== CLEANING SUMMARY ==========\n")
cat("Participants in original dataset:  ", nrow(df), "\n")
cat("Participants after cleaning:       ", nrow(df_clean), "\n")
cat("Columns:                           ", ncol(df_clean), "\n")
cat("Remaining missing values (total):  ", sum(is.na(df_clean)), "\n")

# Preview key variables
df_clean %>%
  select(ParticipantID, total_pretest, total_posttest,
         score_spaced, score_interleaved, score_retrieval) %>%
  print()


# ============================================================
# 5. SAVE THE CLEAN DATASET
# ============================================================

# Option A: save as .sav (can also be opened in SPSS)
# haven::write_sav(df_clean, "dataset_clean.sav")

# Option B: save as .csv (can be opened in any program)
write.csv(df_clean, "dataset_clean.csv", row.names = FALSE)

cat("\nFile saved as 'dataset_clean.csv'!\n")



